{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}
{-| Using the chain index as a library.

A minimal example that just syncs the chain index:

@
`withDefaultRunRequirements` $ \runReq -> do

    syncHandler <- `defaultChainSyncHandler` runReq

    `syncChainIndex` `defaultConfig` runReq syncHandler

    void getLine
@

-}
module Plutus.ChainIndex.Lib (
    RunRequirements(..)
    , withRunRequirements
    , withDefaultRunRequirements
    , defaultLoggingConfig
    , Config.defaultConfig
    -- * Chain index effects
    , CI.handleChainIndexEffects
    , runChainIndexEffects
    -- * Chain synchronisation
    , syncChainIndex
    -- ** Synchronisation handlers
    , ChainSyncHandler
    , ChainSyncEvent(..)
    , EventsQueue
    , storeChainSyncHandler
    , storeFromBlockNo
    , filterTxs
    , runChainIndexDuringSync
    -- * Utils
    , getTipSlot
) where

import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Extras.Beam (BeamLog (SqlLog))
import Control.Monad.Freer.Extras.Beam.Effects (BeamEffect)
import Control.Monad.Freer.Extras.Log qualified as Log
import Data.Default (def)
import Data.Functor (void)
import Data.Pool qualified as Pool
import Database.Beam.Migrate.Simple (autoMigrate)
import Database.Beam.Sqlite qualified as Sqlite
import Database.Beam.Sqlite.Migrate qualified as Sqlite
import Database.SQLite.Simple qualified as Sqlite

import Cardano.Api qualified as C
import Cardano.BM.Configuration.Model qualified as CM
import Cardano.BM.Setup (setupTrace_)
import Cardano.BM.Trace (Trace, logDebug, nullTracer)

import Cardano.Node.Socket.Emulator.Types (epochSlots)
import Cardano.Protocol.Socket.Client qualified as C
import Control.Concurrent.STM (atomically, newTVarIO)
import Control.Concurrent.STM.TBMQueue (TBMQueue, writeTBMQueue)
import Database.Beam.Sqlite (Sqlite)
import Plutus.ChainIndex (ChainIndexLog (BeamLogItem), RunRequirements (RunRequirements), getResumePoints,
                          runChainIndexEffects, tipBlockNo)
import Plutus.ChainIndex qualified as CI
import Plutus.ChainIndex.Compatibility (fromCardanoBlock, fromCardanoPoint, fromCardanoTip, tipFromCardanoBlock)
import Plutus.ChainIndex.Config qualified as Config
import Plutus.ChainIndex.DbSchema (checkedSqliteDb)
import Plutus.ChainIndex.Effects (ChainIndexControlEffect, ChainIndexQueryEffect)
import Plutus.ChainIndex.Logging qualified as Logging
import Plutus.Monitoring.Util (PrettyObject (PrettyObject), convertLog, runLogEffects)

-- | Generate the requirements to run the chain index effects given logging configuration and chain index configuration.
withRunRequirements :: CM.Configuration -> Config.ChainIndexConfig -> (RunRequirements -> IO ()) -> IO ()
withRunRequirements logConfig config cont = do
  pool <- Pool.newPool Pool.PoolConfig
    { Pool.createResource = Sqlite.open (Config.cicDbPath config) >>= setupConn
    , Pool.freeResource = Sqlite.close
    , Pool.poolCacheTTL = 1_000_000
    , Pool.poolMaxResources = 25
    }
  (trace :: Trace IO (PrettyObject ChainIndexLog), _) <- setupTrace_ logConfig "chain-index"
  Pool.withResource pool $ \conn -> do
    Sqlite.runBeamSqliteDebug (logDebug (convertLog PrettyObject trace) . (BeamLogItem . SqlLog)) conn $ do
        autoMigrate Sqlite.migrationBackend checkedSqliteDb

    -- Automatically delete the input when an output from a matching input/output pair is deleted.
    -- See reduceOldUtxoDb in Plutus.ChainIndex.Handlers
    Sqlite.execute_ conn "DROP TRIGGER IF EXISTS delete_matching_input"
    Sqlite.execute_ conn
        "CREATE TRIGGER delete_matching_input AFTER DELETE ON unspent_outputs \
        \BEGIN \
        \  DELETE FROM unmatched_inputs WHERE input_row_tip__row_slot = old.output_row_tip__row_slot \
        \                                 AND input_row_out_ref = old.output_row_out_ref; \
        \END"

    -- creating extra index to optimize utxoSetAtAddress and utxoSetWithCurrency queries
    Sqlite.execute_ conn "DROP INDEX IF EXISTS unspent_index"
    Sqlite.execute_ conn "DROP INDEX IF EXISTS unmatched_index"
    Sqlite.execute_ conn "CREATE INDEX IF NOT EXISTS unspent_index on unspent_outputs (output_row_out_ref, output_row_tip__row_slot)"
    Sqlite.execute_ conn "CREATE INDEX IF NOT EXISTS unmatched_index on unmatched_inputs (input_row_out_ref, input_row_tip__row_slot)"

  stateTVar <- newTVarIO mempty
  cont $ RunRequirements trace stateTVar pool (Config.cicSecurityParam config)

  where
    setupConn conn = do
        -- Optimize Sqlite for write performance, halves the sync time.
        -- https://sqlite.org/wal.html
        Sqlite.execute_ conn "PRAGMA journal_mode=WAL;"
        return conn

-- | Generate the requirements to run the chain index effects given default configurations.
withDefaultRunRequirements :: (RunRequirements -> IO ()) -> IO ()
withDefaultRunRequirements cont = do
    logConfig <- Logging.defaultConfig
    withRunRequirements logConfig Config.defaultConfig cont

-- | The default logging configuration.
defaultLoggingConfig :: IO CM.Configuration
defaultLoggingConfig = Logging.defaultConfig

-- | Chain synchronisation events.
data ChainSyncEvent
    = Resume       CI.Point
    -- ^ Resume from the given point
    | RollForward  CI.ChainSyncBlock CI.Tip
    -- ^ Append the given block. The tip is the current tip of the node, which is newer than the tip of the block during syncing.
    | RollBackward CI.Point CI.Tip
    -- ^ Roll back to the given point. The tip is current tip of the node.
    deriving (Show)

toCardanoChainSyncHandler :: ChainSyncHandler -> C.ChainSyncEvent -> IO ()
toCardanoChainSyncHandler handler = \case
    C.RollBackward cp ct -> handler (RollBackward (fromCardanoPoint cp) (fromCardanoTip ct))
    C.Resume cp -> handler (Resume (fromCardanoPoint cp))
    C.RollForward block ct ->
        let txs = fromCardanoBlock block
        in handler (RollForward (CI.Block (tipFromCardanoBlock block) (map (, def) txs)) (fromCardanoTip ct))

-- | A handler for chain synchronisation events.
type ChainSyncHandler = ChainSyncEvent -> IO ()
type EventsQueue = TBMQueue ChainSyncEvent

storeChainSyncHandler :: EventsQueue -> ChainSyncHandler
storeChainSyncHandler eventsQueue = atomically . writeTBMQueue eventsQueue

-- | Changes the given @ChainSyncHandler@ to only store transactions with a block number no smaller than the given one.
storeFromBlockNo :: CI.BlockNumber -> ChainSyncHandler -> ChainSyncHandler
storeFromBlockNo storeFrom handler (RollForward (CI.Block blockTip txs) chainTip) =
    handler (RollForward (CI.Block blockTip (map (\(tx, opt) -> (tx, opt { CI.tpoStoreTx = CI.tpoStoreTx opt && store })) txs)) chainTip)
        where store = tipBlockNo blockTip >= storeFrom
storeFromBlockNo _ handler evt = handler evt

-- | Changes the given @ChainSyncHandler@ to only process and store certain transactions.
filterTxs
    :: (CI.ChainIndexTx -> Bool)
    -- ^ Only process transactions for which this function returns @True@.
    -> (CI.ChainIndexTx -> Bool)
    -- ^ From those, only store transactions for which this function returns @True@.
    -> ChainSyncHandler
    -- ^ The @ChainSyncHandler@ on which the returned @ChainSyncHandler@ is based.
    -> ChainSyncHandler
filterTxs isAccepted isStored handler (RollForward (CI.Block blockTip txs) chainTip) =
    let txs' = map (\(tx, opt) -> (tx, opt { CI.tpoStoreTx = CI.tpoStoreTx opt && isStored tx }))
                $ filter (isAccepted . fst) txs
    in handler (RollForward (CI.Block blockTip txs') chainTip)
filterTxs _ _ handler evt = handler evt

-- | Get the slot number of the current tip of the node.
getTipSlot :: Config.ChainIndexConfig -> IO (Maybe C.SlotNo)
getTipSlot config = do
  tip <- C.getLocalChainTip $ C.LocalNodeConnectInfo
         { C.localConsensusModeParams = C.CardanoModeParams epochSlots
         , C.localNodeNetworkId = Config.cicNetworkId config
         , C.localNodeSocketPath = Config.cicSocketPath config
         }
  case tip of
    C.ChainTip slotNo _ _ -> pure $ Just slotNo
    C.ChainTipAtGenesis   -> pure $ Nothing


-- | Synchronise the chain index with the node using the given handler.
syncChainIndex :: Config.ChainIndexConfig -> RunRequirements -> ChainSyncHandler -> IO ()
syncChainIndex config runReq syncHandler = do
    Just resumePoints <- runChainIndexDuringSync runReq getResumePoints
    void $ C.runChainSync
        (Config.cicSocketPath config)
        nullTracer
        (Config.cicSlotConfig config)
        (Config.cicNetworkId  config)
        resumePoints
        (toCardanoChainSyncHandler syncHandler)

runChainIndexDuringSync
  :: RunRequirements
  -> Eff '[ChainIndexQueryEffect, ChainIndexControlEffect, BeamEffect Sqlite] a
  -> IO (Maybe a)
runChainIndexDuringSync runReq effect = do
    errOrResult <- runChainIndexEffects runReq effect
    case errOrResult of
        Left err -> do
            runLogEffects (convertLog PrettyObject $ CI.trace runReq) $ Log.logError $ CI.Err err
            pure Nothing
        Right result -> do
            pure (Just result)
