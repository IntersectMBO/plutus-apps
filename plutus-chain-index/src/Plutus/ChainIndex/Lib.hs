{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Using the chain index as a library.

A minimal example that just syncs the chain index:

@
`withDefaultRunRequirements` $ \runReq -> do

    syncHandler <- `showingProgress` $ `defaultChainSynHandler` runReq

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
    , defaultChainSynHandler
    , storeFromBlockNo
    , showingProgress
    -- * Utils
    , getTipSlot
) where

import Control.Concurrent.STM qualified as STM
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Extras.Beam (BeamEffect, BeamLog (SqlLog))
import Control.Monad.Freer.Extras.Log qualified as Log
import Data.Functor (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Database.Beam.Migrate.Simple (autoMigrate)
import Database.Beam.Sqlite qualified as Sqlite
import Database.Beam.Sqlite.Migrate qualified as Sqlite
import Database.SQLite.Simple qualified as Sqlite

import Cardano.Api qualified as C
import Cardano.BM.Configuration.Model qualified as CM
import Cardano.BM.Setup (setupTrace_)
import Cardano.BM.Trace (Trace, logDebug, logError, nullTracer)

import Cardano.Protocol.Socket.Client (ChainSyncEvent (..), runChainSync)
import Cardano.Protocol.Socket.Type (epochSlots)
import Plutus.ChainIndex (ChainIndexLog (BeamLogItem), RunRequirements (RunRequirements), getResumePoints,
                          runChainIndexEffects)
import Plutus.ChainIndex qualified as CI
import Plutus.ChainIndex.Compatibility (fromCardanoBlock, fromCardanoPoint, tipFromCardanoBlock)
import Plutus.ChainIndex.Config qualified as Config
import Plutus.ChainIndex.DbSchema (checkedSqliteDb)
import Plutus.ChainIndex.Effects (ChainIndexControlEffect (..), ChainIndexQueryEffect (..), appendBlock, resumeSync,
                                  rollback)
import Plutus.ChainIndex.Logging qualified as Logging
import Plutus.Monitoring.Util (runLogEffects)

-- | Generate the requirements to run the chain index effects given logging configuration and chain index configuration.
withRunRequirements :: CM.Configuration -> Config.ChainIndexConfig -> (RunRequirements -> IO ()) -> IO ()
withRunRequirements logConfig config cont = do
  Sqlite.withConnection (Config.cicDbPath config) $ \conn -> do

    (trace :: Trace IO ChainIndexLog, _) <- setupTrace_ logConfig "chain-index"

    -- Optimize Sqlite for write performance, halves the sync time.
    -- https://sqlite.org/wal.html
    Sqlite.execute_ conn "PRAGMA journal_mode=WAL"
    Sqlite.runBeamSqliteDebug (logDebug trace . (BeamLogItem . SqlLog)) conn $ do
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

    stateTVar <- STM.newTVarIO mempty
    cont $ RunRequirements trace stateTVar conn (Config.cicSecurityParam config)

-- | Generate the requirements to run the chain index effects given default configurations.
withDefaultRunRequirements :: (RunRequirements -> IO ()) -> IO ()
withDefaultRunRequirements cont = do
    logConfig <- Logging.defaultConfig
    withRunRequirements logConfig Config.defaultConfig cont

-- | The default logging configuration.
defaultLoggingConfig :: IO CM.Configuration
defaultLoggingConfig = Logging.defaultConfig

-- | A handler for chain synchronisation events.
type ChainSyncHandler = ChainSyncEvent -> IO ()

-- | The default chain synchronisation event handler. Updates the in-memory state and the database.
defaultChainSynHandler :: RunRequirements -> ChainSyncHandler
defaultChainSynHandler runReq
    (RollForward block _ opt) = do
        let ciBlock = fromCardanoBlock block
        case ciBlock of
            Left err    ->
                logError (CI.trace runReq) (CI.ConversionFailed err)
            Right txs -> void $ runChainIndexDuringSync runReq $
                appendBlock (tipFromCardanoBlock block) txs opt
defaultChainSynHandler runReq
    (RollBackward point _) = do
        void $ runChainIndexDuringSync runReq $ rollback (fromCardanoPoint point)
defaultChainSynHandler runReq
    (Resume point) = do
        void $ runChainIndexDuringSync runReq $ resumeSync $ fromCardanoPoint point

-- | Changes the given @ChainSyncHandler@ to only store transactions with a block number no smaller than the given one.
storeFromBlockNo :: C.BlockNo -> ChainSyncHandler -> ChainSyncHandler
storeFromBlockNo storeFrom handler (RollForward block@(C.BlockInMode (C.Block (C.BlockHeader _ _ blockNo) _) _) tip opt) =
    handler (RollForward block tip opt{ CI.bpoStoreTxs = blockNo >= storeFrom })
storeFromBlockNo _ handler evt = handler evt

-- | Get the slot number of the current tip of the node.
getTipSlot :: Config.ChainIndexConfig -> IO C.SlotNo
getTipSlot config = do
  C.ChainTip slotNo _ _ <- C.getLocalChainTip $ C.LocalNodeConnectInfo
    { C.localConsensusModeParams = C.CardanoModeParams epochSlots
    , C.localNodeNetworkId = Config.cicNetworkId config
    , C.localNodeSocketPath = Config.cicSocketPath config
    }
  pure slotNo

showProgress :: IORef Integer -> C.SlotNo -> C.SlotNo -> IO ()
showProgress lastProgressRef (C.SlotNo tipSlot) (C.SlotNo blockSlot) = do
  lastProgress <- readIORef lastProgressRef
  let pct = (100 * fromIntegral blockSlot) `div` fromIntegral tipSlot
  if pct > lastProgress then do
    putStrLn $ "Syncing (" ++ show pct ++ "%)"
    writeIORef lastProgressRef pct
  else pure ()

-- | Changes the given @ChainSyncHandler@ to print out the synchronisation progress percentages.
showingProgress :: ChainSyncHandler -> IO ChainSyncHandler
showingProgress handler = do
    -- The primary purpose of this query is to get the first response of the node for potential errors before opening the DB and starting the chain index.
    -- See #69.
    lastProgressRef <- newIORef 0
    pure $ \case
        (RollForward block@(C.BlockInMode (C.Block (C.BlockHeader blockSlot _ _) _) _) tip@(C.ChainTip tipSlot _ _) opt) -> do
            showProgress lastProgressRef tipSlot blockSlot
            handler $ RollForward block tip opt
        evt -> handler evt

-- | Synchronise the chain index with the node using the given handler.
syncChainIndex :: Config.ChainIndexConfig -> RunRequirements -> ChainSyncHandler -> IO ()
syncChainIndex config runReq syncHandler = do
    Just resumePoints <- runChainIndexDuringSync runReq getResumePoints
    void $ runChainSync (Config.cicSocketPath config)
                        nullTracer
                        (Config.cicSlotConfig config)
                        (Config.cicNetworkId  config)
                        resumePoints
                        syncHandler


runChainIndexDuringSync
  :: RunRequirements
  -> Eff '[ChainIndexQueryEffect, ChainIndexControlEffect, BeamEffect] a
  -> IO (Maybe a)
runChainIndexDuringSync runReq effect = do
    errOrResult <- runChainIndexEffects runReq effect
    case errOrResult of
        Left err -> do
            runLogEffects (CI.trace runReq) $ Log.logError $ CI.Err err
            pure Nothing
        Right result -> do
            pure (Just result)
