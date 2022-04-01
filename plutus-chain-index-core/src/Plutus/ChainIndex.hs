{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Plutus.ChainIndex(
    runChainIndexEffects
    , handleChainIndexEffects
    , RunRequirements(..)
    , module Export
    ) where

import Cardano.BM.Trace (Trace)
import Control.Concurrent.STM (TVar, atomically, readTVarIO, writeTVar)
import Control.Monad.Freer (Eff, LastMember, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error, handleError, runError, throwError)
import Control.Monad.Freer.Extras.Beam (BeamEffect, handleBeam)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.Freer.Extras.Modify (raiseEnd, raiseMUnderN)
import Control.Monad.Freer.Extras.Pagination as Export
import Control.Monad.Freer.Reader (Reader, runReader)
import Control.Monad.Freer.State (State, runState)
import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool)
import Database.SQLite.Simple qualified as Sqlite
import Ouroboros.Consensus.Config (SecurityParam (SecurityParam))
import Plutus.ChainIndex.ChainIndexError as Export
import Plutus.ChainIndex.ChainIndexLog as Export
import Plutus.ChainIndex.Effects as Export
import Plutus.ChainIndex.Indexer.Sqlite.DbSchema (chainIndexDb)
import Plutus.ChainIndex.Indexer.Sqlite.Handlers as Export
import Plutus.ChainIndex.Tx as Export
import Plutus.ChainIndex.TxIdState as Export hiding (fromBlock, fromTx, rollback)
import Plutus.ChainIndex.TxOutBalance as Export hiding (fromBlock, fromTx, isSpentOutput, isUnspentOutput, rollback)
import Plutus.ChainIndex.Types as Export
import Plutus.ChainIndex.UtxoState as Export
import Plutus.Monitoring.Util (PrettyObject (PrettyObject), convertLog, runLogEffects)

-- | The required arguments to run the chain index effects.
data RunRequirements = RunRequirements
    { trace         :: Trace IO (PrettyObject ChainIndexLog)
    , stateTVar     :: TVar ChainIndexState
    , pool          :: Pool Sqlite.Connection
    , securityParam :: SecurityParam
    }

-- | Run the chain index effects from the set of all effects using the SQLite
-- store.
runChainIndexEffects
    :: RunRequirements
    -> Eff '[ChainIndexQueryEffect, ChainIndexControlEffect, BeamEffect] a
    -> IO (Either ChainIndexError a)
runChainIndexEffects runReq action =
    runLogEffects (convertLog PrettyObject $ trace runReq)
        $ handleChainIndexEffects runReq
        $ raiseEnd action

-- | Handle the chain index effects from the set of all effects using the
-- SQLite store.
handleChainIndexEffects
    :: forall effs a. (LastMember IO effs, Member (LogMsg ChainIndexLog) effs)
    => RunRequirements
    -> Eff (ChainIndexQueryEffect ': ChainIndexControlEffect ': BeamEffect ': effs) a
    -> Eff effs (Either ChainIndexError a)
handleChainIndexEffects
        RunRequirements { trace, stateTVar, pool, securityParam = SecurityParam d }
        action = do
    state <- liftIO $ readTVarIO stateTVar
    (result, newState) <-
        runState state
        $ runReader pool
        $ runReader (Depth $ fromIntegral d)
        $ runReader chainIndexDb
        $ runError @ChainIndexError
        $ flip handleError (throwError . BeamEffectError)
        $ interpret (handleBeam (convertLog (PrettyObject . BeamLogItem) trace))

        -- All indexers
        $ handleChainIndexControl

        $ interpret handleQuery

        -- Insert the 6 effects needed by the handlers of the 3 chain index effects between those 3 effects and 'effs'.
        $ raiseMUnderN @[_,_,_,_,_,_] @[_,_,_] action
    liftIO $ atomically $ writeTVar stateTVar newState
    pure result

-- | Handle the chain index control effects from the set of all effects using the
-- sqlite store.
handleChainIndexControl :: forall effs.
    ( Member BeamEffect effs
    , Member (State ChainIndexState) effs
    , Member (Reader Depth) effs
    , Member (LogMsg ChainIndexLog) effs
    , Member (Error ChainIndexError) effs
    )
    => Eff (ChainIndexControlEffect ': effs) ~> Eff effs
handleChainIndexControl =
    interpret
        (\eff ->
            handleUtxoIndexer chainIndexDb eff
         >> handleDatumFromHashIndexer chainIndexDb eff
         >> handleRedeemerFromHashIndexer chainIndexDb eff
         >> handleScriptFromHashIndexer chainIndexDb eff
         >> handleUtxoFromAddressIndexer chainIndexDb eff
         >> handleAssetClassFromAddressIndexer chainIndexDb eff
        )
