{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Plutus.ChainIndex(
    runChainIndexEffects
    , handleChainIndexEffects
    , RunRequirements(..)
    , module Export
    ) where

import Control.Monad.Freer.Extras.Pagination as Export
import Plutus.ChainIndex.ChainIndexError as Export
import Plutus.ChainIndex.ChainIndexLog as Export
import Plutus.ChainIndex.Effects as Export
import Plutus.ChainIndex.Handlers as Export
import Plutus.ChainIndex.Tx as Export
import Plutus.ChainIndex.TxIdState as Export hiding (fromBlock, fromTx, rollback)
import Plutus.ChainIndex.TxOutBalance as Export hiding (fromBlock, fromTx, isSpentOutput, isUnspentOutput, rollback)
import Plutus.ChainIndex.Types as Export
import Plutus.ChainIndex.UtxoState as Export

import Cardano.BM.Trace (Trace)
import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import Control.Monad.Freer (Eff, LastMember, Member, interpret)
import Control.Monad.Freer.Error (handleError, runError, throwError)
import Control.Monad.Freer.Extras.Beam (BeamEffect, handleBeam)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.Freer.Extras.Modify (raiseEnd, raiseMUnderN)
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.Freer.State (runState)
import Control.Monad.IO.Class (liftIO)
import Database.SQLite.Simple qualified as Sqlite
import Plutus.Monitoring.Util (convertLog, runLogEffects)

-- | The required arguments to run the chain index effects.
data RunRequirements = RunRequirements
    { trace         :: Trace IO ChainIndexLog
    , stateMVar     :: MVar ChainIndexState
    , conn          :: Sqlite.Connection
    , securityParam :: Int
    }

-- | Run the chain index effects.
runChainIndexEffects
    :: RunRequirements
    -> Eff '[ChainIndexQueryEffect, ChainIndexControlEffect, BeamEffect] a
    -> IO (Either ChainIndexError a)
runChainIndexEffects runReq action =
    runLogEffects (trace runReq)
        $ handleChainIndexEffects runReq
        $ raiseEnd action

-- | Handle the chain index effects from the set of all effects.
handleChainIndexEffects
    :: (LastMember IO effs, Member (LogMsg ChainIndexLog) effs)
    => RunRequirements
    -> Eff (ChainIndexQueryEffect ': ChainIndexControlEffect ': BeamEffect ': effs) a
    -> Eff effs (Either ChainIndexError a)
handleChainIndexEffects RunRequirements{trace, stateMVar, conn, securityParam} action = do
    state <- liftIO $ takeMVar stateMVar
    (result, newState) <-
        runState state
        $ runReader conn
        $ runReader (Depth securityParam)
        $ runError @ChainIndexError
        $ flip handleError (throwError . BeamEffectError)
        $ interpret (handleBeam (convertLog BeamLogItem trace))
        $ interpret handleControl
        $ interpret handleQuery
        -- Insert the 5 effects needed by the handlers of the 3 chain index effects between those 3 effects and 'effs'.
        $ raiseMUnderN @[_,_,_,_,_] @[_,_,_] action
    liftIO $ putMVar stateMVar newState
    pure result
