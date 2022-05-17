{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Plutus.PAB.Db.FS
  ( handleContractStore
  , runFSStoreAction
  )
  where

import Plutus.PAB.Db.FS.ContractStore (handleContractStore)

import Cardano.BM.Trace (Trace)
import Control.Monad.Freer (Eff, interpret, reinterpret, runM, subsume)
import Control.Monad.Freer.Delay (DelayEffect, handleDelayEffect)
import Control.Monad.Freer.Error (handleError, runError, throwError)
import Control.Monad.Freer.Extras (LogMsg, mapLog)
import Control.Monad.Freer.Extras.Modify qualified as Modify
import Control.Monad.Freer.Reader (runReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Typeable (Typeable)
import Plutus.PAB.Db.FS.Types (ContractStoreDir)
import Plutus.PAB.Effects.Contract (ContractStore)
import Plutus.PAB.Effects.Contract.Builtin (Builtin, HasDefinitions, SomeBuiltinState)
import Plutus.PAB.Monitoring.Monitoring (handleLogMsgTrace)
import Plutus.PAB.Monitoring.PABLogMsg (PABLogMsg (..), PABMultiAgentMsg (..))
import Plutus.PAB.Types (PABError (..))

runFSStoreAction ::
    forall a b.
    ( ToJSON a
    , FromJSON a
    , HasDefinitions a
    )
    => ContractStoreDir (Builtin a)
    -> Trace IO (PABLogMsg (Builtin a))
    -> Eff '[ContractStore (Builtin a), LogMsg (PABMultiAgentMsg (Builtin a)), DelayEffect, IO] b
    -> IO (Either PABError b)
runFSStoreAction contractStoreDir trace =
    runM
    . runError
    . runReader contractStoreDir
    . flip handleError (throwError . BeamEffectError)
    . subsume @IO
    . handleDelayEffect
    . interpret (handleLogMsgTrace trace)
    . reinterpret (mapLog SMultiAgent)
    . interpret handleContractStore
    . Modify.raiseEnd
