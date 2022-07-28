{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-

Interface to beam ecosystem used by the PAB to store contracts.

-}
module Plutus.PAB.Db.Beam (runBeamStoreAction) where

import Cardano.BM.Trace (Trace)
import Control.Monad.Freer (Eff, interpret, reinterpret, runM, subsume, type (~>))
import Control.Monad.Freer.Delay (DelayEffect, handleDelayEffect)
import Control.Monad.Freer.Error (Error, handleError, runError, throwError)
import Control.Monad.Freer.Extras (LogMsg, mapLog)
import Control.Monad.Freer.Extras.Beam (BeamError)
import Control.Monad.Freer.Extras.Beam.Effects (BeamEffect, handleBeam)
import Control.Monad.Freer.Extras.Beam.Postgres qualified as Postgres (runBeam)
import Control.Monad.Freer.Extras.Beam.Sqlite qualified as Sqlite (runBeam)
import Control.Monad.Freer.Extras.Modify qualified as Modify
import Control.Monad.Freer.Reader (Reader, runReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Pool (Pool)
import Data.Typeable (Typeable)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Sqlite (Sqlite)
import Plutus.PAB.Db.Beam.ContractStore (handleContractStore)
import Plutus.PAB.Effects.Contract (ContractStore)
import Plutus.PAB.Effects.Contract.Builtin (Builtin, HasDefinitions)
import Plutus.PAB.Monitoring.Monitoring (convertLog, handleLogMsgTrace)
import Plutus.PAB.Monitoring.PABLogMsg (PABLogMsg (..), PABMultiAgentMsg (..))
import Plutus.PAB.Types (DBConnection (..), PABError (..))

-- | Run the ContractStore and ContractDefinitionStore effects on the
--   configured database.
runBeamStoreAction ::
    forall a b.
    ( ToJSON a
    , FromJSON a
    , HasDefinitions a
    , Typeable a
    )
    => DBConnection --Pool Postgres.Connection
    -> Trace IO (PABLogMsg (Builtin a))
    -> Eff '[ContractStore (Builtin a), LogMsg (PABMultiAgentMsg (Builtin a)), DelayEffect, IO] b
    -> IO (Either PABError b)
runBeamStoreAction (PostgresPool pool) trace =
    run pool trace
        (handleBeam Postgres.runBeam (convertLog (SMultiAgent . BeamLogItem) trace))
        (handleContractStore @Postgres)
runBeamStoreAction (SqlitePool pool) trace =
    run pool trace
        (handleBeam Sqlite.runBeam (convertLog (SMultiAgent . BeamLogItem) trace))
        (handleContractStore @Sqlite)

run :: forall p dbt a b
    .  Pool p
    -> Trace IO (PABLogMsg (Builtin a))
    -> BeamEffect dbt ~> Eff '[Reader (Pool p), Error BeamError, Error PABError, IO]
    -> ContractStore (Builtin a) ~> Eff '[ LogMsg (PABMultiAgentMsg (Builtin a))
                                          , DelayEffect
                                          , IO
                                          , BeamEffect dbt
                                          , Reader (Pool p)
                                          , Error BeamError
                                          , Error PABError
                                          , IO
                                          ]
    -> Eff '[ContractStore (Builtin a), LogMsg (PABMultiAgentMsg (Builtin a)), DelayEffect, IO] b
    -> IO (Either PABError b)
run pool trace hBeam hContractStore =
    runM
    . runError
    . flip handleError (throwError . BeamEffectError)
    . runReader pool
    . interpret hBeam
    . subsume @IO
    . handleDelayEffect
    . interpret (handleLogMsgTrace trace)
    . reinterpret (mapLog SMultiAgent)
    . interpret hContractStore
    . Modify.raiseEnd
