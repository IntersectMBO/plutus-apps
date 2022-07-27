{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Control.Monad.Freer.Extras.Beam.Postgres where

import Cardano.BM.Trace (Trace, logDebug)
import Control.Concurrent (threadDelay)
import Control.Exception (throw, try)
import Control.Monad.Freer (Eff, LastMember, Member, type (~>))
import Control.Monad.Freer.Extras.Beam.Common (BeamError (SqlError), BeamLog (..))
import Control.Monad.Freer.Reader (Reader, ask)
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Text qualified as Text
import Database.Beam (MonadIO (liftIO))
import Database.Beam.Postgres (Connection, Pg, runBeamPostgresDebug)
import Database.PostgreSQL.Simple qualified as Postgres

runBeam ::
  forall effs.
  ( LastMember IO effs
  , Member (Reader (Pool Connection)) effs
  )
  => Trace IO BeamLog
  -> Pg
  ~> Eff effs
runBeam trace action = do
  pool <- ask @(Pool Postgres.Connection)
  liftIO $ Pool.withResource pool $ \conn -> loop conn ( 5 :: Int )
  where
    loop conn retries = do
      let traceSql = logDebug trace . SqlLog
      resultEither <- try $ Postgres.withTransaction conn $ runBeamPostgresDebug traceSql conn action
      case resultEither of
          -- 'Database.SQLite.Simple.ErrorError' corresponds to an SQL error or
          -- missing database. When this exception is raised, we suppose it's
          -- because the another transaction was already running.
          Left Postgres.SqlError {} | retries > 0 -> do
              threadDelay 100_000
              loop conn (retries - 1)
          -- We handle and rethrow errors other than
          -- 'Database.SQLite.Simple.ErrorError'.
          Left e -> throw $ SqlError $ Text.pack $ show e
          Right v -> return v
