{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Control.Monad.Freer.Extras.Beam.Sqlite where

import Cardano.BM.Trace (Trace, logDebug)
import Control.Concurrent (threadDelay)
import Control.Exception (throw, try)
import Control.Monad.Freer (Eff, LastMember, Member, type (~>))
import Control.Monad.Freer.Extras.Beam.Common (BeamError (SqlError), BeamLog (..))
import Control.Monad.Freer.Reader (Reader, ask)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default, def)
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Text qualified as Text
import Database.Beam (MonadIO (liftIO))
import Database.Beam.Sqlite (SqliteM, runBeamSqliteDebug)
import Database.SQLite.Simple qualified as Sqlite
import GHC.Generics (Generic)

data DbConfig =
    DbConfig
    { dbConfigFile     :: Text.Text
    -- ^ The path to the sqlite database file. May be absolute or relative.
    , dbConfigPoolSize :: Int
    -- ^ Max number of concurrent sqlite database connections.
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Default database config uses an in-memory sqlite database that is shared
-- between all threads in the process.
defaultDbConfig :: DbConfig
defaultDbConfig = DbConfig
                  { dbConfigFile = "file::memory:?cache=shared"
                  , dbConfigPoolSize = 20
                  }

instance Default DbConfig where
  def = defaultDbConfig

runBeam ::
  forall effs.
  ( LastMember IO effs
  , Member (Reader (Pool Sqlite.Connection)) effs
  )
  => Trace IO BeamLog
  -> SqliteM
  ~> Eff effs
runBeam trace action = do
  pool <- ask @(Pool Sqlite.Connection)
  liftIO $ Pool.withResource pool $ \conn -> loop conn ( 5 :: Int )
  where
    loop conn retries = do
      let traceSql = logDebug trace . SqlLog
      resultEither <- try $ Sqlite.withTransaction conn $ runBeamSqliteDebug traceSql conn action
      case resultEither of
          -- 'Database.SQLite.Simple.ErrorError' corresponds to an SQL error or
          -- missing database. When this exception is raised, we suppose it's
          -- because the another transaction was already running.
          Left (Sqlite.SQLError Sqlite.ErrorError _ _) | retries > 0 -> do
              threadDelay 100_000
              loop conn (retries - 1)
          -- We handle and rethrow errors other than
          -- 'Database.SQLite.Simple.ErrorError'.
          Left e -> throw $ SqlError $ Text.pack $ show e
          Right v -> return v
