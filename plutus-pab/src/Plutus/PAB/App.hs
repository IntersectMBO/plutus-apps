{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Plutus.PAB.App(
    App,
    runApp,
    AppEnv(..),
    StorageBackend(..),
    -- * App actions
    migrate,
    dbConnect,
    handleContractDefinition
    ) where

import Cardano.Api.NetworkId.Extra (NetworkIdWrapper (unNetworkIdWrapper))
import Cardano.BM.Trace (Trace, logDebug)
import Cardano.ChainIndex.Types qualified as ChainIndex
import Cardano.Node.Client (handleNodeClientClient, runChainSyncWithCfg)
import Cardano.Node.Params qualified as Params
import Cardano.Node.Types (ChainSyncHandle, NodeMode (MockNode),
                           PABServerConfig (PABServerConfig, pscBaseUrl, pscNetworkId, pscNodeMode, pscSocketPath))
import Cardano.Protocol.Socket.Mock.Client qualified as MockClient
import Cardano.Wallet.LocalClient qualified as LocalWalletClient
import Cardano.Wallet.Mock.Client qualified as WalletMockClient
import Cardano.Wallet.RemoteClient qualified as RemoteWalletClient
import Cardano.Wallet.Types qualified as Wallet
import Control.Lens (preview, (^?!))
import Control.Monad (void)
import Control.Monad.Freer (Eff, LastMember, Member, interpret, reinterpret, reinterpret2, reinterpretN, type (~>))
import Control.Monad.Freer.Error (Error, handleError, throwError)
import Control.Monad.Freer.Extras.Beam.Effects (handleBeam)
import Control.Monad.Freer.Extras.Beam.Postgres qualified as Postgres (DbConfig (..), runBeam)
import Control.Monad.Freer.Extras.Beam.Sqlite qualified as Sqlite (DbConfig (..), runBeam)
import Control.Monad.Freer.Extras.Log (LogMsg, mapLog)
import Control.Monad.Freer.Reader (Reader, ask, runReader)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)
import Database.Beam.Migrate.Simple (autoMigrate)
import Database.Beam.Postgres qualified as Postgres
import Database.Beam.Postgres.Migrate qualified as Postgres
import Database.Beam.Sqlite qualified as Sqlite
import Database.Beam.Sqlite.Migrate qualified as Sqlite
import Database.SQLite.Simple qualified as Sqlite
import Network.HTTP.Client (ManagerSettings (managerResponseTimeout), managerModifyRequest, newManager,
                            responseTimeoutMicro, setRequestIgnoreStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Plutus.ChainIndex.Client qualified as ChainIndex
import Plutus.PAB.Core (EffectHandlers (EffectHandlers), PABAction)
import Plutus.PAB.Core qualified as Core
import Plutus.PAB.Core.ContractInstance.BlockchainEnv qualified as BlockchainEnv
import Plutus.PAB.Core.ContractInstance.STM as Instances (InstancesState, emptyInstancesState)
import Plutus.PAB.Db.Beam.ContractStore qualified as BeamEff
import Plutus.PAB.Db.Memory.ContractStore (InMemInstances, initialInMemInstances)
import Plutus.PAB.Db.Memory.ContractStore qualified as InMem
import Plutus.PAB.Db.Schema (checkedPostgresDb, checkedSqliteDb)
import Plutus.PAB.Effects.Contract (ContractDefinition (AddDefinition, GetDefinitions))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (BuiltinHandler, contractHandler),
                                            HasDefinitions (getDefinitions))
import Plutus.PAB.Monitoring.Monitoring (convertLog, handleLogMsgTrace)
import Plutus.PAB.Monitoring.PABLogMsg (PABLogMsg (SMultiAgent), PABMultiAgentMsg (BeamLogItem, UserLog, WalletClient),
                                        WalletClientMsg)
import Plutus.PAB.Timeout (Timeout (Timeout))
import Plutus.PAB.Types (ChainQueryConfig (..), ChainQueryEnv (..), Config (Config), DBConnection (..), DbConfig (..),
                         PABError (BeamEffectError, ChainIndexError, NodeClientError, RemoteWalletWithMockNodeError, WalletClientError, WalletError),
                         WebserverConfig (WebserverConfig), _PostgresPool, _SqlitePool, chainQueryConfig, dbConfig,
                         endpointTimeout, getBlockfrostEnv, getChainIndexEnv, nodeServerConfig, pabWebserverConfig,
                         waitStatusTimeout, walletServerConfig)
import Servant.Client (ClientEnv, ClientError, mkClientEnv)
import Wallet.API (NodeClientEffect)
import Wallet.Effects (WalletEffect)
import Wallet.Emulator.Wallet (Wallet)
import Wallet.Error (WalletAPIError)
import Wallet.Types (ContractInstanceId)

import Database.Beam.Postgres (Postgres)
import Database.Beam.Sqlite (Sqlite)
import Plutus.Blockfrost.Client as BlockfrostClient
import Plutus.Blockfrost.Types qualified as BF (BlockfrostConfig (bfTokenPath), BlockfrostEnv (..))

------------------------------------------------------------

-- | Application environment with a contract type `a`.
data AppEnv a =
    AppEnv
        { dbPool                :: DBConnection
        , walletClientEnv       :: Maybe ClientEnv -- ^ No 'ClientEnv' when in the remote client setting.
        , nodeClientEnv         :: ClientEnv
        , chainQueryEnv         :: ChainQueryEnv
        , txSendHandle          :: Maybe MockClient.TxSendHandle -- No 'TxSendHandle' required when connecting to the real node.
        , chainSyncHandle       :: ChainSyncHandle
        , appConfig             :: Config
        , appTrace              :: Trace IO (PABLogMsg (Builtin a))
        , appInMemContractStore :: InMemInstances (Builtin a)
        }

appEffectHandlers
  :: forall a.
  ( FromJSON a
  , ToJSON a
  , HasDefinitions a
  , Typeable a
  )
  => StorageBackend
  -> Config
  -> Trace IO (PABLogMsg (Builtin a))
  -> BuiltinHandler a
  -> EffectHandlers (Builtin a) (AppEnv a)
appEffectHandlers storageBackend config trace BuiltinHandler{contractHandler} =
    EffectHandlers
        { initialiseEnvironment = do
            env <- liftIO $ mkEnv trace config
            instancesState <- liftIO Instances.emptyInstancesState
            blockchainEnv <- liftIO $ BlockchainEnv.startNodeClient config instancesState
            pure (instancesState, blockchainEnv, env)

        , handleLogMessages =
            interpret (handleLogMsgTrace trace)
            . reinterpret (mapLog SMultiAgent)

        , handleContractEffect =
            interpret (handleLogMsgTrace trace)
            . reinterpret contractHandler

        , handleContractStoreEffect =
          case storageBackend of
            InMemoryBackend ->
              interpret (Core.handleUserEnvReader @(Builtin a) @(AppEnv a))
              . interpret (Core.handleMappedReader @(AppEnv a) appInMemContractStore)
              . reinterpret2 InMem.handleContractStore

            BeamBackend ->
              interpret (handleLogMsgTrace trace)
              . reinterpret (mapLog @_ @(PABLogMsg (Builtin a)) SMultiAgent)
              . flip handleError (throwError . BeamEffectError)
              . interpret (Core.handleUserEnvReader @(Builtin a) @(AppEnv a))
              . case dbConfig config of
                    SqliteDB _ ->
                          interpret (Core.handleMappedReader @(AppEnv a) ((^?!_SqlitePool) . dbPool))
                        . interpret (handleBeam Sqlite.runBeam (convertLog (SMultiAgent . BeamLogItem) trace))
                        . reinterpretN @'[_, _, _, _, _] (BeamEff.handleContractStore @Sqlite)
                    PostgresDB _ ->
                          interpret (Core.handleMappedReader @(AppEnv a) ((^?!_PostgresPool) . dbPool))
                        . interpret (handleBeam Postgres.runBeam (convertLog (SMultiAgent . BeamLogItem) trace))
                        . reinterpretN @'[_, _, _, _, _] (BeamEff.handleContractStore @Postgres)

        , handleContractDefinitionEffect =
            interpret (handleLogMsgTrace trace)
            . reinterpret (mapLog @_ @(PABLogMsg (Builtin a)) SMultiAgent)
            . flip handleError (throwError . BeamEffectError)
            . case dbConfig config of
                  SqliteDB _ -> interpret (Core.handleUserEnvReader @(Builtin a) @(AppEnv a))
                                . interpret (Core.handleMappedReader @(AppEnv a) ((^?!_SqlitePool) . dbPool))
                                . interpret (handleBeam Sqlite.runBeam (convertLog (SMultiAgent . BeamLogItem) trace))
                                . reinterpretN @'[_, _, _, _, _ ] handleContractDefinition
                  PostgresDB _ -> interpret (Core.handleUserEnvReader @(Builtin a) @(AppEnv a))
                                . interpret (Core.handleMappedReader @(AppEnv a) ((^?!_PostgresPool) . dbPool))
                                . interpret (handleBeam Postgres.runBeam (convertLog (SMultiAgent . BeamLogItem) trace))
                                . reinterpretN @'[_, _, _, _, _] handleContractDefinition

        , handleServicesEffects = \wallet cidM -> do
            -- handle 'NodeClientEffect'
            flip handleError (throwError . NodeClientError)
            . interpret (Core.handleUserEnvReader @(Builtin a) @(AppEnv a))
            . reinterpret (Core.handleMappedReader @(AppEnv a) @ChainSyncHandle chainSyncHandle)
            . interpret (Core.handleUserEnvReader @(Builtin a) @(AppEnv a))
            . reinterpret (Core.handleMappedReader @(AppEnv a) @(Maybe MockClient.TxSendHandle) txSendHandle)
            . interpret (Core.handleUserEnvReader @(Builtin a) @(AppEnv a))
            . reinterpret (Core.handleMappedReader @(AppEnv a) @ClientEnv nodeClientEnv)
            . reinterpretN @'[_, _, _, _]
              (\nodeClientEffect -> do
                params <- liftIO $ Params.fromPABServerConfig $ nodeServerConfig config
                handleNodeClientClient @IO params nodeClientEffect)

            -- handle 'ChainIndexEffect'
            . flip handleError (throwError . ChainIndexError)
            . interpret (Core.handleUserEnvReader @(Builtin a) @(AppEnv a))
            . (case chainQueryConfig config of
                ChainIndexConfig _ -> reinterpret (Core.handleMappedReader @(AppEnv a) @ClientEnv (getChainIndexEnv . chainQueryEnv))
                                    . reinterpret2 (ChainIndex.handleChainIndexClient @IO)
                BlockfrostConfig _ -> reinterpret (Core.handleMappedReader @(AppEnv a) @BF.BlockfrostEnv (getBlockfrostEnv . chainQueryEnv))
                                    . reinterpret2 (BlockfrostClient.handleBlockfrostClient @IO))

            -- handle 'WalletEffect'
            . flip handleError (throwError . WalletClientError)
            . flip handleError (throwError . WalletError)
            . interpret (mapLog @_ @(PABMultiAgentMsg (Builtin a)) WalletClient)
            . interpret (Core.handleUserEnvReader @(Builtin a) @(AppEnv a))
            . reinterpret (Core.handleMappedReader @(AppEnv a) @(Maybe ClientEnv) walletClientEnv)
            . interpret (Core.handleUserEnvReader @(Builtin a) @(AppEnv a))
            . interpret (Core.handleInstancesStateReader @(Builtin a) @(AppEnv a))
            . reinterpretN @'[_, _, _, _, _, _] (handleWalletEffect (nodeServerConfig config) cidM wallet)

        , onStartup = pure ()

        , onShutdown = pure ()
        }

handleWalletEffect
  :: forall effs.
  ( LastMember IO effs
  , Member NodeClientEffect effs
  , Member (Error ClientError) effs
  , Member (Error WalletAPIError) effs
  , Member (Error PABError) effs
  , Member (Reader (Maybe ClientEnv)) effs
  , Member (LogMsg WalletClientMsg) effs
  , Member (Reader InstancesState) effs
  )
  => PABServerConfig
  -> Maybe ContractInstanceId
  -> Wallet
  -> WalletEffect
  ~> Eff effs
handleWalletEffect PABServerConfig { pscNodeMode = MockNode } _ w eff = do
    clientEnvM <- ask @(Maybe ClientEnv)
    case clientEnvM of
        Nothing -> throwError RemoteWalletWithMockNodeError
        Just clientEnv ->
            runReader clientEnv $ WalletMockClient.handleWalletClient @IO w eff
handleWalletEffect nodeCfg cidM w eff = do
    clientEnvM <- ask @(Maybe ClientEnv)
    case clientEnvM of
        Nothing -> RemoteWalletClient.handleWalletClient cidM eff
        Just clientEnv ->
            runReader clientEnv $ LocalWalletClient.handleWalletClient @IO nodeCfg w eff

runApp ::
    forall a b.
    ( FromJSON a
    , ToJSON a
    , HasDefinitions a
    , Typeable a
    )
    => StorageBackend
    -> Trace IO (PABLogMsg (Builtin a)) -- ^ Top-level tracer
    -> BuiltinHandler a
    -> Config -- ^ Client configuration
    -> App a b -- ^ Action
    -> IO (Either PABError b)
runApp
    storageBackend
    trace
    contractHandler
    config@Config{pabWebserverConfig=WebserverConfig{endpointTimeout, waitStatusTimeout}} =
    Core.runPAB (Timeout endpointTimeout) (Timeout waitStatusTimeout) (appEffectHandlers storageBackend config trace contractHandler)

type App a b = PABAction (Builtin a) (AppEnv a) b

data StorageBackend = BeamBackend | InMemoryBackend
  deriving (Eq, Ord, Show)

mkEnv :: Trace IO (PABLogMsg (Builtin a)) -> Config -> IO (AppEnv a)
mkEnv appTrace appConfig@Config { dbConfig
             , nodeServerConfig = PABServerConfig{pscBaseUrl, pscSocketPath, pscNodeMode, pscNetworkId}
             , walletServerConfig
             , chainQueryConfig
             } = do
    walletClientEnv <- maybe (pure Nothing) (fmap Just . clientEnv) $ preview Wallet._LocalWalletConfig walletServerConfig
    nodeClientEnv <- clientEnv pscBaseUrl
    chainQueryEnv <- mkChainQueryEnv
    dbPool <- dbConnect dbConfig appTrace
    txSendHandle <-
      case pscNodeMode of
        MockNode -> liftIO $ Just <$> MockClient.runTxSender pscSocketPath
        _        -> pure Nothing
    -- This is for access to the slot number in the interpreter
    chainSyncHandle <- runChainSyncWithCfg $ nodeServerConfig appConfig
    appInMemContractStore <- liftIO initialInMemInstances
    pure AppEnv {..}
  where
    clientEnv baseUrl = mkClientEnv <$> liftIO mkManager <*> pure (coerce baseUrl)

    mkManager =
        newManager $
        tlsManagerSettings { managerModifyRequest = pure . setRequestIgnoreStatus
                           , managerResponseTimeout = responseTimeoutMicro 60_000_000 }

    mkChainQueryEnv :: IO ChainQueryEnv
    mkChainQueryEnv = case chainQueryConfig of
        ChainIndexConfig config -> ChainIndexEnv <$> clientEnv (ChainIndex.ciBaseUrl config)
        BlockfrostConfig config -> return $ BlockfrostEnv $
            BF.BlockfrostEnv { envBfTokenPath = BF.bfTokenPath config
                             , envNetworkId = unNetworkIdWrapper pscNetworkId}

logDebugString :: Trace IO (PABLogMsg t) -> Text -> IO ()
logDebugString trace = logDebug trace . SMultiAgent . UserLog

dbConnect :: DbConfig -> Trace IO (PABLogMsg (Builtin a)) -> IO DBConnection
dbConnect (PostgresDB db) trace = PostgresPool <$> dbConnectPostgres db trace
dbConnect (SqliteDB db) trace   = SqlitePool <$> dbConnectSqlite db trace

migrate :: DbConfig -> Trace IO (PABLogMsg (Builtin a)) -> IO ()
migrate (PostgresDB db) = void . migratePostgres db
migrate (SqliteDB db)   = void . migrateSqlite db

-- POSTGRES
-- | Initialize/update the database to hold our effects.
migratePostgres :: Postgres.DbConfig -> Trace IO (PABLogMsg (Builtin a)) -> IO ()
migratePostgres config trace = do
    pool <- dbConnectPostgres config trace
    logDebugString trace "Running beam migration"
    Pool.withResource pool (runBeamMigrationPostgres trace)

runBeamMigrationPostgres
  :: Trace IO (PABLogMsg (Builtin a))
  -> Postgres.Connection
  -> IO ()
runBeamMigrationPostgres trace conn = Postgres.runBeamPostgresDebug (logDebugString trace . pack) conn $ do
  autoMigrate Postgres.migrationBackend checkedPostgresDb

-- | Connect to the database.
dbConnectPostgres :: Postgres.DbConfig -> Trace IO (PABLogMsg (Builtin a)) -> IO (Pool Postgres.Connection)
dbConnectPostgres Postgres.DbConfig{..} trace = do
  pool <- Pool.createPool
    (Postgres.connect Postgres.ConnectInfo {
      connectHost=unpack dbConfigHost,
      connectPort=dbConfigPort,
      connectUser=unpack dbConfigUser,
      connectPassword=unpack dbConfigPass,
      connectDatabase=unpack dbConfigDatabase
    })
    Postgres.close
    dbConfigPoolSize
    5_000_000
    5
  logDebugString trace $ "Connecting to DB: " <> databaseStr
  return pool
  where
    databaseStr :: Text
    databaseStr = dbConfigHost <> ":" <> (pack . show) dbConfigPort

-- SQLITE
migrateSqlite :: Sqlite.DbConfig -> Trace IO (PABLogMsg (Builtin a)) -> IO ()
migrateSqlite config trace = do
    pool <- dbConnectSqlite config trace
    logDebugString trace "Running beam migration"
    Pool.withResource pool (runBeamMigrationSqlite trace)

runBeamMigrationSqlite
  :: Trace IO (PABLogMsg (Builtin a))
  -> Sqlite.Connection
  -> IO ()
runBeamMigrationSqlite trace conn = Sqlite.runBeamSqliteDebug (logDebugString trace . pack) conn $ do
  autoMigrate Sqlite.migrationBackend checkedSqliteDb

dbConnectSqlite :: Sqlite.DbConfig -> Trace IO (PABLogMsg (Builtin a)) -> IO (Pool Sqlite.Connection)
dbConnectSqlite Sqlite.DbConfig {dbConfigFile, dbConfigPoolSize} trace = do
  pool <- Pool.createPool (Sqlite.open $ unpack dbConfigFile) Sqlite.close dbConfigPoolSize 5_000_000 5
  logDebugString trace $ "Connecting to DB: " <> dbConfigFile
  return pool

handleContractDefinition ::
  forall a effs. HasDefinitions a
  => ContractDefinition (Builtin a)
  ~> Eff effs
handleContractDefinition = \case
  AddDefinition _ -> pure ()
  GetDefinitions  -> pure getDefinitions
