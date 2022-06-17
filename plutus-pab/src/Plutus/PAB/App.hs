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

import Cardano.BM.Trace (Trace, logDebug)
import Cardano.ChainIndex.Types qualified as ChainIndex
import Cardano.Node.Client (handleNodeClientClient, runChainSyncWithCfg)
import Cardano.Node.Params qualified as Params
import Cardano.Node.Types (ChainSyncHandle, NodeMode (AlonzoNode, MockNode),
                           PABServerConfig (PABServerConfig, pscBaseUrl, pscNodeMode, pscSocketPath))
import Cardano.Protocol.Socket.Mock.Client qualified as MockClient
import Cardano.Wallet.LocalClient qualified as LocalWalletClient
import Cardano.Wallet.Mock.Client qualified as WalletMockClient
import Cardano.Wallet.RemoteClient qualified as RemoteWalletClient
import Cardano.Wallet.Types qualified as Wallet
import Control.Concurrent.STM qualified as STM
import Control.Lens (preview)
import Control.Monad.Freer (Eff, LastMember, Member, interpret, reinterpret, reinterpret2, reinterpretN, type (~>))
import Control.Monad.Freer.Error (Error, handleError, throwError)
import Control.Monad.Freer.Extras.Beam (handleBeam)
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
import Plutus.PAB.Db.Schema (checkedSqliteDb)
import Plutus.PAB.Effects.Contract (ContractDefinition (AddDefinition, GetDefinitions))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (BuiltinHandler, contractHandler),
                                            HasDefinitions (getDefinitions))
import Plutus.PAB.Monitoring.Monitoring (convertLog, handleLogMsgTrace)
import Plutus.PAB.Monitoring.PABLogMsg (PABLogMsg (SMultiAgent), PABMultiAgentMsg (BeamLogItem, UserLog, WalletClient),
                                        WalletClientMsg)
import Plutus.PAB.Timeout (Timeout (Timeout))
import Plutus.PAB.Types (Config (Config), DbConfig (..),
                         PABError (BeamEffectError, ChainIndexError, NodeClientError, RemoteWalletWithMockNodeError, WalletClientError, WalletError),
                         WebserverConfig (WebserverConfig), chainIndexConfig, dbConfig, endpointTimeout,
                         nodeServerConfig, pabWebserverConfig, walletServerConfig)
import Servant.Client (ClientEnv, ClientError, mkClientEnv)
import Wallet.API (NodeClientEffect)
import Wallet.Effects (WalletEffect)
import Wallet.Emulator.Wallet (Wallet)
import Wallet.Error (WalletAPIError)
import Wallet.Types (ContractInstanceId)

------------------------------------------------------------

-- | Application environment with a contract type `a`.
data AppEnv a =
    AppEnv
        { dbPool                :: Pool Sqlite.Connection
        , walletClientEnv       :: Maybe ClientEnv -- ^ No 'ClientEnv' when in the remote client setting.
        , nodeClientEnv         :: ClientEnv
        , chainIndexEnv         :: ClientEnv
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
            instancesState <- liftIO $ STM.atomically Instances.emptyInstancesState
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

            BeamSqliteBackend ->
              interpret (handleLogMsgTrace trace)
              . reinterpret (mapLog @_ @(PABLogMsg (Builtin a)) SMultiAgent)
              . interpret (Core.handleUserEnvReader @(Builtin a) @(AppEnv a))
              . interpret (Core.handleMappedReader @(AppEnv a) dbPool)
              . flip handleError (throwError . BeamEffectError)
              . interpret (handleBeam (convertLog (SMultiAgent . BeamLogItem) trace))
              . reinterpretN @'[_, _, _, _, _] BeamEff.handleContractStore

        , handleContractDefinitionEffect =
            interpret (handleLogMsgTrace trace)
            . reinterpret (mapLog @_ @(PABLogMsg (Builtin a)) SMultiAgent)
            . interpret (Core.handleUserEnvReader @(Builtin a) @(AppEnv a))
            . interpret (Core.handleMappedReader @(AppEnv a) dbPool)
            . flip handleError (throwError . BeamEffectError)
            . interpret (handleBeam (convertLog (SMultiAgent . BeamLogItem) trace))
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
            . reinterpret (Core.handleMappedReader @(AppEnv a) @ClientEnv chainIndexEnv)
            . reinterpret2 (ChainIndex.handleChainIndexClient @IO)

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
handleWalletEffect nodeCfg@PABServerConfig { pscNodeMode = AlonzoNode } cidM w eff = do
    clientEnvM <- ask @(Maybe ClientEnv)
    case clientEnvM of
        Nothing -> RemoteWalletClient.handleWalletClient nodeCfg cidM eff
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
    config@Config{pabWebserverConfig=WebserverConfig{endpointTimeout}} =
    Core.runPAB (Timeout endpointTimeout) (appEffectHandlers storageBackend config trace contractHandler)

type App a b = PABAction (Builtin a) (AppEnv a) b

data StorageBackend = BeamSqliteBackend | InMemoryBackend
  deriving (Eq, Ord, Show)

mkEnv :: Trace IO (PABLogMsg (Builtin a)) -> Config -> IO (AppEnv a)
mkEnv appTrace appConfig@Config { dbConfig
             , nodeServerConfig = PABServerConfig{pscBaseUrl, pscSocketPath, pscNodeMode}
             , walletServerConfig
             , chainIndexConfig
             } = do
    walletClientEnv <- maybe (pure Nothing) (fmap Just . clientEnv) $ preview Wallet._LocalWalletConfig walletServerConfig
    nodeClientEnv <- clientEnv pscBaseUrl
    chainIndexEnv <- clientEnv (ChainIndex.ciBaseUrl chainIndexConfig)
    dbPool <- dbConnect appTrace dbConfig
    txSendHandle <-
      case pscNodeMode of
        AlonzoNode -> pure Nothing
        MockNode   ->
          liftIO $ Just <$> MockClient.runTxSender pscSocketPath
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

logDebugString :: Trace IO (PABLogMsg t) -> Text -> IO ()
logDebugString trace = logDebug trace . SMultiAgent . UserLog

-- | Initialize/update the database to hold our effects.
migrate :: Trace IO (PABLogMsg (Builtin a)) -> DbConfig -> IO ()
migrate trace config = do
    pool <- dbConnect trace config
    logDebugString trace "Running beam migration"
    Pool.withResource pool (runBeamMigration trace)

runBeamMigration
  :: Trace IO (PABLogMsg (Builtin a))
  -> Sqlite.Connection
  -> IO ()
runBeamMigration trace conn = Sqlite.runBeamSqliteDebug (logDebugString trace . pack) conn $ do
  autoMigrate Sqlite.migrationBackend checkedSqliteDb

-- | Connect to the database.
dbConnect :: Trace IO (PABLogMsg (Builtin a)) -> DbConfig -> IO (Pool Sqlite.Connection)
dbConnect trace DbConfig {dbConfigFile, dbConfigPoolSize} = do
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
