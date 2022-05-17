{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}

module Plutus.PAB.Run.Cli where -- (ConfigCommandArgs(..), runConfigCommand) where

-----------------------------------------------------------------------------------------------------------------------
-- Command interpretation
-----------------------------------------------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Cardano.Api.NetworkId.Extra (unNetworkIdWrapper)
import Cardano.BM.Configuration (Configuration)
import Cardano.BM.Data.Trace (Trace)
import Cardano.ChainIndex.Server qualified as ChainIndex
import Cardano.Node.Server qualified as NodeServer
import Cardano.Node.Types (NodeMode (AlonzoNode, MockNode),
                           PABServerConfig (pscNetworkId, pscNodeMode, pscSlotConfig, pscSocketPath), _AlonzoNode)
import Cardano.Protocol.Socket.Type (epochSlots)
import Cardano.Wallet.Mock.Server qualified as WalletServer
import Cardano.Wallet.Mock.Types (WalletMsg)
import Cardano.Wallet.Types (WalletConfig (LocalWalletConfig, RemoteWalletConfig))
import Control.Concurrent (takeMVar, threadDelay)
import Control.Concurrent.Async (Async, async, waitAny)
import Control.Concurrent.Availability (Availability, available, starting)
import Control.Concurrent.STM qualified as STM
import Control.Lens (preview)
import Control.Monad (forM, forM_, forever, void, when)
import Control.Monad.Freer (Eff, LastMember, Member, interpret, runM)
import Control.Monad.Freer.Delay (DelayEffect, delayThread, handleDelayEffect)
import Control.Monad.Freer.Error (throwError)
import Control.Monad.Freer.Extras.Log (LogMsg, logInfo)
import Control.Monad.Freer.Reader (ask, runReader)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logErrorN, runStdoutLoggingT)
import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Aeson.OneLine (renderValue)
import Data.Bifunctor qualified as Bifunctor
import Data.Foldable (traverse_)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.OpenApi.Schema qualified as OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified as T
import Data.Text.Extras (tshow)
import Data.Time.Units (Second)
import Data.Typeable (Typeable)
import Plutus.Contract.Resumable (responses)
import Plutus.Contract.State (State (State, record))
import Plutus.Contract.State qualified as State
import Plutus.PAB.App qualified as App
import Plutus.PAB.Core qualified as Core
import Plutus.PAB.Core.ContractInstance (ContractInstanceState (ContractInstanceState), updateState)
import Plutus.PAB.Core.ContractInstance.STM (InstanceState, emptyInstanceState)
import Plutus.PAB.Db.Beam qualified as Beam
import Plutus.PAB.Db.FS (runFSStoreAction)
import Plutus.PAB.Db.FS.Types (ContractStoreDir (ContractStoreDir))
import Plutus.PAB.Db.Memory.ContractStore (initialInMemInstances)
import Plutus.PAB.Effects.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler, HasDefinitions, SomeBuiltinState, getResponse)
import Plutus.PAB.Monitoring.Monitoring qualified as LM
import Plutus.PAB.Run.Command (ConfigCommand (ChainIndex, ContractState, ForkCommands, Migrate, MockWallet, PABWebserver, ReportActiveContracts, ReportAvailableContracts, ReportContractHistory, StartNode))
import Plutus.PAB.Types (Config (Config, contractStoreConfig, pabWebserverConfig),
                         ContractStoreBackend (FSContractStore, InMemoryContractStore, SqliteContractStore),
                         ContractStoreConfig (UseFSStore, UseInMemoryStore, UseSqliteStore), PABError, SqliteConfig,
                         SqliteConnectionPool, chainIndexConfig, nodeServerConfig, walletServerConfig)
import Plutus.PAB.Webserver.Server qualified as PABServer
import Plutus.PAB.Webserver.Types (ContractActivationArgs (ContractActivationArgs, caID, caWallet))
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)
import Servant qualified
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import Wallet.Emulator.Wallet qualified as Wallet
import Wallet.Types qualified as Wallet

data ConfigCommandArgs a =
    ConfigCommandArgs
        { ccaTrace         :: Trace IO (LM.AppMsg (Builtin a))  -- ^ PAB Tracer logging instance
        , ccaLoggingConfig :: Configuration -- ^ Monitoring configuration
        , ccaPABConfig     :: Config        -- ^ PAB Configuration
        , ccaAvailability  :: Availability  -- ^ Token for signaling service availability
        , ccaInMemoryStore :: Bool -- ^ Allows to override contract store setup
        }

toPABMsg :: Trace m (LM.AppMsg (Builtin a)) -> Trace m (LM.PABLogMsg (Builtin a))
toPABMsg = LM.convertLog LM.PABMsg

withSQLite :: Trace IO (LM.AppMsg (Builtin a)) -> SqliteConfig -> (SqliteConnectionPool -> IO b) -> IO b
withSQLite trace sqliteConfig h = do
  let
    trace' = toPABMsg trace
  pool <- App.dbConnect trace' sqliteConfig
  h pool

withSQLite' :: Trace IO (LM.AppMsg (Builtin a)) -> ContractStoreConfig -> (SqliteConnectionPool -> IO b) -> IO b
withSQLite' trace (UseSqliteStore sqliteConfig) h = withSQLite trace sqliteConfig h
withSQLite' _ _ _                          = do
  hPutStrLn stderr "Given command is only supported for SQLite contract store"
  exitWith $ ExitFailure 1

withPersistentContractsStore :: Trace IO (LM.AppMsg (Builtin a)) -> ContractStoreConfig -> (SqliteConnectionPool -> IO b) -> (ContractStoreDir (Builtin a) -> IO b) -> IO b
withPersistentContractsStore trace (UseSqliteStore sqliteConfig) sqliteAction _ = withSQLite trace sqliteConfig sqliteAction
withPersistentContractsStore _ (UseFSStore dir) _ fsAction = fsAction (ContractStoreDir dir)
withPersistentContractsStore _ UseInMemoryStore _ _ = do
  hPutStrLn stderr "Given command is only supported for persistent contract stores."
  exitWith $ ExitFailure 1

runPersistentStoreAction
  :: (ToJSON a, FromJSON a, HasDefinitions a, Typeable a)
  => Trace IO (LM.AppMsg (Builtin a))
  -> ContractStoreConfig
  -> Eff '[Contract.ContractStore (Builtin a), LogMsg (LM.PABMultiAgentMsg (Builtin a)), DelayEffect, IO] b
  -> IO (Either PABError b)
runPersistentStoreAction trace config action = withPersistentContractsStore trace config runSqlite runFS
  where
    runSqlite conn = Beam.runBeamStoreAction
        conn
        (LM.convertLog LM.PABMsg trace)
        action
    runFS contractStoreDir = runFSStoreAction
      contractStoreDir
      (LM.convertLog LM.PABMsg trace)
      action

logPersistentStoreAction
  :: forall a b
  . (ToJSON a, FromJSON a, ToJSON b, HasDefinitions a, Typeable a)
  => Trace IO (LM.AppMsg (Builtin a))
  -> ContractStoreConfig
  -> Eff '[Contract.ContractStore (Builtin a), LogMsg (LM.PABMultiAgentMsg (Builtin a)), DelayEffect, IO] b
  -> IO ()
logPersistentStoreAction trace config action = do
  (res :: Either PABError b) <- runPersistentStoreAction trace config action
  putStrLn $ T.unpack $ renderValue $ toJSON (Bifunctor.first (error . show) res :: Either String b)


-- | Interpret a 'Command' in 'Eff' using the provided tracer and configurations
--
runConfigCommand :: forall a.
    ( Ord a
    , Show a
    , ToJSON a
    , FromJSON a
    , Pretty a
    , Servant.MimeUnrender Servant.JSON a
    , HasDefinitions a
    , OpenApi.ToSchema a
    )
    => BuiltinHandler a
    -> ConfigCommandArgs a
    -> ConfigCommand
    -> IO ()

-- Run the database migration
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig=Config{contractStoreConfig}} Migrate =
  withSQLite' ccaTrace contractStoreConfig $ App.migrate (toPABMsg ccaTrace)

-- Run mock wallet service
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig = Config {nodeServerConfig, chainIndexConfig, walletServerConfig = LocalWalletConfig ws},ccaAvailability} MockWallet =
    liftIO $ WalletServer.main
        (toWalletLog ccaTrace)
        ws
        (pscSocketPath nodeServerConfig)
        (pscSlotConfig nodeServerConfig)
        (ChainIndex.ciBaseUrl chainIndexConfig)
        ccaAvailability

-- Run mock wallet service
runConfigCommand _ ConfigCommandArgs{ccaPABConfig = Config {walletServerConfig = RemoteWalletConfig}} MockWallet =
    error "Plutus.PAB.Run.Cli.runConfigCommand: Can't run mock wallet in remote wallet config."

-- Run mock node server
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig = Config {nodeServerConfig},ccaAvailability} StartNode = do
    case pscNodeMode nodeServerConfig of
        MockNode -> do
            liftIO $ NodeServer.main
                (toMockNodeServerLog ccaTrace)
                nodeServerConfig
                ccaAvailability
        AlonzoNode -> do
            available ccaAvailability
            -- The semantics of Command(s) is that once a set of commands are
            -- started if any finishes the entire application is terminated. We want
            -- to prevent that by keeping the thread suspended.
            forever $ threadDelay 1000000000

-- Run PAB webserver
runConfigCommand
    contractHandler
    ConfigCommandArgs { ccaTrace
                      , ccaPABConfig =
                          config@Config { pabWebserverConfig, nodeServerConfig }
                      , ccaAvailability, ccaInMemoryStore
                      } PABWebserver = do

    when (isJust $ preview _AlonzoNode $ pscNodeMode nodeServerConfig) $ do
        C.ChainTip slotNo _ _ <- C.getLocalChainTip $ C.LocalNodeConnectInfo
            { C.localConsensusModeParams = C.CardanoModeParams epochSlots
            , C.localNodeNetworkId = unNetworkIdWrapper $ pscNetworkId nodeServerConfig
            , C.localNodeSocketPath = pscSocketPath nodeServerConfig
            }
        LM.runLogEffects ccaTrace $ do
            logInfo @(LM.AppMsg (Builtin a))
                $ LM.PABMsg
                $ LM.SCoreMsg
                $ LM.ConnectingToAlonzoNode nodeServerConfig slotNo

    let
      -- Use Contract store override flag
      config' = if ccaInMemoryStore
        then config { contractStoreConfig = UseInMemoryStore }
        else config

      readContracts = do
        cIds   <- Map.toList <$> Contract.getActiveContracts @(Builtin a)
        forM cIds $ \(cid, args) -> do
           s <- Contract.getState @(Builtin a) cid
           let priorContract :: (SomeBuiltinState a, Wallet.ContractInstanceId, ContractActivationArgs a)
               priorContract = (s, cid, args)
           pure priorContract

    -- Restore the running contracts by first collecting up enough details about the
    -- previous contracts to re-start them
    previousContracts <- case contractStoreConfig config' of
      UseSqliteStore sqliteConfig -> do
        connection <- App.dbConnect (LM.convertLog LM.PABMsg ccaTrace) sqliteConfig
        -- Restore the running contracts by first collecting up enough details about the
        -- previous contracts to re-start them
        Beam.runBeamStoreAction
          connection
          (LM.convertLog LM.PABMsg ccaTrace)
          readContracts
      UseFSStore dir ->
        runFSStoreAction
          (ContractStoreDir dir)
          (LM.convertLog LM.PABMsg ccaTrace)
          readContracts
      UseInMemoryStore -> pure $ Right []

    -- TODO paluh:
    -- Currently we don't share the contract instance state with the main thread.
    -- Fix in memory store (turn it into simple filesystem store / contract instance per json file)
    contractStore <- case contractStoreConfig config' of
       UseSqliteStore cfg -> SqliteContractStore <$> App.dbConnect (toPABMsg ccaTrace) cfg
       UseFSStore dir     -> pure $ FSContractStore (ContractStoreDir dir)
       UseInMemoryStore   ->  InMemoryContractStore <$> initialInMemInstances @(Builtin a)

    -- Then, start the server
    result <- App.runApp contractStore (toPABMsg ccaTrace) contractHandler config
      $ do
          env <- ask @(Core.PABEnvironment (Builtin a) (App.AppEnv a))

          -- But first, spin up all the previous contracts
          logInfo @(LM.PABMultiAgentMsg (Builtin a)) LM.RestoringPABState
          case previousContracts of
            Left err -> throwError err
            Right ts -> do
                forM_ ts $ \(s, cid, args) -> do
                  action <- buildPABAction @a @(App.AppEnv a) s cid args
                  liftIO . async $ Core.runPAB' env action
                  pure ()
                logInfo @(LM.PABMultiAgentMsg (Builtin a)) (LM.PABStateRestored $ length ts)

          -- then, actually start the server.
          (mvar, _) <- PABServer.startServer pabWebserverConfig ccaAvailability
          liftIO $ takeMVar mvar
    either handleError return result
  where
    handleError err = do
        runStdoutLoggingT $ (logErrorN . tshow . pretty) err
        exitWith (ExitFailure 2)

-- Fork a list of commands
runConfigCommand contractHandler c@ConfigCommandArgs{ccaAvailability, ccaPABConfig=Config {nodeServerConfig} } (ForkCommands commands) =
    let shouldStartMocks = case pscNodeMode nodeServerConfig of
                             MockNode   -> True
                             AlonzoNode -> False
        startedCommands  = filter (mockedServices shouldStartMocks) commands
     in void $ do
          putStrLn $ "Starting all commands (" <> show startedCommands <> ")."
          threads <- traverse forkCommand startedCommands
          putStrLn $ "Started all commands (" <> show startedCommands <> ")."
          waitAny threads
  where
    mockedServices :: Bool -> ConfigCommand -> Bool
    mockedServices shouldStartMocks ChainIndex = shouldStartMocks
    mockedServices shouldStartMocks MockWallet = shouldStartMocks
    mockedServices _ _                         = True
    forkCommand :: ConfigCommand -> IO (Async ())
    forkCommand subcommand = do
      putStrLn $ "Starting: " <> show subcommand
      asyncId <- async . void . runConfigCommand contractHandler c $ subcommand
      putStrLn $ "Started: " <> show subcommand
      starting ccaAvailability
      pure asyncId

-- Run the chain-index service
runConfigCommand _ ConfigCommandArgs{ccaAvailability, ccaTrace, ccaPABConfig=Config { nodeServerConfig, chainIndexConfig }} ChainIndex =
    ChainIndex.main
        (toChainIndexLog ccaTrace)
        chainIndexConfig
        (pscSocketPath nodeServerConfig)
        (pscSlotConfig nodeServerConfig)
        ccaAvailability

-- Get the state of a contract
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig=Config{contractStoreConfig}} (ContractState contractInstanceId) =
  logPersistentStoreAction ccaTrace contractStoreConfig $ do
    s <- Contract.getState @(Builtin a) contractInstanceId
    pure $ Contract.serialisableState (Proxy @(Builtin a)) s

-- Get all available contracts
runConfigCommand _ ConfigCommandArgs{ccaTrace} ReportAvailableContracts = do
    runM
        $ interpret (App.handleContractDefinition @a)
        $ interpret (LM.handleLogMsgTrace ccaTrace)
        $ handleDelayEffect
        $ do
            availableContracts <- Contract.getDefinitions @(Builtin a)
            traverse_ (logInfo @(LM.AppMsg (Builtin a)) . LM.AvailableContract . render . pretty) availableContracts
            drainLog
                where
                    render = renderStrict . layoutPretty defaultLayoutOptions

-- Get all active contracts
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig=Config{contractStoreConfig}} ReportActiveContracts = do
  logPersistentStoreAction ccaTrace contractStoreConfig $ Contract.getActiveContracts @(Builtin a)

-- Get history of a specific contract
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig=Config{contractStoreConfig}} (ReportContractHistory contractInstanceId) =
  logPersistentStoreAction ccaTrace contractStoreConfig $ do
    s <- Contract.getState @(Builtin a) contractInstanceId
    let State.ContractResponse{State.newState=State{record}} = Contract.serialisableState (Proxy @(Builtin a)) s
    pure $ responses record

toChainIndexLog :: Trace m (LM.AppMsg (Builtin a)) -> Trace m LM.ChainIndexServerMsg
toChainIndexLog = LM.convertLog $ LM.PABMsg . LM.SChainIndexServerMsg

toWalletLog :: Trace m (LM.AppMsg (Builtin a)) -> Trace m WalletMsg
toWalletLog = LM.convertLog $ LM.PABMsg . LM.SWalletMsg

toMockNodeServerLog :: Trace m (LM.AppMsg (Builtin a)) -> Trace m LM.PABServerLogMsg
toMockNodeServerLog = LM.convertLog $ LM.PABMsg . LM.SMockserverLogMsg

-- | Wait for some time to allow all log messages to be printed to
--   the terminal.
drainLog :: Member DelayEffect effs => Eff effs ()
drainLog = delayThread (1 :: Second)

-- | Build a PAB Action that will run the provided context with the
-- reconstructed state.
buildPABAction ::
    forall a env effs.
    ( LastMember IO effs
    )
    => SomeBuiltinState a
    -> Wallet.ContractInstanceId
    -> ContractActivationArgs a
    -> Eff effs (Core.PABAction (Builtin a) env Wallet.ContractInstanceId)
buildPABAction currentState cid ContractActivationArgs{caWallet, caID} = do
    let r = getResponse currentState

    -- Bring up the STM state
    stmState :: InstanceState <- liftIO $ STM.atomically emptyInstanceState
    void $ runReader stmState $ updateState @IO r

    -- Squish it into a PAB action which we will run
    let ciState = ContractInstanceState currentState (pure stmState)
        wallet = fromMaybe (Wallet.knownWallet 1) caWallet
    pure $ Core.activateContract' @(Builtin a) ciState cid wallet caID
