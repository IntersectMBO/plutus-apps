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

module Plutus.PAB.Run.Cli (ConfigCommandArgs(..), runConfigCommand) where

-----------------------------------------------------------------------------------------------------------------------
-- Command interpretation
-----------------------------------------------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Cardano.Api.NetworkId.Extra (NetworkIdWrapper (..))
import Cardano.BM.Configuration (Configuration)
import Cardano.BM.Data.Trace (Trace)
import Cardano.ChainIndex.Server qualified as ChainIndex
import Cardano.Node.Params qualified as Params
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
import Control.Monad.Freer.Extras.Log (logInfo)
import Control.Monad.Freer.Reader (ask, runReader)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logErrorN, runStdoutLoggingT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (traverse_)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.OpenApi.Schema qualified as OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Text.Extras (tshow)
import Data.Time.Units (Second)
import Plutus.Contract.Resumable (responses)
import Plutus.Contract.State (State (State, record))
import Plutus.Contract.State qualified as State
import Plutus.PAB.App qualified as App
import Plutus.PAB.Core qualified as Core
import Plutus.PAB.Core.ContractInstance (ContractInstanceState (ContractInstanceState), updateState)
import Plutus.PAB.Core.ContractInstance.STM (InstanceState, emptyInstanceState)
import Plutus.PAB.Db.Beam qualified as Beam
import Plutus.PAB.Effects.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler, HasDefinitions, SomeBuiltinState, getResponse)
import Plutus.PAB.Monitoring.Monitoring qualified as LM
import Plutus.PAB.Run.Command (ConfigCommand (ChainIndex, ContractState, ForkCommands, Migrate, MockWallet, PABWebserver, ReportActiveContracts, ReportAvailableContracts, ReportContractHistory, StartNode))
import Plutus.PAB.Types (Config (Config, dbConfig, pabWebserverConfig), chainIndexConfig, nodeServerConfig,
                         walletServerConfig)
import Plutus.PAB.Webserver.Server qualified as PABServer
import Plutus.PAB.Webserver.Types (ContractActivationArgs (ContractActivationArgs, caID, caWallet))
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)
import Servant qualified
import System.Exit (ExitCode (ExitFailure), exitWith)
import Wallet.Emulator.Wallet qualified as Wallet
import Wallet.Types qualified as Wallet

data ConfigCommandArgs a =
    ConfigCommandArgs
        { ccaTrace          :: Trace IO (LM.AppMsg (Builtin a))  -- ^ PAB Tracer logging instance
        , ccaLoggingConfig  :: Configuration -- ^ Monitoring configuration
        , ccaPABConfig      :: Config        -- ^ PAB Configuration
        , ccaAvailability   :: Availability  -- ^ Token for signaling service availability
        , ccaStorageBackend :: App.StorageBackend -- ^ Wheter to use the beam-sqlite or in-memory backend
        }

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
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig=Config{dbConfig}} Migrate =
    App.migrate (toPABMsg ccaTrace) dbConfig

-- Run mock wallet service
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig = Config {nodeServerConfig, chainIndexConfig, walletServerConfig = LocalWalletConfig ws},ccaAvailability} MockWallet = do
    params <- liftIO $ Params.fromPABServerConfig nodeServerConfig
    liftIO $ WalletServer.main
        (toWalletLog ccaTrace)
        ws
        (pscSocketPath nodeServerConfig)
        params
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
                          config@Config { pabWebserverConfig, nodeServerConfig, dbConfig }
                      , ccaAvailability, ccaStorageBackend
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

    connection <- App.dbConnect (LM.convertLog LM.PABMsg ccaTrace) dbConfig
    -- Restore the running contracts by first collecting up enough details about the
    -- previous contracts to re-start them
    previousContracts <-
        Beam.runBeamStoreAction connection (LM.convertLog LM.PABMsg ccaTrace)
        $ interpret (LM.handleLogMsgTrace ccaTrace)
        $ do
            cIds   <- Map.toList <$> Contract.getActiveContracts @(Builtin a)
            forM cIds $ \(cid, args) -> do
                s <- Contract.getState @(Builtin a) cid
                let priorContract :: (SomeBuiltinState a, Wallet.ContractInstanceId, ContractActivationArgs a)
                    priorContract = (s, cid, args)
                pure priorContract

    -- Then, start the server
    result <- App.runApp ccaStorageBackend (toPABMsg ccaTrace) contractHandler config
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
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig=Config{dbConfig}} (ContractState contractInstanceId) = do
    connection <- App.dbConnect (LM.convertLog LM.PABMsg ccaTrace) dbConfig
    fmap (either (error . show) id)
        $ Beam.runBeamStoreAction connection (LM.convertLog LM.PABMsg ccaTrace)
        $ interpret (LM.handleLogMsgTrace ccaTrace)
        $ do
            s <- Contract.getState @(Builtin a) contractInstanceId
            let outputState = Contract.serialisableState (Proxy @(Builtin a)) s
            logInfo @(LM.AppMsg (Builtin a)) $ LM.PABMsg $ LM.SCoreMsg $ LM.FoundContract $ Just outputState
            drainLog

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
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig=Config{dbConfig}} ReportActiveContracts = do
    connection <- App.dbConnect (LM.convertLog LM.PABMsg ccaTrace) dbConfig
    fmap (either (error . show) id)
        $ Beam.runBeamStoreAction connection (LM.convertLog LM.PABMsg ccaTrace)
        $ interpret (LM.handleLogMsgTrace ccaTrace)
        $ do
            logInfo @(LM.AppMsg (Builtin a)) LM.ActiveContractsMsg
            instancesById <- Contract.getActiveContracts @(Builtin a)
            let idsByDefinition = Map.fromListWith (<>) $ fmap (\(inst, ContractActivationArgs{caID}) -> (caID, Set.singleton inst)) $ Map.toList instancesById
            traverse_ (\(e, s) -> logInfo @(LM.AppMsg (Builtin a)) $ LM.ContractInstances e (Set.toList s)) $ Map.toAscList idsByDefinition
            drainLog

-- Get history of a specific contract
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig=Config{dbConfig}} (ReportContractHistory contractInstanceId) = do
    connection <- App.dbConnect (LM.convertLog LM.PABMsg ccaTrace) dbConfig
    fmap (either (error . show) id)
        $ Beam.runBeamStoreAction connection (LM.convertLog LM.PABMsg ccaTrace)
        $ interpret (LM.handleLogMsgTrace ccaTrace)
        $ do
            logInfo @(LM.AppMsg (Builtin a)) LM.ContractHistoryMsg
            s <- Contract.getState @(Builtin a) contractInstanceId
            let State.ContractResponse{State.newState=State{record}} = Contract.serialisableState (Proxy @(Builtin a)) s
            traverse_ logStep (responses record)
            drainLog
  where
      logStep response = logInfo @(LM.AppMsg (Builtin a)) $
          LM.ContractHistoryItem contractInstanceId (snd <$> response)

toPABMsg :: Trace m (LM.AppMsg (Builtin a)) -> Trace m (LM.PABLogMsg (Builtin a))
toPABMsg = LM.convertLog LM.PABMsg

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

