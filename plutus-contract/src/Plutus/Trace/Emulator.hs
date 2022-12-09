{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-

An emulator trace is a contract trace that can be run in the Plutus emulator.

-}
module Plutus.Trace.Emulator(
    Emulator
    , EmulatorTrace
    , EmulatorEffects
    , BaseEmulatorEffects
    , Wallet.Emulator.Stream.EmulatorErr(..)
    , Plutus.Trace.Emulator.Types.ContractHandle(..)
    , ContractInstanceTag
    , ContractConstraints
    -- * Constructing Traces
    , Assert.assert
    , RunContract.activateContract
    , RunContract.activateContractWallet
    , RunContract.walletInstanceTag
    , RunContract.callEndpoint
    , RunContract.getContractState
    , RunContract.observableState
    , RunContract.activeEndpoints
    , EmulatedWalletAPI.liftWallet
    , EmulatedWalletAPI.payToWallet
    , Waiting.nextSlot
    , Waiting.waitUntilSlot
    , Waiting.waitUntilTime
    , Waiting.waitNSlots
    , Waiting.waitNMilliSeconds
    , EmulatorControl.freezeContractInstance
    , EmulatorControl.thawContractInstance
    -- ** Inspecting the chain state
    , EmulatorControl.setSigningProcess
    , EmulatorControl.chainState
    , EmulatorControl.getSlotConfig
    , ChainState.chainNewestFirst
    , ChainState.txPool
    , ChainState.index
    , ChainState.chainCurrentSlot
    -- ** Inspecting the agent states
    , EmulatorControl.agentState
    , Wallet.ownPaymentPrivateKey
    , Wallet.nodeClient
    , Wallet.signingProcess
    -- * Throwing errors
    , throwError
    , EmulatorRuntimeError(..)
    -- * Running traces
    , EmulatorConfig(..)
    , initialChainState
    , params
    , runEmulatorStream
    , TraceConfig(..)
    , traceConfigShowEventExample
    , runEmulatorTrace
    , evalEmulatorTrace
    , PrintEffect(..)
    , runEmulatorTraceEff
    , runEmulatorTraceIO
    , runEmulatorTraceIO'
    -- * Interpreter
    , interpretEmulatorTrace
    ) where

import Cardano.Api qualified as C
import Cardano.Node.Emulator.Chain (ChainControlEffect)
import Cardano.Node.Emulator.Chain qualified as ChainState
import Cardano.Node.Emulator.Params (Params (..))
import Control.Foldl (generalize, list)
import Control.Lens hiding ((:>))
import Control.Monad (forM_, void)
import Control.Monad.Freer (Eff, Member, interpret, interpretM, raise, reinterpret, run, runM, subsume)
import Control.Monad.Freer.Coroutine (Yield)
import Control.Monad.Freer.Error (Error, handleError, throwError)
import Control.Monad.Freer.Extras.Log (LogLevel (Info), LogMessage (LogMessage), LogMsg, mapLog)
import Control.Monad.Freer.Extras.Modify (raiseEnd)
import Control.Monad.Freer.Reader (Reader)
import Control.Monad.Freer.State (State, evalState)
import Control.Monad.Freer.TH (makeEffect)
import Data.Aeson qualified as A
import Data.Default (Default (def))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Ledger.CardanoWallet qualified as CW
import Ledger.Slot (getSlot)
import Plutus.Trace.Effects.Assert (Assert, handleAssert)
import Plutus.Trace.Effects.Assert qualified as Assert
import Plutus.Trace.Effects.ContractInstanceId (ContractInstanceIdEff, handleDeterministicIds)
import Plutus.Trace.Effects.EmulatedWalletAPI (EmulatedWalletAPI, handleEmulatedWalletAPI)
import Plutus.Trace.Effects.EmulatedWalletAPI qualified as EmulatedWalletAPI
import Plutus.Trace.Effects.EmulatorControl (EmulatorControl, handleEmulatorControl)
import Plutus.Trace.Effects.EmulatorControl qualified as EmulatorControl
import Plutus.Trace.Effects.RunContract (RunContract, StartContract, handleRunContract, handleStartContract)
import Plutus.Trace.Effects.RunContract qualified as RunContract
import Plutus.Trace.Effects.Waiting (Waiting, handleWaiting)
import Plutus.Trace.Effects.Waiting qualified as Waiting
import Plutus.Trace.Emulator.System (launchSystemThreads)
import Plutus.Trace.Emulator.Types (ContractConstraints, ContractInstanceLog (ContractInstanceLog),
                                    ContractInstanceMsg (ContractLog, CurrentRequests, HandledRequest, NoRequestsHandled, StoppedWithError),
                                    ContractInstanceTag, Emulator, EmulatorMessage,
                                    EmulatorRuntimeError (EmulatedWalletError), EmulatorThreads,
                                    UserThreadMsg (UserLog))
import Plutus.Trace.Emulator.Types qualified
import Plutus.Trace.Scheduler (EmSystemCall, ThreadId, exit, runThreads)
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.String (renderString)
import Prettyprinter.Render.Text (renderStrict)
import Streaming (Stream)
import Streaming.Prelude (Of ((:>)))
import System.IO (Handle, hPutStrLn, stdout)
import Wallet.Emulator.MultiAgent (EmulatorEvent,
                                   EmulatorEvent' (InstanceEvent, SchedulerEvent, UserThreadEvent, WalletEvent),
                                   EmulatorState (_chainState, _walletStates), EmulatorTimeEvent (EmulatorTimeEvent),
                                   MultiAgentControlEffect, MultiAgentEffect, schedulerEvent)
import Wallet.Emulator.Stream (EmulatorConfig (_initialChainState, _params), EmulatorErr, filterLogLevel, foldStreamM,
                               initialChainState, params, runTraceStream)
import Wallet.Emulator.Stream qualified
import Wallet.Emulator.Wallet (Entity, balances)
import Wallet.Emulator.Wallet qualified as Wallet

-- | A very simple effect for interpreting the output printing done by the
-- trace printing functions:
--
-- * 'runEmulatorTraceEff'
-- * 'runEmulatorTraceIO'
-- * 'runEmulatorTraceIOWithConfig'
data PrintEffect r where
  PrintLn :: String -> PrintEffect ()
makeEffect ''PrintEffect


type EmulatorEffects = StartContract
                    ': BaseEmulatorEffects

type BaseEmulatorEffects =
             [ RunContract
             , Assert
             , Waiting
             , EmulatorControl
             , EmulatedWalletAPI
             , LogMsg String
             , Error EmulatorRuntimeError
             ]

type EmulatorTrace = Eff EmulatorEffects

handleEmulatorTrace ::
    forall effs a.
    ( Member MultiAgentEffect effs
    , Member MultiAgentControlEffect effs
    , Member (State EmulatorThreads) effs
    , Member (State EmulatorState) effs
    , Member (Error EmulatorRuntimeError) effs
    , Member (LogMsg EmulatorEvent') effs
    , Member ContractInstanceIdEff effs
    )
    => Params
    -> EmulatorTrace a
    -> Eff (Reader ThreadId ': Yield (EmSystemCall effs EmulatorMessage a) (Maybe EmulatorMessage) ': effs) ()
handleEmulatorTrace params@Params{pNetworkId, pSlotConfig} action = do
    result <- subsume @(Error EmulatorRuntimeError)
            . interpret (mapLog (UserThreadEvent . UserLog))
            . flip handleError (throwError . EmulatedWalletError)
            . reinterpret handleEmulatedWalletAPI
            . interpret (handleEmulatorControl @_ @effs @a params)
            . interpret (handleWaiting @_ @effs @a pSlotConfig)
            . interpret (handleAssert @_ @effs @a)
            . interpret (handleRunContract @_ @effs @a)
            . interpret (handleStartContract @_ @effs @a pNetworkId)
            $ raiseEnd action
    void $ exit @effs @EmulatorMessage result

-- | Run a 'Trace Emulator', streaming the log messages as they arrive
runEmulatorStream :: forall effs a.
    EmulatorConfig
    -> EmulatorTrace a
    -> Stream (Of (LogMessage EmulatorEvent)) (Eff effs) (Either EmulatorErr a, EmulatorState)
runEmulatorStream conf = runTraceStream conf . interpretEmulatorTrace conf

-- | Interpret a 'Trace Emulator' action in the multi agent and emulated
--   blockchain effects.
interpretEmulatorTrace :: forall effs a.
    ( Member MultiAgentEffect effs
    , Member MultiAgentControlEffect effs
    , Member (Error EmulatorRuntimeError) effs
    , Member ChainControlEffect effs
    , Member (LogMsg EmulatorEvent') effs
    , Member (State EmulatorState) effs
    )
    => EmulatorConfig
    -> EmulatorTrace a
    -> Eff effs (Maybe a)
interpretEmulatorTrace conf action =
    -- add a wait action to the beginning to ensure that the
    -- initial transaction gets validated before the wallets
    -- try to spend their funds
    let action' = do
          void Waiting.nextSlot
          res <- action
          void Waiting.nextSlot
          pure res
        wallets = fromMaybe (Wallet.toMockWallet <$> CW.knownMockWallets) (preview (initialChainState . _Left . to Map.keys) conf)
    in
    evalState @EmulatorThreads mempty
        $ handleDeterministicIds
        $ interpret (mapLog (review schedulerEvent))
        $ runThreads
        $ do
            raise $ launchSystemThreads wallets
            handleEmulatorTrace (_params conf) action'

-- | Options for how to set up and print the trace.
data TraceConfig = TraceConfig
  { traceConfigShowEvent    :: LogMessage EmulatorEvent -> Maybe String
  -- ^ Function to decide how to print the particular events.
  , traceConfigOutputHandle :: Handle
  -- ^ Where to print the outputs to. Default: 'System.IO.stdout'
  , traceConfigMinLogLevel  :: LogLevel
  }

instance Default TraceConfig where
  def = TraceConfig
      { traceConfigShowEvent = Just
                             . Text.unpack
                             . renderStrict
                             . layoutPretty defaultLayoutOptions
                             . pretty
      , traceConfigOutputHandle = stdout
      , traceConfigMinLogLevel = Info
      }

-- | Some example of how to configure the 'traceConfigShowEvent'.
traceConfigShowEventExample :: LogMessage EmulatorEvent -> Maybe String
traceConfigShowEventExample (LogMessage _minLogLevel (EmulatorTimeEvent slot e)) =
    let logMsgMaybe = case e of
          UserThreadEvent (UserLog msg) ->
              Just $ "*** USER LOG: " <> msg
          InstanceEvent (ContractInstanceLog (ContractLog (A.String msg)) _ _) ->
              Just $ "*** CONTRACT LOG: " <> show msg
          InstanceEvent (ContractInstanceLog (StoppedWithError err)       _ _) ->
              Just $ "*** CONTRACT STOPPED WITH ERROR: " <> show err
          InstanceEvent (ContractInstanceLog NoRequestsHandled            _ _) ->
              Nothing
          InstanceEvent (ContractInstanceLog (HandledRequest _)           _ _) ->
              Nothing
          InstanceEvent (ContractInstanceLog (CurrentRequests _)          _ _) ->
              Nothing
          SchedulerEvent _ ->
              Nothing
          WalletEvent _ _ ->
              Nothing
          ev ->
              Just . renderString . layoutPretty defaultLayoutOptions . pretty $ ev
        paddedSlotNo = pad 5 (getSlot slot)
     in fmap (\m -> "Slot " <> paddedSlotNo <> ": " <> m) logMsgMaybe
 where
    pad :: Int -> Integer -> String
    pad n = (\x -> replicate (n - length x) '0' ++ x) . show

evalEmulatorTrace
    :: TraceConfig
    -> EmulatorConfig
    -> EmulatorTrace a
    -> Either EmulatorErr a
evalEmulatorTrace tcfg cfg trace = case runEmulatorTrace tcfg cfg trace of
  (_, r, _) -> r

-- | Run an emulator trace to completion, returning a tuple of the final state
-- of the emulator, the events, and any error, if any.
runEmulatorTrace
    :: TraceConfig
    -> EmulatorConfig
    -> EmulatorTrace a
    -> ([LogMessage EmulatorEvent], Either EmulatorErr a, EmulatorState)
runEmulatorTrace TraceConfig { traceConfigMinLogLevel } cfg trace =
    (\(xs :> (y, z)) -> (xs, y, z))
    $ run
    $ foldStreamM (generalize list)
    $ filterLogLevel traceConfigMinLogLevel
    $ runEmulatorStream cfg trace

-- | Run the emulator trace returning an effect that can be evaluated by
-- interpreting the 'PrintEffect's.
runEmulatorTraceEff :: forall effs. Member PrintEffect effs
    => TraceConfig
    -> EmulatorConfig
    -> EmulatorTrace ()
    -> Eff effs ()
runEmulatorTraceEff tcfg cfg trace =
  let (xs, me, e) = runEmulatorTrace tcfg cfg trace
      balances' = balances (_chainState e) (_walletStates e)
   in do
      case me of
        Right _  -> return ()
        Left err -> printLn $ "ERROR: " <> show err

      forM_ xs $ \ete -> do
        case traceConfigShowEvent tcfg ete of
          Nothing -> pure ()
          Just s  -> printLn s

      printLn "Final balances"
      printBalances balances'

-- | Runs the trace with 'runEmulatorTrace', with default configuration that
-- prints a selection of events to stdout.
--
-- Example:
--
-- >>> runEmulatorTraceIO (void $ Trace.waitNSlots 1)
runEmulatorTraceIO
    :: EmulatorTrace ()
    -> IO ()
runEmulatorTraceIO = runEmulatorTraceIO' def def

-- | Runs the trace with a given configuration for the trace and the config.
--
-- Example of running a trace and saving the output to a file:
--
-- >>> withFile "/tmp/trace-log.txt" WriteMode $ \h -> runEmulatorTraceIO' (def { traceConfigOutputHandle = h }) def (void $ Trace.waitNSlots 1)
runEmulatorTraceIOWithConfig
    :: TraceConfig
    -> EmulatorConfig
    -> EmulatorTrace ()
    -> IO ()
runEmulatorTraceIOWithConfig tcfg cfg trace
  = runPrintEffectIO (traceConfigOutputHandle tcfg) $ runEmulatorTraceEff tcfg cfg trace

{-# DEPRECATED runEmulatorTraceIO' "Renamed to runEmulatorTraceIOWithConfig" #-}
runEmulatorTraceIO'
    :: TraceConfig
    -> EmulatorConfig
    -> EmulatorTrace ()
    -> IO ()
runEmulatorTraceIO' = runEmulatorTraceIOWithConfig

runPrintEffectIO
    :: Handle
    -> Eff '[PrintEffect, IO] r
    -> IO r
runPrintEffectIO hdl = runM . interpretM f
  where
    f :: PrintEffect r -> IO r
    f = \case
      PrintLn s -> hPutStrLn hdl s

printBalances :: forall effs. Member PrintEffect effs
              => Map.Map Entity C.Value
              -> Eff effs ()
printBalances m = do
    forM_ (Map.toList m) $ \(e, v) -> do
        printLn $ show e <> ": " <> Text.unpack (C.renderValuePretty v)
