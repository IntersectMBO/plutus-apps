{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}
-- | A state machine with two states and two roles that take turns.
module Plutus.Contracts.PingPong(
    PingPongState(..),
    Input(..),
    PingPongError(..),
    PingPongSchema,
    runPing,
    runPong,
    ping,
    pong,
    initialise,
    runStop,
    runWaitForUpdate,
    combined,
    simplePingPong,
    simplePingPongAuto
    ) where

import Control.Lens
import Control.Monad (forever, void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid (Last (..))
import GHC.Generics (Generic)
import Ledger.Ada qualified as Ada
import Ledger.Constraints (TxConstraints)
import Ledger.Params qualified as P
import Ledger.Typed.Scripts qualified as Scripts
import PlutusTx qualified
import PlutusTx.Prelude hiding (Applicative (..), check)

import Plutus.Contract
import Plutus.Contract.StateMachine (AsSMContractError (..), OnChainState, State (..), Void)
import Plutus.Contract.StateMachine qualified as SM
import Prelude qualified as Haskell

data PingPongState = Pinged | Ponged | Stopped
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Eq PingPongState where
    Pinged == Pinged = True
    Ponged == Ponged = True
    _ == _           = False

data Input = Ping | Pong | Stop
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

type PingPongSchema =
        Endpoint "initialise" ()
        .\/ Endpoint "ping" ()
        .\/ Endpoint "pong" ()
        .\/ Endpoint "stop" () -- Transition the state machine instance to the final state
        .\/ Endpoint "wait" () -- Wait for a change to the on-chain state of the machine

data PingPongError =
    PingPongContractError ContractError
    | PingPongSMError SM.SMContractError
    | StoppedUnexpectedly
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''PingPongError

instance AsSMContractError PingPongError where
    _SMContractError = _PingPongSMError

instance AsContractError PingPongError where
    _ContractError = _PingPongContractError

{-# INLINABLE transition #-}
transition :: State PingPongState -> Input -> Maybe (TxConstraints Void Void, State PingPongState)
transition State{stateData=oldData,stateValue} input = case (oldData, input) of
    (_,      Stop) -> Just (mempty, State{stateData=Stopped, stateValue=mempty})
    (Pinged, Pong) -> Just (mempty, State{stateData=Ponged, stateValue})
    (Ponged, Ping) -> Just (mempty, State{stateData=Pinged, stateValue})
    _              -> Nothing

{-# INLINABLE machine #-}
machine :: SM.StateMachine PingPongState Input
machine = SM.mkStateMachine Nothing transition isFinal where
    isFinal Stopped = True
    isFinal _       = False

{-# INLINABLE mkValidator #-}
mkValidator :: Scripts.ValidatorType (SM.StateMachine PingPongState Input)
mkValidator = SM.mkValidator machine

typedValidator :: Scripts.TypedValidator (SM.StateMachine PingPongState Input)
typedValidator = Scripts.mkTypedValidator @(SM.StateMachine PingPongState Input)
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator @PingPongState @Input

machineInstance :: SM.StateMachineInstance PingPongState Input
machineInstance = SM.StateMachineInstance machine typedValidator

client :: SM.StateMachineClient PingPongState Input
client = SM.mkStateMachineClient machineInstance

initialise :: forall w. P.Params -> Promise w PingPongSchema PingPongError ()
initialise cfg = endpoint @"initialise" $ \() -> void $ SM.runInitialise cfg client Pinged (Ada.lovelaceValueOf 1)

run ::
    forall w.
    PingPongState
    -> Promise w PingPongSchema PingPongError ()
    -> Contract w PingPongSchema PingPongError ()
run expectedState action = do
    let go Nothing = throwError StoppedUnexpectedly
        go (Just currentState)
            | SM.getStateData currentState == expectedState = awaitPromise action
            | otherwise = runWaitForUpdate >>= go
    maybeState <- SM.getOnChainState client
    let datum = fmap fst maybeState
    go datum

runPing :: forall w. P.Params -> Contract w PingPongSchema PingPongError ()
runPing cfg = run Ponged (ping cfg)

ping :: forall w. P.Params -> Promise w PingPongSchema PingPongError ()
ping cfg = endpoint @"ping" $ \() -> void (SM.runStep cfg client Ping)

runPong :: forall w. P.Params -> Contract w PingPongSchema PingPongError ()
runPong cfg = run Pinged (pong cfg)

pong :: forall w. P.Params -> Promise w PingPongSchema PingPongError ()
pong cfg = endpoint @"pong" $ \() -> void (SM.runStep cfg client Pong)

runStop :: forall w. P.Params -> Promise w PingPongSchema PingPongError ()
runStop cfg = endpoint @"stop" $ \() -> void (SM.runStep cfg client Stop)

runWaitForUpdate :: forall w. Contract w PingPongSchema PingPongError (Maybe (OnChainState PingPongState Input))
runWaitForUpdate = SM.waitForUpdate client

combined :: P.Params -> Contract (Last PingPongState) PingPongSchema PingPongError ()
combined cfg = forever (selectList [initialise cfg, ping cfg, pong cfg, runStop cfg, wait]) where
    wait = endpoint @"wait" $ \() -> do
        logInfo @Haskell.String "runWaitForUpdate"
        newState <- runWaitForUpdate
        case newState of
            Nothing -> logWarn @Haskell.String "runWaitForUpdate: Nothing"
            Just ocs -> do
                logInfo $ "new state: " <> Haskell.show (SM.getStateData ocs)
                tell (Last $ Just $ SM.getStateData ocs)

simplePingPongAuto :: P.Params -> Contract (Last PingPongState) PingPongSchema PingPongError ()
simplePingPongAuto cfg = do
  logInfo @Haskell.String "Initialising PingPongAuto"
  void $ SM.runInitialise cfg client Pinged (Ada.lovelaceValueOf 2)
  logInfo @Haskell.String "Waiting for PONG"
  awaitPromise $ pong cfg
  logInfo @Haskell.String "Waiting for PING"
  awaitPromise $ ping cfg
  logInfo @Haskell.String "Waiting for PONG"
  awaitPromise $ pong cfg

simplePingPong :: P.Params -> Contract (Last PingPongState) PingPongSchema PingPongError ()
simplePingPong cfg =
  awaitPromise (initialise cfg)
  >> awaitPromise (pong cfg)
  >> awaitPromise (ping cfg)
  >> awaitPromise (pong cfg)

PlutusTx.unstableMakeIsData ''PingPongState
PlutusTx.makeLift ''PingPongState
PlutusTx.unstableMakeIsData ''Input
PlutusTx.makeLift ''Input
