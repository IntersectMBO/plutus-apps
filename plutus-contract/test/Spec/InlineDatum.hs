{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}


module Spec.InlineDatum where

import PlutusTx.Prelude hiding (Eq)
import Prelude (Show, String, show)

import Control.Monad (void)
import GHC.Generics (Generic)
import Ledger.Typed.Scripts (TypedValidator)
import Plutus.Contract (Contract, EmptySchema, logError, mapError)
import Plutus.Contract.StateMachine (StateMachine, StateMachineClient, ThreadToken, mkStateMachine, stateData)
import Plutus.Contract.StateMachine qualified as SM
import Plutus.Contract.Test
import Plutus.Script.Utils.Typed (ScriptContextV2)
import Plutus.Script.Utils.V1.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.Trace (EmulatorTrace, activateContractWallet)
import Plutus.Trace qualified as Trace
import PlutusTx qualified
import Test.Tasty

-- * Very simple plutus state machine using a thread token

data State
  = First
  | Second
  deriving (Generic, Show)

PlutusTx.makeLift ''State
PlutusTx.unstableMakeIsData ''State

data Input
  = Step
  deriving (Generic, Show)
PlutusTx.makeLift ''Input
PlutusTx.unstableMakeIsData ''Input

{-# INLINEABLE transition #-}
transition :: SM.State State -> Input -> Maybe (SM.TxConstraints SM.Void SM.Void, SM.State State)
transition oldState _ = Just (mempty, oldState{stateData = Second})

{-# INLINEABLE stateMachine #-}
stateMachine :: ThreadToken -> StateMachine State Input
stateMachine threadToken =
  mkStateMachine (Just threadToken) transition isFinal True
 where
  isFinal = const False

typedValidator :: ThreadToken -> TypedValidator (StateMachine State Input)
typedValidator threadToken =
  V2.mkTypedValidator @(StateMachine State Input)
    ($$(PlutusTx.compile [||validator||]) `PlutusTx.applyCode` PlutusTx.liftCode threadToken)
    $$(PlutusTx.compile [||wrap||])
 where
  validator c = SM.mkValidator (stateMachine c)
  wrap = Scripts.mkUntypedValidator @ScriptContextV2 @State @Input

stateMachineClient :: ThreadToken -> StateMachineClient State Input
stateMachineClient threadToken =
  let machine = stateMachine threadToken
      inst = typedValidator threadToken
   in SM.mkStateMachineClient (SM.StateMachineInstance machine inst)

-- * Minimal test runner for repro

contract :: Contract () EmptySchema String ()
contract = do
  threadToken <- mapSMError SM.getThreadToken
  logError @String $ "Forged thread token: " <> show threadToken

  let client = stateMachineClient threadToken
  void $ mapSMError $ SM.runInitialise client First mempty
  logError @String $ "Initialized state machine"

  res <- mapSMError $ SM.runStep client Step
  case res of
    SM.TransitionFailure (SM.InvalidTransition os i) -> logError @String $ "Invalid transition: " <> show (os, i)
    SM.TransitionSuccess s                           -> logError @String $ "Transition success: " <> show s
 where
  mapSMError = mapError (show @SM.SMContractError)

testTrace :: EmulatorTrace ()
testTrace = do
  void $ activateContractWallet w1 contract
  void $ Trace.waitNSlots 10

tests :: TestTree
tests = testGroup "Inline Datum"
    [ checkPredicate "Runs successfully"
        (assertDone contract (Trace.walletInstanceTag w1) (const True) "No errors"
         .&&. assertNoFailedTransactions)
        testTrace
    ]
