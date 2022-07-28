{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE BlockArguments        #-}
module Spec.Contract.TxConstraints (tests) where

import Control.Lens hiding ((.>))
import Control.Monad (forever, replicateM_, void)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (catchError)
import Control.Monad.Freer.Extras.Log (LogLevel (Debug))
import Control.Monad.Freer.Extras.Log qualified as Log
import Data.Functor.Apply ((.>))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)

import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Tx (getCardanoTxId)
import Plutus.Contract as Con hiding (collectFromScript)
import Plutus.Contract.State qualified as State
import Plutus.Contract.Test (Shrinking (DoShrink, DontShrink), TracePredicate, assertAccumState, assertContractError,
                             assertDone, assertInstanceLog, assertNoFailedTransactions, assertResumableResult,
                             assertUserLog, checkEmulatorFails, checkPredicate, checkPredicateOptions,
                             defaultCheckOptions, endpointAvailable, minLogLevel, mockWalletPaymentPubKeyHash, not,
                             valueAtAddress, w1, w2, w3, waitingForSlot, walletFundsChange, (.&&.))
import Plutus.Contract.Tx (collectFromScript)
import Plutus.Contract.Types (ResumableResult (ResumableResult, _finalState), responses)
import Plutus.Contract.Util (loopM)
import Plutus.Script.Utils.V1.Address (mkValidatorAddress)
import Plutus.Script.Utils.V1.Scripts (datumHash)
import Plutus.Script.Utils.V1.Typed.TypeUtils (Any)
import Plutus.Trace qualified as Trace
import Plutus.Trace.Emulator (ContractInstanceTag, EmulatorTrace, activateContract, activeEndpoints, callEndpoint)
import Plutus.Trace.Emulator qualified as Trace
import Plutus.Trace.Emulator.Types (ContractInstanceLog (_cilMessage),
                                    ContractInstanceMsg (ContractLog, CurrentRequests, HandledRequest, ReceiveEndpointCall, Started, StoppedNoError),
                                    ContractInstanceState (ContractInstanceState, instContractState),
                                    UserThreadMsg (UserLog))
import Plutus.V1.Ledger.Api (Address, Datum (Datum), DatumHash, TxOutRef, Validator)
import Plutus.V1.Ledger.Tx (TxOut (txOutDatumHash))
import PlutusTx qualified
import Prelude hiding (not)
import Wallet.Emulator qualified as EM
import Wallet.Emulator.Wallet (mockWalletAddress)

import Data.List.NonEmpty qualified as NE
import Ledger.Constraints qualified as TC
import Ledger.Constraints.OffChain.V2 qualified as TC
import Plutus.ChainIndex.Types (RollbackState (Committed), TxOutState (Spent, Unspent), TxOutStatus, TxStatus,
                                TxValidity (TxValid))
import Plutus.Contract.Effects (ActiveEndpoint (ActiveEndpoint, aeDescription, aeMetadata))
import Plutus.Script.Utils.V1.Address qualified as PV1
import Plutus.Script.Utils.V1.Typed.Scripts qualified as PV1
import Plutus.Script.Utils.V2.Address qualified as PV2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as PV2
import Plutus.V1.Ledger.Address qualified as Addr
import Plutus.V1.Ledger.Api qualified as PV1
import Plutus.V2.Ledger.Api qualified as PV2

run :: String -> TracePredicate -> EmulatorTrace () -> TestTree
run = checkPredicateOptions (defaultCheckOptions & minLogLevel .~ Debug)

check :: String -> Contract () EmptySchema ContractError () -> _ -> TestTree
check nm contract pred = run nm (pred contract) (void $ activateContract w1 contract tag)

tag :: ContractInstanceTag
tag = "instance 1"

tests :: TestTree
tests = testGroup "contract tx constraints"
    [ checkPredicate "mustReferencePubKeyOutput returns False on-chain when used for unlocking funds in a PlutusV1 script"
        (walletFundsChange w1 (Ada.adaValueOf (-10))
        .&&. valueAtAddress mustReferencePubKeyOutputV1ValidatorAddress (== Ada.adaValueOf 10)
        ) $ do
            void $ activateContract w1 mustReferencePubKeyOutputV1ConTest tag
            void $ Trace.waitNSlots 1

    , checkPredicate "mustReferencePubKeyOutput can be used on-chain to unlock funds in a PlutusV2 script"
        (walletFundsChange w1 (Ada.adaValueOf 0)
        .&&. valueAtAddress mustReferencePubKeyOutputV2ValidatorAddress (== Ada.adaValueOf 0)
        ) $ do
            void $ activateContract w1 mustReferencePubKeyOutputV2ConTest tag
            void $ Trace.waitNSlots 1
    ]

{-
Test case: mustReferencePubKeyOutput returns False on-chain when used for unlocking funds in a PlutusV1 script"
-}

{-# INLINABLE mkMustReferencePubKeyOutputV1Validator #-}
mkMustReferencePubKeyOutputV1Validator :: TxOutRef -> () -> PV1.ScriptContext -> Bool
mkMustReferencePubKeyOutputV1Validator txOutRef _ =
    TC.checkV1ScriptContext @Void @Void (TC.mustReferencePubKeyOutput txOutRef)

{-# INLINABLE mustReferencePubKeyOutputV1Validator #-}
mustReferencePubKeyOutputV1Validator :: PV1.Validator
mustReferencePubKeyOutputV1Validator = PV1.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = PV1.mkUntypedValidator mkMustReferencePubKeyOutputV1Validator

mustReferencePubKeyOutputV1ValidatorAddress :: Address
mustReferencePubKeyOutputV1ValidatorAddress =
    PV1.mkValidatorAddress mustReferencePubKeyOutputV1Validator

mustReferencePubKeyOutputV1ConTest :: Contract () EmptySchema ContractError ()
mustReferencePubKeyOutputV1ConTest = do
    -- Locking some Ada in the script address
    (utxoRef, utxo) <- head . Map.toList <$> ownUtxos
    let vh = fromJust $ Addr.toValidatorHash mustReferencePubKeyOutputV1ValidatorAddress
        lookups = mempty
        tx = TC.mustPayToOtherScript vh (Datum $ PlutusTx.toBuiltinData utxoRef) (Ada.adaValueOf 10)
    mkTxConstraints @Void lookups tx >>= submitTxConfirmed

    -- Trying to unlock the Ada in the script address
    let lookups = TC.unspentOutputs (Map.singleton utxoRef utxo)
        tx = TC.mustReferencePubKeyOutput utxoRef
    mkTxConstraints @Void lookups tx >>= submitTxConfirmed

{-
Test case: mustReferencePubKeyOutput can be used on-chain to unlock funds in a PlutusV2 script
-}

{-# INLINABLE mkMustReferencePubKeyOutputV2Validator #-}
mkMustReferencePubKeyOutputV2Validator :: TxOutRef -> () -> PV2.ScriptContext -> Bool
mkMustReferencePubKeyOutputV2Validator txOutRef _ =
    TC.checkV2ScriptContext @Void @Void (TC.mustReferencePubKeyOutput txOutRef)

{-# INLINABLE mustReferencePubKeyOutputV2Validator #-}
mustReferencePubKeyOutputV2Validator :: PV2.Validator
mustReferencePubKeyOutputV2Validator = PV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = PV2.mkUntypedValidator mkMustReferencePubKeyOutputV2Validator

mustReferencePubKeyOutputV2ValidatorAddress :: Address
mustReferencePubKeyOutputV2ValidatorAddress =
    PV2.mkValidatorAddress mustReferencePubKeyOutputV2Validator

mustReferencePubKeyOutputV2ConTest :: Contract () EmptySchema ContractError ()
mustReferencePubKeyOutputV2ConTest = do
    -- Locking some Ada in the script address
    (utxoRef, utxo) <- head . Map.toList <$> ownUtxos
    let vh = fromJust $ Addr.toValidatorHash mustReferencePubKeyOutputV2ValidatorAddress
        lookups = mempty
        tx = TC.mustPayToOtherScript vh (Datum $ PlutusTx.toBuiltinData utxoRef) (Ada.adaValueOf 10)
    mkTxConstraints @Void lookups tx >>= submitTxConfirmed

    -- Trying to unlock the Ada in the script address
    -- ownPkh <- Ledger.PaymentPubKeyHash . fromJust . Addr.toPubKeyHash . NE.head <$> ownAddresses
    scriptUtxos <- utxosAt mustReferencePubKeyOutputV2ValidatorAddress
    logInfo $ show scriptUtxos
    let
        addressMap = Map.singleton mustReferencePubKeyOutputV2ValidatorAddress scriptUtxos
        lookups = TC.unspentOutputs (Map.singleton utxoRef utxo <> scriptUtxos)
               <> TC.otherPlutusV2Script mustReferencePubKeyOutputV2Validator
        tx = TC.mustReferencePubKeyOutput utxoRef
          <> TC.collectFromScript addressMap mustReferencePubKeyOutputV2Validator (PV2.Redeemer $ PlutusTx.toBuiltinData ())
    logInfo $ show addressMap
    logInfo $ show lookups
    logInfo $ show $ collectFromScript addressMap mustReferencePubKeyOutputV2Validator (PV2.Redeemer $ PlutusTx.toBuiltinData ())
    logInfo $ show $ addressMap ^. at (PV2.mkValidatorAddress mustReferencePubKeyOutputV2Validator)
    logInfo $ show tx
    mkTxConstraints @Any lookups tx >>= submitTxConfirmed
