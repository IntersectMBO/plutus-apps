{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.TxConstraints.RequiredSigner(tests) where

import Control.Monad (void)
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)

import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import Data.String (fromString)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.CardanoWallet as CW
import Ledger.Constraints.OffChain qualified as Constraints (paymentPubKey, typedValidatorLookups, unspentOutputs)
import Ledger.Constraints.OnChain.V1 qualified as Constraints
import Ledger.Constraints.TxConstraints qualified as Constraints (collectFromTheScript, mustBeSignedBy,
                                                                  mustIncludeDatum, mustPayToTheScript,
                                                                  requiredSignatories)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertFailedTransaction, assertValidatedTransactionCount, checkPredicateOptions,
                             defaultCheckOptions, mockWalletPaymentPubKey, mockWalletPaymentPubKeyHash, w1, w2)
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError), unitDatum)
import PlutusTx qualified
import Prelude
import Wallet.Emulator.Wallet (signPrivateKeys, walletToMockWallet)

tests :: TestTree
tests =
    testGroup "required signer"
        [
          ownWallet
        , otherWallet
        , otherWalletNoSigningProcess
        , withoutOffChainMustBeSignedBy
        , phase2FailureMustBeSignedBy
        ]

mustBeSignedByContract :: Ledger.PaymentPubKey -> Ledger.PaymentPubKeyHash -> Contract () Empty ContractError ()
mustBeSignedByContract pk pkh = do
    let lookups1 = Constraints.typedValidatorLookups mustBeSignedByTypedValidator
        tx1 = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 25_000_000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt (Ledger.scriptHashAddress $ Scripts.validatorHash mustBeSignedByTypedValidator)
    let lookups2 =
            Constraints.typedValidatorLookups mustBeSignedByTypedValidator
            <> Constraints.unspentOutputs utxos
            <> Constraints.paymentPubKey pk
        tx2 =
            Constraints.collectFromTheScript utxos pkh
            <> Constraints.mustIncludeDatum unitDatum
            <> Constraints.mustBeSignedBy pkh
    logInfo @String $ "Required Signatories: " ++ show (Constraints.requiredSignatories tx2)
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

withoutOffChainMustBeSignedByContract :: Ledger.PaymentPubKey -> Ledger.PaymentPubKeyHash -> Contract () Empty ContractError ()
withoutOffChainMustBeSignedByContract pk pkh = do
    let lookups1 = Constraints.typedValidatorLookups mustBeSignedByTypedValidator
        tx1 = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 25_000_000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt (Ledger.scriptHashAddress $ Scripts.validatorHash mustBeSignedByTypedValidator)
    let lookups2 =
            Constraints.typedValidatorLookups mustBeSignedByTypedValidator
            <> Constraints.unspentOutputs utxos
            <> Constraints.paymentPubKey pk
        tx2 =
            Constraints.collectFromTheScript utxos pkh
            <> Constraints.mustIncludeDatum unitDatum
    logInfo @String $ "Required Signatories: " ++ show (Constraints.requiredSignatories tx2)
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

ownWallet :: TestTree
ownWallet =
    let pk  = mockWalletPaymentPubKey     w1
        pkh = mockWalletPaymentPubKeyHash w1
        trace = do
            void $ Trace.activateContractWallet w1 $ mustBeSignedByContract pk pkh
            void $ Trace.waitNSlots 1
    in checkPredicateOptions defaultCheckOptions "own wallet's signature passes on-chain mustBeSignedBy validation" (assertValidatedTransactionCount 2) (void trace)

otherWallet :: TestTree -- must use Trace.setSigningProcess for w2
otherWallet =
    let pk  = mockWalletPaymentPubKey     w2
        pkh = mockWalletPaymentPubKeyHash w2
        trace = do
            Trace.setSigningProcess w1 (Just $ signPrivateKeys [paymentPrivateKey $ fromJust $ walletToMockWallet w1, paymentPrivateKey $ fromJust $ walletToMockWallet w2])
            void $ Trace.activateContractWallet w1 $ mustBeSignedByContract pk pkh
            void $ Trace.waitNSlots 1
    in checkPredicateOptions defaultCheckOptions "other wallet's signature passes on-chain mustBeSignedBy validation" (assertValidatedTransactionCount 2) (void trace)

otherWalletNoSigningProcess :: TestTree
otherWalletNoSigningProcess =
    let pk  = mockWalletPaymentPubKey     w2
        pkh = mockWalletPaymentPubKeyHash w2
        trace = do
            void $ Trace.activateContractWallet w1 $ mustBeSignedByContract pk pkh
            void $ Trace.waitNSlots 1
    in checkPredicateOptions defaultCheckOptions "without Trace.setSigningProcess fails phase-1 validation"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.CardanoLedgerValidationError str -> isInfixOf "MissingRequiredSigners" str; _ -> False  }))
    (void trace)

withoutOffChainMustBeSignedBy :: TestTree -- there's no "required signer" in the txbody logs but still passes phase-2 so it must be there. Raised https://github.com/input-output-hk/plutus-apps/issues/645. It'd be good to check log output for expected required signer pubkey in these tests.
withoutOffChainMustBeSignedBy =
    let pk  = mockWalletPaymentPubKey     w1
        pkh = mockWalletPaymentPubKeyHash w1
        trace = do
            void $ Trace.activateContractWallet w1 $ withoutOffChainMustBeSignedByContract pk pkh
            void $ Trace.waitNSlots 1
    in checkPredicateOptions defaultCheckOptions "without mustBeSignedBy off-chain constraint passes mustBeSignedBy on-chain validation because required signer is still included in txbody"
    (assertValidatedTransactionCount 2)
    (void trace)

phase2FailureMustBeSignedBy :: TestTree
phase2FailureMustBeSignedBy =
    let pk  = mockWalletPaymentPubKey     w1
        pkh = Ledger.PaymentPubKeyHash $ fromString "76aaef06f38cc98ed08ceb168ddb55bab2ea5df43a6847a99f086fc9" :: Ledger.PaymentPubKeyHash
        trace = do
            void $ Trace.activateContractWallet w1 $ withoutOffChainMustBeSignedByContract pk pkh
            void $ Trace.waitNSlots 1
    in checkPredicateOptions defaultCheckOptions "with wrong pubkey fails on-chain mustBeSignedBy constraint validation"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("L4":_) _) -> True; _ -> False  }))
    (void trace)

{-
    validator using mustBeSignedBy constraint
-}

data UnitTest
instance Scripts.ValidatorTypes UnitTest  where
    type instance DatumType UnitTest = ()
    type instance RedeemerType UnitTest = Ledger.PaymentPubKeyHash

{-# INLINEABLE mustBeSignedByValidator #-}
mustBeSignedByValidator :: () -> Ledger.PaymentPubKeyHash -> Ledger.ScriptContext -> Bool
mustBeSignedByValidator _ pkh ctx = Constraints.checkScriptContext @Void @Void (Constraints.mustBeSignedBy pkh) ctx

mustBeSignedByTypedValidator :: Scripts.TypedValidator UnitTest
mustBeSignedByTypedValidator = Scripts.mkTypedValidator @UnitTest
    $$(PlutusTx.compile [||mustBeSignedByValidator||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator
