{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.TxConstraints.RequiredSigner(tests) where

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)

import Data.Maybe (fromJust)
import Data.String (fromString)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.CardanoWallet as CW
import Ledger.Constraints.OffChain qualified as Constraints (paymentPubKey, plutusV1OtherScript,
                                                             plutusV1TypedValidatorLookups, unspentOutputs)
import Ledger.Constraints.OnChain.V1 qualified as Constraints
import Ledger.Constraints.TxConstraints qualified as Constraints (mustBeSignedBy, mustIncludeDatum, mustPayToTheScript,
                                                                  mustSpendScriptOutput, requiredSignatories)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertFailedTransaction, assertValidatedTransactionCount, checkPredicateOptions,
                             defaultCheckOptions, mockWalletPaymentPubKey, mockWalletPaymentPubKeyHash, w1, w2)
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (Redeemer (Redeemer), ScriptContext (scriptContextTxInfo), ToData (toBuiltinData), TxInfo)
import Plutus.V1.Ledger.Contexts (txSignedBy)
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError), unitDatum)
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Prelude
import Wallet.Emulator.Wallet (signPrivateKeys, walletToMockWallet)

tests :: TestTree
tests =
    testGroup "required signer"
        [
        -- tests that use txSignedBy on-chain
          ownWallet
        , otherWallet
        , missingRequiredSignature
        , otherWalletNoSigningProcess
        , withoutOffChainMustBeSignedBy
        -- tests that use mustBeSignedBy constraint on-chain
        , ownWalletOnChainConstraint
        , missingRequiredSignatureOnChainConstraint
        , withoutOffChainMustBeSignedByOnChainConstraint
        ]

mustBeSignedByContract :: Ledger.PaymentPubKey -> Ledger.PaymentPubKeyHash -> Scripts.TypedValidator UnitTest -> Contract () Empty ContractError ()
mustBeSignedByContract pk pkh tv = do
    let validator = Scripts.validatorScript tv
        lookups1 = Constraints.plutusV1TypedValidatorLookups tv
        tx1 = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 25000000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt (Ledger.scriptHashAddress $ Scripts.validatorHash tv)
    let orefs = fst <$> Map.toList utxos
        redeemer = Redeemer $ toBuiltinData pkh
        lookups2 =
            Constraints.plutusV1OtherScript validator
            <> Constraints.unspentOutputs utxos
            <> Constraints.paymentPubKey pk
        tx2 =
            foldMap (\oref -> Constraints.mustSpendScriptOutput oref redeemer) orefs
            <> Constraints.mustIncludeDatum unitDatum
            <> Constraints.mustBeSignedBy pkh
    logInfo @String $ "Required Signatories: " ++ show (Constraints.requiredSignatories tx2)
    ledgerTx2 <- submitTxConstraintsWith @Void lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

withoutOffChainMustBeSignedByContract :: Ledger.PaymentPubKey -> Ledger.PaymentPubKeyHash -> Scripts.TypedValidator UnitTest -> Contract () Empty ContractError ()
withoutOffChainMustBeSignedByContract pk pkh tv = do
    let validator = Scripts.validatorScript tv
        lookups1 = Constraints.plutusV1TypedValidatorLookups tv
        tx1 = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 25000000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt (Ledger.scriptHashAddress $ Scripts.validatorHash tv)
    let orefs = fst <$> Map.toList utxos
        redeemer = Redeemer $ toBuiltinData pkh
        lookups2 =
            Constraints.plutusV1OtherScript validator
            <> Constraints.unspentOutputs utxos
            <> Constraints.paymentPubKey pk
        tx2 =
            foldMap (\oref -> Constraints.mustSpendScriptOutput oref redeemer) orefs
            <> Constraints.mustIncludeDatum unitDatum
    logInfo @String $ "Required Signatories: " ++ show (Constraints.requiredSignatories tx2)
    ledgerTx2 <- submitTxConstraintsWith @Void lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

ownWallet :: TestTree
ownWallet =
    let pk  = mockWalletPaymentPubKey     w1
        pkh = mockWalletPaymentPubKeyHash w1
        trace = do
            void $ Trace.activateContractWallet w1 $ mustBeSignedByContract pk pkh txSignedByTypedValidator
            void $ Trace.waitNSlots 1
    in checkPredicateOptions defaultCheckOptions "own wallet's signature passes on-chain txSignedBy validation" (assertValidatedTransactionCount 2) (void trace)

otherWallet :: TestTree
otherWallet =
    let pk  = mockWalletPaymentPubKey     w2
        pkh = mockWalletPaymentPubKeyHash w2
        trace = do
            Trace.setSigningProcess w1 (Just $ signPrivateKeys [paymentPrivateKey $ fromJust $ walletToMockWallet w1, paymentPrivateKey $ fromJust $ walletToMockWallet w2])
            void $ Trace.activateContractWallet w1 $ mustBeSignedByContract pk pkh txSignedByTypedValidator
            void $ Trace.waitNSlots 1
    in checkPredicateOptions defaultCheckOptions "other wallet's signature passes on-chain txSignedBy validation" (assertValidatedTransactionCount 2) (void trace)

missingRequiredSignature :: TestTree -- this should fail as phase-1 in the contract before script execution. We need to run phase-1 before phase-2 or only use ledger validation (PLT-645).
missingRequiredSignature =
    let pk  = mockWalletPaymentPubKey     w1
        pkh = Ledger.PaymentPubKeyHash $ fromString "76aaef06f38cc98ed08ceb168ddb55bab2ea5df43a6847a99f086fc1" :: Ledger.PaymentPubKeyHash
        trace = do
            void $ Trace.activateContractWallet w1 $ mustBeSignedByContract pk pkh txSignedByTypedValidator
            void $ Trace.waitNSlots 1
    in checkPredicateOptions defaultCheckOptions "with wrong pubkey fails on-chain txSignedBy validation"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("Missing signature":_) _) -> True; _ -> False  }))
    (void trace)

otherWalletNoSigningProcess :: TestTree -- this should fail as phase-1 in the contract before script execution. We need to run phase-1 before phase-2 or only use ledger validation (PLT-645).
otherWalletNoSigningProcess =
    let pk  = mockWalletPaymentPubKey     w2
        pkh = mockWalletPaymentPubKeyHash w2
        trace = do
            void $ Trace.activateContractWallet w1 $ mustBeSignedByContract pk pkh txSignedByTypedValidator
            void $ Trace.waitNSlots 1
    in checkPredicateOptions defaultCheckOptions "without Trace.setSigningProcess fails on-chain txSignedBy validation"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("Missing signature":_) _) -> True; _ -> False  }))
    (void trace)

withoutOffChainMustBeSignedBy :: TestTree -- there's no "required signer" in the txbody logs but still passes phase-2 so it must be there. Raised https://github.com/input-output-hk/plutus-apps/issues/645. It'd be good to check log output for expected required signer pubkey in these tests.
withoutOffChainMustBeSignedBy =
    let pk  = mockWalletPaymentPubKey     w1
        pkh = mockWalletPaymentPubKeyHash w1
        trace = do
            void $ Trace.activateContractWallet w1 $ withoutOffChainMustBeSignedByContract pk pkh txSignedByTypedValidator
            void $ Trace.waitNSlots 1
    in checkPredicateOptions defaultCheckOptions "without mustBeSignedBy off-chain constraint passes txSignedBy on-chain validation because required signer is still included in txbody"
    (assertValidatedTransactionCount 2)
    (void trace)

ownWalletOnChainConstraint :: TestTree
ownWalletOnChainConstraint =
    let pk  = mockWalletPaymentPubKey     w1
        pkh = mockWalletPaymentPubKeyHash w1
        trace = do
            void $ Trace.activateContractWallet w1 $ mustBeSignedByContract pk pkh mustBeSignedByTypedValidator
            void $ Trace.waitNSlots 1
    in checkPredicateOptions defaultCheckOptions "own wallet's signature passes on-chain mustBeSignedBy constraint validation" (assertValidatedTransactionCount 2) (void trace)

missingRequiredSignatureOnChainConstraint :: TestTree
missingRequiredSignatureOnChainConstraint =
    let pk  = mockWalletPaymentPubKey     w1
        pkh = Ledger.PaymentPubKeyHash $ fromString "76aaef06f38cc98ed08ceb168ddb55bab2ea5df43a6847a99f086fc9" :: Ledger.PaymentPubKeyHash
        trace = do
            void $ Trace.activateContractWallet w1 $ mustBeSignedByContract pk pkh mustBeSignedByTypedValidator
            void $ Trace.waitNSlots 1
    in checkPredicateOptions defaultCheckOptions "with wrong pubkey fails on-chain mustBeSignedBy constraint validation"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("L4":_) _) -> True; _ -> False  }))
    (void trace)

withoutOffChainMustBeSignedByOnChainConstraint :: TestTree -- there's no "required signer" in the txbody logs but still passes phase-2 so it must be there. Raised https://github.com/input-output-hk/plutus-apps/issues/645. It'd be good to check log output for expected required signer pubkey in these tests.
withoutOffChainMustBeSignedByOnChainConstraint =
    let pk  = mockWalletPaymentPubKey     w1
        pkh = mockWalletPaymentPubKeyHash w1
        trace = do
            void $ Trace.activateContractWallet w1 $ withoutOffChainMustBeSignedByContract pk pkh mustBeSignedByTypedValidator
            void $ Trace.waitNSlots 1
    in checkPredicateOptions defaultCheckOptions "without mustBeSignedBy off-chain constraint passes on-chain mustBeSignedBy constraint validation because required signer is still included in txbody"
    (assertValidatedTransactionCount 2)
    (void trace)

{-
    validator for using txSignedBy
-}

{-# INLINEABLE txSignedByValidator #-}
txSignedByValidator :: () -> Ledger.PaymentPubKeyHash -> ScriptContext -> Bool
txSignedByValidator _ pkh ctx = P.traceIfFalse "Missing signature" (txSignedBy info P.$ Ledger.unPaymentPubKeyHash pkh)
    where
    info :: TxInfo
    info = scriptContextTxInfo ctx

data UnitTest
instance Scripts.ValidatorTypes UnitTest  where
    type instance DatumType UnitTest = ()
    type instance RedeemerType UnitTest = Ledger.PaymentPubKeyHash

txSignedByTypedValidator :: Scripts.TypedValidator UnitTest
txSignedByTypedValidator = Scripts.mkTypedValidator @UnitTest
    $$(PlutusTx.compile [||txSignedByValidator||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

{-
    validator for using on-chain mustBeSignedBy constraint
-}
{-# INLINEABLE mustBeSignedByValidator #-}
mustBeSignedByValidator :: () -> Ledger.PaymentPubKeyHash -> ScriptContext -> Bool
mustBeSignedByValidator _ pkh ctx = Constraints.checkScriptContext @Void @Void (Constraints.mustBeSignedBy pkh) ctx

mustBeSignedByTypedValidator :: Scripts.TypedValidator UnitTest
mustBeSignedByTypedValidator = Scripts.mkTypedValidator @UnitTest
    $$(PlutusTx.compile [||mustBeSignedByValidator||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator
