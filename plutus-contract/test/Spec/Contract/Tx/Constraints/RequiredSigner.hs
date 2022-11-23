{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.Contract.Tx.Constraints.RequiredSigner(tests) where

import Control.Lens (_Just, has)
import Control.Monad (void)
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)

import Data.Default (Default (def))
import Data.Map as M
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.CardanoWallet qualified as CW
import Ledger.Constraints.OffChain qualified as Constraints hiding (requiredSignatories)
import Ledger.Constraints.OnChain.V1 qualified as Constraints
import Ledger.Constraints.TxConstraints qualified as Constraints
import Ledger.Test (someAddressV2, someValidatorHashV2, someValidatorV2)
import Ledger.Tx qualified as Tx
import Ledger.Tx.Constraints qualified as TxCons
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertFailedTransaction, assertValidatedTransactionCount, changeInitialWalletValue,
                             checkPredicateOptions, defaultCheckOptions, mockWalletPaymentPubKeyHash, w1, w2)
import Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
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
        , phase2FailureMustBeSignedBy
        , withoutOffChainMustBeSignedBy
        -- When we'll have enough constraints, reuse the previous tests
        , cardanoTxOwnWallet
        , cardanoTxOtherWalletNoSigningProcess
        ]

w1PubKey :: Ledger.PaymentPubKeyHash
w1PubKey = mockWalletPaymentPubKeyHash w1

w2PubKey :: Ledger.PaymentPubKeyHash
w2PubKey = mockWalletPaymentPubKeyHash w2

mustBeSignedByContract :: Ledger.PaymentPubKeyHash -> Ledger.PaymentPubKeyHash -> Contract () Empty ContractError ()
mustBeSignedByContract paymentPubKey signedPubKey = do
    let lookups1 = Constraints.typedValidatorLookups mustBeSignedByTypedValidator
        tx1 = Constraints.mustPayToTheScriptWithDatumInTx
                ()
                (Ada.lovelaceValueOf 25_000_000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt (Ledger.scriptHashAddress $ Scripts.validatorHash mustBeSignedByTypedValidator)
    let lookups2 =
            Constraints.typedValidatorLookups mustBeSignedByTypedValidator
            <> Constraints.unspentOutputs utxos
            <> Constraints.paymentPubKeyHash paymentPubKey
        tx2 =
            Constraints.collectFromTheScript utxos signedPubKey
            <> Constraints.mustIncludeDatumInTx unitDatum
            <> Constraints.mustBeSignedBy signedPubKey
    logInfo @String $ "Required Signatories: " ++ show (Constraints.requiredSignatories tx2)
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

withoutOffChainMustBeSignedByContract :: Ledger.PaymentPubKeyHash -> Ledger.PaymentPubKeyHash -> Contract () Empty ContractError ()
withoutOffChainMustBeSignedByContract paymentPubKey signedPubKey = do
    let lookups1 = Constraints.typedValidatorLookups mustBeSignedByTypedValidator
        tx1 = Constraints.mustPayToTheScriptWithDatumInTx
                ()
                (Ada.lovelaceValueOf 25_000_000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt (Ledger.scriptHashAddress $ Scripts.validatorHash mustBeSignedByTypedValidator)
    let lookups2 =
            Constraints.typedValidatorLookups mustBeSignedByTypedValidator
            <> Constraints.unspentOutputs utxos
            <> Constraints.paymentPubKeyHash paymentPubKey
        tx2 =
            Constraints.collectFromTheScript utxos signedPubKey
            <> Constraints.mustIncludeDatumInTx unitDatum
    logInfo @String $ "Required Signatories: " ++ show (Constraints.requiredSignatories tx2)
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

ownWallet :: TestTree
ownWallet =
    let trace = do
            void $ Trace.activateContractWallet w1 $ mustBeSignedByContract w1PubKey w1PubKey
            void Trace.nextSlot
    in checkPredicateOptions defaultCheckOptions "own wallet's signature passes on-chain mustBeSignedBy validation" (assertValidatedTransactionCount 2) (void trace)

otherWallet :: TestTree -- must use Trace.setSigningProcess for w2
otherWallet =
    let trace = do
            Trace.setSigningProcess w1 (Just $ signPrivateKeys [CW.paymentPrivateKey $ fromJust $ walletToMockWallet w1, CW.paymentPrivateKey $ fromJust $ walletToMockWallet w2])
            void $ Trace.activateContractWallet w1 $ mustBeSignedByContract w2PubKey w2PubKey
            void Trace.nextSlot
    in checkPredicateOptions defaultCheckOptions "other wallet's signature passes on-chain mustBeSignedBy validation" (assertValidatedTransactionCount 2) (void trace)

otherWalletNoSigningProcess :: TestTree
otherWalletNoSigningProcess =
    let trace = do
            void $ Trace.activateContractWallet w1 $ mustBeSignedByContract w2PubKey w2PubKey
            void Trace.nextSlot
    in checkPredicateOptions defaultCheckOptions "without Trace.setSigningProcess fails phase-1 validation"
    (assertFailedTransaction (\_ err -> case err of {Ledger.CardanoLedgerValidationError msg -> Text.isInfixOf "MissingRequiredSigners" msg; _ -> False  }))
    (void trace)

withoutOffChainMustBeSignedBy :: TestTree -- there's no "required signer" in the txbody logs but still passes phase-2 so it must be there. Raised https://github.com/input-output-hk/plutus-apps/issues/645. It'd be good to check log output for expected required signer pubkey in these tests.
withoutOffChainMustBeSignedBy =
    let trace = do
            void $ Trace.activateContractWallet w1 $ withoutOffChainMustBeSignedByContract w1PubKey w1PubKey
            void Trace.nextSlot
    in checkPredicateOptions defaultCheckOptions "without mustBeSignedBy off-chain constraint required signer is not included in txbody so phase-2 validation fails"
    (assertFailedTransaction (\_ err -> case err of {Ledger.ScriptFailure (EvaluationError ("L4":_) _) -> True; _ -> False  }))
    (void trace)

phase2FailureMustBeSignedBy :: TestTree
phase2FailureMustBeSignedBy =
    let trace = do
            void $ Trace.activateContractWallet w1 $ withoutOffChainMustBeSignedByContract w1PubKey w2PubKey
            void Trace.nextSlot
    in checkPredicateOptions defaultCheckOptions "with wrong pubkey fails on-chain mustBeSignedBy constraint validation"
    (assertFailedTransaction (\_ err -> case err of {Ledger.ScriptFailure (EvaluationError ("L4":_) _) -> True; _ -> False  }))
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
mustBeSignedByValidator _ pkh = Constraints.checkScriptContext @Void @Void (Constraints.mustBeSignedBy pkh)

mustBeSignedByTypedValidator :: Scripts.TypedValidator UnitTest
mustBeSignedByTypedValidator = Scripts.mkTypedValidator @UnitTest
    $$(PlutusTx.compile [||mustBeSignedByValidator||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator


-- plutus-tx-constraints tests
-- all below to be covered by the above tests when the corresponding constraints will be implemented
-- for CardanoTx


tag :: Trace.ContractInstanceTag
tag = "instance 1"

cardanoTxOwnWalletContract
    :: Ledger.PaymentPubKeyHash
    -> Ledger.PaymentPubKeyHash
    -> Contract () EmptySchema ContractError ()
cardanoTxOwnWalletContract pk pkh = do
    let mkTx lookups constraints = either (error . show) id $ TxCons.mkTx @UnitTest def lookups constraints

    utxos <- ownUtxos
    myAddr <- ownAddress
    let get3 (a:b:c:_) = (a, b, c)
        get3 _         = error "Spec.Contract.TxConstraints.get3: not enough inputs"
        ((utxoRef, utxo), (utxoRefForBalance1, _), (utxoRefForBalance2, _)) = get3 $ M.toList utxos
        lookups1 = Constraints.unspentOutputs utxos
               <> Constraints.typedValidatorLookups mustBeSignedByTypedValidator
               <> Constraints.plutusV2OtherScript someValidatorV2
               <> Constraints.paymentPubKeyHash pk
        tx1 = Constraints.mustPayToOtherScriptWithDatumInTx
                someValidatorHashV2
                (Ledger.Datum $ PlutusTx.toBuiltinData utxoRef)
                (Ada.adaValueOf 5)
          <> Constraints.mustSpendPubKeyOutput utxoRefForBalance1
          <> Constraints.mustUseOutputAsCollateral utxoRefForBalance1
          <> Constraints.mustPayToAddressWithReferenceValidator
                myAddr
                someValidatorHashV2
                Nothing
                (Ada.adaValueOf 30)
    submitTxConfirmed $ mkTx lookups1 tx1

    -- Trying to unlock the Ada in the script address
    scriptUtxos <- utxosAt someAddressV2
    utxos' <- ownUtxos
    let
        scriptUtxo = fst . head . M.toList $ scriptUtxos
        refScriptUtxo = head . M.keys . M.filter (has $ Tx.decoratedTxOutReferenceScript . _Just) $ utxos'
        lookups2 = Constraints.unspentOutputs (M.singleton utxoRef utxo <> scriptUtxos <> utxos')
          <> Constraints.typedValidatorLookups mustBeSignedByTypedValidator
        tx2 = Constraints.mustReferenceOutput utxoRef
          <> Constraints.mustSpendScriptOutputWithReference scriptUtxo Ledger.unitRedeemer refScriptUtxo
          <> Constraints.mustSpendPubKeyOutput utxoRefForBalance2
          <> Constraints.mustUseOutputAsCollateral utxoRefForBalance2
          <> Constraints.mustBeSignedBy pkh
    logInfo @String $ "Required Signatories: " ++ show (Constraints.requiredSignatories tx2)
    submitTxConfirmed $ mkTx lookups2 tx2

cardanoTxOwnWallet :: TestTree
cardanoTxOwnWallet =
    let trace = do
            void $ Trace.activateContractWallet w1 $ cardanoTxOwnWalletContract w1PubKey w1PubKey
            void Trace.nextSlot
    in checkPredicateOptions
        (changeInitialWalletValue w1 (const $ Ada.adaValueOf 1000) defaultCheckOptions)
        "own wallet's signature passes on-chain mustBeSignedBy validation with cardano tx"
        (assertValidatedTransactionCount 2)
        (void trace)

cardanoTxOtherWalletNoSigningProcess :: TestTree
cardanoTxOtherWalletNoSigningProcess =
    let trace = do
            void $ Trace.activateContractWallet w1 $ cardanoTxOwnWalletContract w2PubKey w2PubKey
            void Trace.nextSlot
    in checkPredicateOptions
        -- Needed to manually balance the transaction
        -- We may remove it once PLT-321
        (changeInitialWalletValue w1 (const $ Ada.adaValueOf 1000) defaultCheckOptions)
        "without Trace.setSigningProcess fails phase-1 validation"
        (assertFailedTransaction (\_ err -> case err of {Ledger.CardanoLedgerValidationError msg -> Text.isInfixOf "MissingRequiredSigners" msg; _ -> False  }))
    (void trace)
