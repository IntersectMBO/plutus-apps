{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.Contract.Tx.Constraints.RequiredSigner(tests) where

import Control.Monad (void)
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)

import Data.Default (Default (def))
import Data.Map as M
import Data.Maybe (fromJust)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.CardanoWallet qualified as CW
import Ledger.Constraints.OffChain qualified as Constraints hiding (requiredSignatories)
import Ledger.Constraints.OnChain.V2 qualified as Constraints
import Ledger.Constraints.TxConstraints qualified as Constraints
import Ledger.Tx qualified as Tx
import Ledger.Tx.Constraints qualified as TxCons
import Plutus.Contract as Con
import Plutus.Contract.Test (assertEvaluationError, assertFailedTransaction, assertValidatedTransactionCount,
                             changeInitialWalletValue, checkPredicateOptions, defaultCheckOptions,
                             mockWalletPaymentPubKeyHash, w1, w2)
import Plutus.Script.Utils.Typed qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts qualified as Scripts
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Scripts (unitDatum)
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx qualified
import Prelude
import Spec.Contract.Error (cardanoLedgerErrorContaining)
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
    params <- getParams
    let lookups1 = Constraints.typedValidatorLookups mustBeSignedByTypedValidator
        tx1 = Constraints.mustPayToTheScriptWithDatumInTx
                ()
                (Ada.lovelaceValueOf 25_000_000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt $ Scripts.validatorCardanoAddress (Ledger.pNetworkId params) mustBeSignedByTypedValidator
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
    params <- getParams
    let lookups1 = Constraints.typedValidatorLookups mustBeSignedByTypedValidator
        tx1 = Constraints.mustPayToTheScriptWithDatumInTx
                ()
                (Ada.lovelaceValueOf 25_000_000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt $ Scripts.validatorCardanoAddress (Ledger.pNetworkId params) mustBeSignedByTypedValidator
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
    (assertFailedTransaction (const $ cardanoLedgerErrorContaining "MissingRequiredSigners"))
    (void trace)

withoutOffChainMustBeSignedBy :: TestTree -- there's no "required signer" in the txbody logs but still passes phase-2 so it must be there. Raised https://github.com/input-output-hk/plutus-apps/issues/645. It'd be good to check log output for expected required signer pubkey in these tests.
withoutOffChainMustBeSignedBy =
    let trace = do
            void $ Trace.activateContractWallet w1 $ withoutOffChainMustBeSignedByContract w1PubKey w1PubKey
            void Trace.nextSlot
    in checkPredicateOptions defaultCheckOptions "without mustBeSignedBy off-chain constraint required signer is not included in txbody so phase-2 validation fails"
    (assertEvaluationError "L4")
    (void trace)

phase2FailureMustBeSignedBy :: TestTree
phase2FailureMustBeSignedBy =
    let trace = do
            void $ Trace.activateContractWallet w1 $ withoutOffChainMustBeSignedByContract w1PubKey w2PubKey
            void Trace.nextSlot
    in checkPredicateOptions defaultCheckOptions "with wrong pubkey fails on-chain mustBeSignedBy constraint validation"
    (assertEvaluationError "L4")
    (void trace)

{-
    validator using mustBeSignedBy constraint
-}

data UnitTest
instance Scripts.ValidatorTypes UnitTest  where
    type instance DatumType UnitTest = ()
    type instance RedeemerType UnitTest = Ledger.PaymentPubKeyHash

{-# INLINEABLE mustBeSignedByValidator #-}
mustBeSignedByValidator :: () -> Ledger.PaymentPubKeyHash -> PV2.ScriptContext -> Bool
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


cardanoTxOwnWalletContract
    :: Ledger.PaymentPubKeyHash
    -> Ledger.PaymentPubKeyHash
    -> Contract () EmptySchema ContractError ()
cardanoTxOwnWalletContract paymentPubKey signedPubKey = do
    params <- getParams
    let mkTx lookups constraints = either (error . show) id $ TxCons.mkTx @UnitTest def lookups constraints

    utxos <- ownUtxos
    let get3 (a:b:c:_) = (a, b, c)
        get3 _         = error "Spec.Contract.TxConstraints.get3: not enough inputs"
        ((utxoRef, utxo), (utxoRefForBalance1, _), (utxoRefForBalance2, _)) = get3 $ M.toList utxos
        lookups1 = Constraints.typedValidatorLookups mustBeSignedByTypedValidator
                <> Constraints.unspentOutputs utxos
        tx1 = Constraints.mustPayToTheScriptWithDatumInTx
                ()
                (Ada.lovelaceValueOf 25_000_000)
          <> Constraints.mustSpendPubKeyOutput utxoRefForBalance1
          <> Constraints.mustUseOutputAsCollateral utxoRefForBalance1
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    -- Trying to unlock the Ada in the script address
    scriptUtxos <- utxosAt $ Scripts.validatorCardanoAddress (Ledger.pNetworkId params) mustBeSignedByTypedValidator
    utxos' <- ownUtxos
    let lookups2 =
            Constraints.typedValidatorLookups mustBeSignedByTypedValidator
            <> Constraints.unspentOutputs (M.singleton utxoRef utxo <> scriptUtxos <> utxos')
            <> Constraints.paymentPubKeyHash paymentPubKey
        tx2 =
            Constraints.collectFromTheScript utxos signedPubKey
            <> Constraints.mustIncludeDatumInTx unitDatum
            <> Constraints.mustUseOutputAsCollateral utxoRefForBalance2
            <> Constraints.mustSpendPubKeyOutput utxoRefForBalance2
            <> Constraints.mustBeSignedBy signedPubKey
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
        (assertFailedTransaction (const $ cardanoLedgerErrorContaining "MissingRequiredSigners"))
        (void trace)
