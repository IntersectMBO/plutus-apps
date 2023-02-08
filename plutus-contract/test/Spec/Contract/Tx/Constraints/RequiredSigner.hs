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

import Cardano.Node.Emulator.Params qualified as Params
import Data.Maybe (fromJust)
import Ledger qualified
import Ledger.CardanoWallet qualified as CW
import Ledger.Constraints.OffChain qualified as Constraints hiding (requiredSignatories)
import Ledger.Constraints.OnChain.V2 qualified as Constraints
import Ledger.Constraints.TxConstraints qualified as Constraints
import Ledger.Tx qualified as Tx
import Plutus.Contract as Con
import Plutus.Contract.Test (assertEvaluationError, assertFailedTransaction, assertValidatedTransactionCount,
                             checkPredicateOptions, defaultCheckOptions, mockWalletPaymentPubKeyHash, w1, w2)
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Typed qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts qualified as Scripts
import Plutus.Trace.Emulator qualified as Trace (activateContractWallet, nextSlot, setSigningProcess)
import Plutus.V1.Ledger.Scripts (unitDatum)
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx qualified
import Prelude
import Spec.Contract.Error (cardanoLedgerErrorContaining)
import Wallet.Emulator.Wallet (signPrivateKeys, walletToMockWallet)

tests :: TestTree
tests = testGroup "Required signer" [ ownWallet
    , otherWallet
    , otherWalletNoSigningProcess
    , phase2FailureMustBeSignedBy
    , withoutOffChainMustBeSignedBy
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

    utxos <- utxosAt $ Scripts.validatorCardanoAddress (Params.pNetworkId params) mustBeSignedByTypedValidator
    let lookups2 =
            Constraints.typedValidatorLookups mustBeSignedByTypedValidator
            <> Constraints.unspentOutputs utxos
            <> Constraints.paymentPubKeyHash paymentPubKey
        tx2 =
            Constraints.collectFromTheScript utxos signedPubKey
            <> Constraints.mustIncludeDatumInTx unitDatum
            <> Constraints.mustBeSignedBy signedPubKey
    logInfo @String $ "Required Signatories: " ++ show (Constraints.requiredSignatories tx2)
    ledgerTx2 <- submitTxConstraintsWith lookups2 tx2
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

    utxos <- utxosAt $ Scripts.validatorCardanoAddress (Params.pNetworkId params) mustBeSignedByTypedValidator
    let lookups2 =
            Constraints.typedValidatorLookups mustBeSignedByTypedValidator
            <> Constraints.unspentOutputs utxos
            <> Constraints.paymentPubKeyHash paymentPubKey
        tx2 =
            Constraints.collectFromTheScript utxos signedPubKey
            <> Constraints.mustIncludeDatumInTx unitDatum
    logInfo @String $ "Required Signatories: " ++ show (Constraints.requiredSignatories tx2)
    ledgerTx2 <- submitTxConstraintsWith lookups2 tx2
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

withoutOffChainMustBeSignedBy :: TestTree
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
