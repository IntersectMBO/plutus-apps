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
import Data.String (fromString)
import Data.Text qualified as Text
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.CardanoWallet qualified as CW
import Ledger.Constraints.OffChain qualified as Constraints hiding (requiredSignatories)
import Ledger.Constraints.OnChain.V1 qualified as Constraints
import Ledger.Constraints.OnChain.V2 qualified as Cons.V2
import Ledger.Constraints.TxConstraints qualified as Constraints
import Ledger.Tx qualified as Tx
import Ledger.Tx.Constraints qualified as TxCons
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertFailedTransaction, assertValidatedTransactionCount, changeInitialWalletValue,
                             checkPredicateOptions, defaultCheckOptions, mockWalletPaymentPubKey,
                             mockWalletPaymentPubKeyHash, w1, w2)
import Plutus.Script.Utils.Typed (Any)
import Plutus.Script.Utils.V2.Address qualified as PSU.V2
import Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api qualified as PV1
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError), unitDatum)
import Plutus.V2.Ledger.Api qualified as PV2
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
    let pkh = mockWalletPaymentPubKeyHash w1
        trace = do
            void $ Trace.activateContractWallet w1 $ mustBeSignedByContract pkh pkh
            void Trace.nextSlot
    in checkPredicateOptions defaultCheckOptions "own wallet's signature passes on-chain mustBeSignedBy validation" (assertValidatedTransactionCount 2) (void trace)

otherWallet :: TestTree -- must use Trace.setSigningProcess for w2
otherWallet =
    let pkh = mockWalletPaymentPubKeyHash w2
        trace = do
            Trace.setSigningProcess w1 (Just $ signPrivateKeys [CW.paymentPrivateKey $ fromJust $ walletToMockWallet w1, CW.paymentPrivateKey $ fromJust $ walletToMockWallet w2])
            void $ Trace.activateContractWallet w1 $ mustBeSignedByContract pkh pkh
            void Trace.nextSlot
    in checkPredicateOptions defaultCheckOptions "other wallet's signature passes on-chain mustBeSignedBy validation" (assertValidatedTransactionCount 2) (void trace)

otherWalletNoSigningProcess :: TestTree
otherWalletNoSigningProcess =
    let pkh = mockWalletPaymentPubKeyHash w2
        trace = do
            void $ Trace.activateContractWallet w1 $ mustBeSignedByContract pkh pkh
            void Trace.nextSlot
    in checkPredicateOptions defaultCheckOptions "without Trace.setSigningProcess fails phase-1 validation"
    (assertFailedTransaction (\_ err -> case err of {Ledger.CardanoLedgerValidationError msg -> Text.isInfixOf "MissingRequiredSigners" msg; _ -> False  }))
    (void trace)

withoutOffChainMustBeSignedBy :: TestTree -- there's no "required signer" in the txbody logs but still passes phase-2 so it must be there. Raised https://github.com/input-output-hk/plutus-apps/issues/645. It'd be good to check log output for expected required signer pubkey in these tests.
withoutOffChainMustBeSignedBy =
    let pkh = mockWalletPaymentPubKeyHash w1
        trace = do
            void $ Trace.activateContractWallet w1 $ withoutOffChainMustBeSignedByContract pkh pkh
            void Trace.nextSlot
    in checkPredicateOptions defaultCheckOptions "without mustBeSignedBy off-chain constraint required signer is not included in txbody so phase-2 validation fails"
    (assertFailedTransaction (\_ err -> case err of {Ledger.ScriptFailure (EvaluationError ("L4":_) _) -> True; _ -> False  }))
    (void trace)

phase2FailureMustBeSignedBy :: TestTree
phase2FailureMustBeSignedBy =
    let pkh1  = mockWalletPaymentPubKeyHash     w1
        pkh2 = Ledger.PaymentPubKeyHash $ fromString "76aaef06f38cc98ed08ceb168ddb55bab2ea5df43a6847a99f086fc9" :: Ledger.PaymentPubKeyHash
        trace = do
            void $ Trace.activateContractWallet w1 $ withoutOffChainMustBeSignedByContract pkh1 pkh2
            void $ Trace.waitNSlots 1
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


{-# INLINABLE mustReferenceOutputValidatorV2 #-}
mustReferenceOutputValidatorV2 :: PV2.Validator
mustReferenceOutputValidatorV2 = PV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     mkMustReferenceOutputV2Validator = mkMustReferenceOutputValidator Cons.V2.checkScriptContext
     wrap = PSU.V2.mkUntypedValidator mkMustReferenceOutputV2Validator

tag :: Trace.ContractInstanceTag
tag = "instance 1"

mkMustReferenceOutputValidator
    :: (Constraints.TxConstraints Void Void -> sc -> Bool)
    -> PV1.TxOutRef -> () -> sc -> Bool
mkMustReferenceOutputValidator checkScriptContext txOutRef _ =
    checkScriptContext (Constraints.mustReferenceOutput txOutRef)

mustReferenceOutputV2ValidatorAddress :: Ledger.Address
mustReferenceOutputV2ValidatorAddress =
    PSU.V2.mkValidatorAddress mustReferenceOutputValidatorV2

cardanoTxOwnWalletContract
    :: Ledger.PaymentPubKeyHash
    -> Ledger.PaymentPubKeyHash
    -> Contract () EmptySchema ContractError ()
cardanoTxOwnWalletContract pk pkh = do
    let mkTx lookups constraints = either (error . show) id $ TxCons.mkTx @Any def lookups constraints

    utxos <- ownUtxos
    myAddr <- ownAddress
    let get3 (a:b:c:_) = (a, b, c)
        get3 _         = error "Spec.Contract.TxConstraints.get3: not enough inputs"
        ((utxoRef, utxo), (utxoRefForBalance1, _), (utxoRefForBalance2, _)) = get3 $ M.toList utxos
        vh = fromJust $ Ledger.toValidatorHash mustReferenceOutputV2ValidatorAddress
        lookups1 = Constraints.unspentOutputs utxos
               <> Constraints.plutusV2OtherScript mustReferenceOutputValidatorV2
               <> Constraints.paymentPubKeyHash pk
        tx1 = Constraints.mustPayToOtherScriptWithDatumInTx
                vh
                (Ledger.Datum $ PlutusTx.toBuiltinData utxoRef)
                (Ada.adaValueOf 5)
          <> Constraints.mustSpendPubKeyOutput utxoRefForBalance1
          <> Constraints.mustUseOutputAsCollateral utxoRefForBalance1
          <> Constraints.mustPayToAddressWithReferenceValidator
                myAddr
                vh
                Nothing
                (Ada.adaValueOf 30)
    submitTxConfirmed $ mkTx lookups1 tx1

    -- Trying to unlock the Ada in the script address
    scriptUtxos <- utxosAt mustReferenceOutputV2ValidatorAddress
    utxos' <- ownUtxos
    let
        scriptUtxo = fst . head . M.toList $ scriptUtxos
        refScriptUtxo = head . M.keys . M.filter (has $ Tx.decoratedTxOutReferenceScript . _Just) $ utxos'
        lookups2 = Constraints.unspentOutputs (M.singleton utxoRef utxo <> scriptUtxos <> utxos')
        tx2 = Constraints.mustReferenceOutput utxoRef
          <> Constraints.mustSpendScriptOutputWithReference scriptUtxo Ledger.unitRedeemer refScriptUtxo
          <> Constraints.mustSpendPubKeyOutput utxoRefForBalance2
          <> Constraints.mustUseOutputAsCollateral utxoRefForBalance2
          <> Constraints.mustBeSignedBy pkh
    logInfo @String $ "Required Signatories: " ++ show (Constraints.requiredSignatories tx2)
    submitTxConfirmed $ mkTx lookups2 tx2

cardanoTxOwnWallet :: TestTree
cardanoTxOwnWallet =
    let pkh = mockWalletPaymentPubKeyHash w1
        trace = do
            void $ Trace.activateContractWallet w1 $ cardanoTxOwnWalletContract pkh pkh
            void Trace.nextSlot
    in checkPredicateOptions
        (changeInitialWalletValue w1 (const $ Ada.adaValueOf 1000) defaultCheckOptions)
        "own wallet's signature passes on-chain mustBeSignedBy validation with cardano tx" (assertValidatedTransactionCount 2) (void trace)

cardanoTxOtherWalletNoSigningProcess :: TestTree
cardanoTxOtherWalletNoSigningProcess =
    let pkh = mockWalletPaymentPubKeyHash w2
        trace = do
            void $ Trace.activateContractWallet w1 $ cardanoTxOwnWalletContract pkh pkh
            void Trace.nextSlot
    in checkPredicateOptions
        (changeInitialWalletValue w1 (const $ Ada.adaValueOf 1000) defaultCheckOptions)
        "without Trace.setSigningProcess fails phase-1 validation"
        (assertFailedTransaction (\_ err -> case err of {Ledger.CardanoLedgerValidationError msg -> Text.isInfixOf "MissingRequiredSigners" msg; _ -> False  }))
    (void trace)
