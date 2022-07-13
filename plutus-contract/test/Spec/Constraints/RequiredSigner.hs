{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.Constraints.RequiredSigner(tests) where

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)

import Data.Maybe (fromJust)
import Data.String (fromString)
import Ledger (PaymentPubKey (PaymentPubKey), PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash))
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.CardanoWallet as CW
import Ledger.Constraints.OffChain qualified as Constraints (paymentPubKey, plutusV1OtherScript,
                                                             plutusV1TypedValidatorLookups, unspentOutputs)
import Ledger.Constraints.TxConstraints qualified as Constraints (mustBeSignedBy, mustIncludeDatum, mustPayToTheScript,
                                                                  mustSpendScriptOutput, requiredSignatories)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertFailedTransaction, assertValidatedTransactionCount, checkPredicateOptions,
                             defaultCheckOptions, mockWalletPaymentPubKey, mockWalletPaymentPubKeyHash, w1, w2)
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (PubKeyHash, Redeemer (Redeemer), ScriptContext (scriptContextTxInfo),
                             ToData (toBuiltinData), TxInfo, Validator, ValidatorHash)
import Plutus.V1.Ledger.Contexts (txSignedBy)
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError), unitDatum)
import PlutusTx qualified
import PlutusTx.Prelude (traceIfFalse)
import Prelude hiding (not)
import Wallet.Emulator.Wallet (signPrivateKeys, walletToMockWallet)

tests :: TestTree
tests =
    testGroup "required signer"
        [ ownWallet
        , otherWallet
        , missingRequiredSignature
        , otherWalletNoSigningProcess
        , defaultBehaviour -- no need to explictly use mustBeSignedBy for own wallet
        ]

contract :: PaymentPubKey -> PaymentPubKeyHash -> Contract () Empty ContractError ()
contract pk pkh = do
    let lookups1 = Constraints.plutusV1TypedValidatorLookups typedValidator
        tx1 = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 25000000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt scrAddress
    let orefs = fst <$> Map.toList utxos
        redeemer = Redeemer $ toBuiltinData $ unPaymentPubKeyHash pkh
        lookups2 =
            Constraints.plutusV1OtherScript validatorScript
            <> Constraints.unspentOutputs utxos
            <> Constraints.paymentPubKey pk
        tx2 =
            foldMap (\oref -> Constraints.mustSpendScriptOutput oref redeemer) orefs
            <> Constraints.mustIncludeDatum unitDatum
            <> Constraints.mustBeSignedBy pkh
    logInfo @String $ "Required Signatories: " ++ show (Constraints.requiredSignatories tx2)
    ledgerTx2 <- submitTxConstraintsWith @Void lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

ownWallet :: TestTree
ownWallet =
    let pk  = mockWalletPaymentPubKey     w1
        pkh = mockWalletPaymentPubKeyHash w1
        trace = do
            void $ Trace.activateContractWallet w1 $ contract pk pkh
            void $ Trace.waitNSlots 1
    in checkPredicateOptions defaultCheckOptions "own wallet's signature passes on-chain validation" (assertValidatedTransactionCount 2) (void trace)

otherWallet :: TestTree
otherWallet =
    let pk  = mockWalletPaymentPubKey     w2
        pkh = mockWalletPaymentPubKeyHash w2
        trace = do
            Trace.setSigningProcess w1 (Just $ signPrivateKeys [paymentPrivateKey $ fromJust $ walletToMockWallet w1, paymentPrivateKey $ fromJust $ walletToMockWallet w2])
            void $ Trace.activateContractWallet w1 $ contract pk pkh
            void $ Trace.waitNSlots 1
    in checkPredicateOptions defaultCheckOptions "other wallet's signature passes on-chain validation" (assertValidatedTransactionCount 2) (void trace)

missingRequiredSignature :: TestTree -- this should fail as phase-1 in the contract before script execution. We need to run phase-1 before phase-2 or only use ledger validation (PLT-645).
missingRequiredSignature =
    let pk  = mockWalletPaymentPubKey     w1
        pkh = PaymentPubKeyHash $ fromString "76aaef06f38cc98ed08ceb168ddb55bab2ea5df43a6847a99f086fc1" :: PaymentPubKeyHash
        trace = do
            void $ Trace.activateContractWallet w1 $ contract pk pkh
            void $ Trace.waitNSlots 1
    in checkPredicateOptions defaultCheckOptions "with wrong pubkey fails on-chain validation"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("Missing signature":_) _) -> True; _ -> False  }))
    (void trace)

otherWalletNoSigningProcess :: TestTree -- this should fail as phase-1 in the contract before script execution. We need to run phase-1 before phase-2 or only use ledger validation (PLT-645).
otherWalletNoSigningProcess =
    let pk  = mockWalletPaymentPubKey     w2
        pkh = mockWalletPaymentPubKeyHash w2
        trace = do
            void $ Trace.activateContractWallet w1 $ contract pk pkh
            void $ Trace.waitNSlots 1
    in checkPredicateOptions defaultCheckOptions "without Trace.setSigningProcess fails on-chain validation"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("Missing signature":_) _) -> True; _ -> False  }))
    (void trace)

defaultBehaviour :: TestTree
defaultBehaviour =
    let normalSpendContract :: Contract () Empty ContractError () = do
            let lookups1 = Constraints.plutusV1TypedValidatorLookups typedValidator
                tx1 = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 25000000)
            ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

            utxos <- utxosAt scrAddress
            let orefs = fst <$> Map.toList utxos
                redeemer = Redeemer $ toBuiltinData $ unPaymentPubKeyHash pkh
                lookups2 =
                    Constraints.plutusV1OtherScript validatorScript
                    <> Constraints.unspentOutputs utxos
                tx2 =
                    foldMap (\oref -> Constraints.mustSpendScriptOutput oref redeemer) orefs
                    <> Constraints.mustIncludeDatum unitDatum
            logInfo @String $ "Required Signatories: " ++ show (Constraints.requiredSignatories tx2)
            ledgerTx2 <- submitTxConstraintsWith @Void lookups2 tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

        pkh = mockWalletPaymentPubKeyHash w2
        trace = do
            void $ Trace.activateContractWallet w1 normalSpendContract
            void $ Trace.waitNSlots 1
    in checkPredicateOptions defaultCheckOptions "without mustBeSignedBy constraint fails on-chain validation (even though tx is signed by correct wallet)"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("Missing signature":_) _) -> True; _ -> False  }))
    (void trace)

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> PubKeyHash -> ScriptContext -> Bool
mkValidator _ pkh ctx = traceIfFalse "Missing signature" (txSignedBy info pkh)
    where
    info :: TxInfo
    info = scriptContextTxInfo ctx

data UnitTest
instance Scripts.ValidatorTypes UnitTest  where
    type instance DatumType UnitTest = ()
    type instance RedeemerType UnitTest = PubKeyHash

typedValidator :: Scripts.TypedValidator UnitTest
typedValidator = Scripts.mkTypedValidator @UnitTest
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

validatorScript :: Validator
validatorScript = Scripts.validatorScript typedValidator

valHash :: ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = Ledger.scriptHashAddress valHash
