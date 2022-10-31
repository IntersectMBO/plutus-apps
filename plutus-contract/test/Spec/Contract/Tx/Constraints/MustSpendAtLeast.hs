{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.Contract.Tx.Constraints.MustSpendAtLeast(tests) where

import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints.OffChain qualified as Constraints
import Ledger.Constraints.OnChain.V1 qualified as Constraints
import Ledger.Constraints.TxConstraints qualified as Constraints
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertFailedTransaction, assertValidatedTransactionCount, checkPredicateOptions,
                             defaultCheckOptions, w1)
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (BuiltinByteString, Datum (Datum), ScriptContext, ValidatorHash)
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError))
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Prelude hiding (not)

tests :: TestTree
tests =
    testGroup "MustSpendAtLeast"
        [ entireScriptBalance
        , lessThanScriptBalance
        --, higherThanScriptBalanceWithoutWalletPubkeyLookup -- Failing due to PLT-665
        --, higherThanScriptBalanceWithWalletPubkeyLookup    -- Failing due to PLT-665
        , phase2Failure
        ]

scriptBalance :: Integer
scriptBalance = 25_000_000

mustSpendAtLeastContract :: Integer -> Integer -> Ledger.PaymentPubKeyHash-> Contract () Empty ContractError ()
mustSpendAtLeastContract offAmt onAmt pkh = do
    let lookups1 = Constraints.typedValidatorLookups typedValidator
        tx1 = Constraints.mustPayToTheScriptWithDatumInTx
                onAmt
                (Ada.lovelaceValueOf scriptBalance)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt scrAddress
    let lookups2 = Constraints.typedValidatorLookups typedValidator
            <> Constraints.unspentOutputs utxos
            <> Constraints.ownPaymentPubKeyHash pkh
        tx2 =
            Constraints.collectFromTheScript utxos ()
            <> Constraints.mustIncludeDatumInTx (Datum $ PlutusTx.toBuiltinData onAmt)
            <> Constraints.mustSpendAtLeast (Ada.lovelaceValueOf offAmt)
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

trace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void $ Trace.waitNSlots 1

emptyPubKey :: Ledger.PaymentPubKeyHash
emptyPubKey = PlutusTx.unsafeFromBuiltinData $ PlutusTx.toBuiltinData ("" :: BuiltinByteString)

entireScriptBalance :: TestTree
entireScriptBalance =
    let contract = mustSpendAtLeastContract scriptBalance scriptBalance emptyPubKey
    in  checkPredicateOptions
            defaultCheckOptions
            "Successful use of mustSpendAtLeast at script's exact balance"
            (assertValidatedTransactionCount 2)
            (void $ trace contract)

lessThanScriptBalance :: TestTree
lessThanScriptBalance =
    let amt = scriptBalance - 1
        contract = mustSpendAtLeastContract amt amt emptyPubKey
    in  checkPredicateOptions
            defaultCheckOptions
            "Successful use of mustSpendAtLeast below script's balance"
            (assertValidatedTransactionCount 2)
            (void $ trace contract )

-- TODO: These two tests are failing due to PLT-665
{-
higherThanScriptBalanceWithWalletPubkeyLookup :: TestTree
higherThanScriptBalanceWithWalletPubkeyLookup =
    let amt = scriptBalance + 5_000_000
        contract = mustSpendAtLeastContract amt amt $ mockWalletPaymentPubKeyHash w1
    in  checkPredicateOptions
            defaultCheckOptions
            "Validation pass when mustSpendAtLeast is greater than script's balance and wallet's pubkey is included in the lookup"
            (assertValidatedTransactionCount 2)
            (void $ trace contract)

higherThanScriptBalanceWithoutWalletPubkeyLookup :: TestTree
higherThanScriptBalanceWithoutWalletPubkeyLookup =
    let amt = scriptBalance + 5_000_000
        contract = mustSpendAtLeastContract amt amt $ mockWalletPaymentPubKeyHash w6
    in  checkPredicateOptions
            defaultCheckOptions
            "Fail validation when mustSpendAtLeast is greater than script's balance and wallet's pubkey is not in the lookup"
            (assertContractError contract (Trace.walletInstanceTag w1) (\case { ConstraintResolutionContractError Constraints.OwnPubKeyMissing -> True; _ -> False }) "failed to throw error"
            .&&. assertValidatedTransactionCount 1)
            (void $ trace contract)
-}

phase2Failure :: TestTree
phase2Failure =
    let offAmt = scriptBalance
        onAmt  = scriptBalance + 1
        contract = mustSpendAtLeastContract offAmt onAmt emptyPubKey
    in  checkPredicateOptions
            defaultCheckOptions
            "Fail phase-2 validation when on-chain mustSpendAtLeast is greater than script's balance"
            (assertFailedTransaction (\_ err -> case err of {Ledger.ScriptFailure (EvaluationError ("L5":_) _) -> True; _ -> False }))
            (void $ trace contract)

{-# INLINEABLE mkValidator #-}
mkValidator :: Integer -> () -> ScriptContext -> Bool
mkValidator amt _ ctx = P.traceIfFalse "mustSpendAtLeast not satisfied" (Constraints.checkScriptContext @() @() (Constraints.mustSpendAtLeast P.$ Ada.lovelaceValueOf amt) ctx)

data UnitTest
instance Scripts.ValidatorTypes UnitTest where
    type instance DatumType UnitTest = Integer
    type instance RedeemerType UnitTest = ()

typedValidator :: Scripts.TypedValidator UnitTest
typedValidator = Scripts.mkTypedValidator @UnitTest
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

valHash :: ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = Ledger.scriptHashAddress valHash
