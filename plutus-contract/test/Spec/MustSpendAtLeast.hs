{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.MustSpendAtLeast(tests) where

import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints.OffChain qualified as Constraints (MkTxError (OwnPubKeyMissing), plutusV1OtherScript,
                                                             plutusV1TypedValidatorLookups, unspentOutputs)
import Ledger.Constraints.OnChain.V1 qualified as Constraints (checkScriptContext)
import Ledger.Constraints.TxConstraints qualified as Constraints (collectFromTheScript, mustIncludeDatum,
                                                                  mustPayToTheScript, mustSpendAtLeast)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertContractError, assertFailedTransaction, assertValidatedTransactionCount,
                             checkPredicateOptions, defaultCheckOptions, w1, (.&&.))
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (Datum (Datum), ScriptContext, Validator, ValidatorHash)
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError))
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Prelude hiding (not)

tests :: TestTree
tests =
    testGroup "MustSpendAtLeast"
        [ entireScriptBalance
        , lessThanScriptBalance
        , higherThanScriptBalance
        , phase2Failure
        ]

scriptBalance :: Integer
scriptBalance = 25_000_000

contract :: Integer -> Integer -> Contract () Empty ContractError ()
contract offAmt onAmt = do
    let lookups1 = Constraints.plutusV1TypedValidatorLookups typedValidator
        tx1 = Constraints.mustPayToTheScript onAmt (Ada.lovelaceValueOf scriptBalance)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt scrAddress
    let lookups2 = Constraints.plutusV1TypedValidatorLookups typedValidator
            <> Constraints.plutusV1OtherScript validatorScript
            <> Constraints.unspentOutputs utxos
        tx2 =
            Constraints.collectFromTheScript utxos ()
            <> Constraints.mustIncludeDatum (Datum $ PlutusTx.toBuiltinData onAmt)
            <> Constraints.mustSpendAtLeast (Ada.lovelaceValueOf offAmt)
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

trace :: Integer -> Integer -> Trace.EmulatorTrace ()
trace offAmt onAmt = do
    void $ Trace.activateContractWallet w1 $ contract offAmt onAmt
    void $ Trace.waitNSlots 1

entireScriptBalance :: TestTree
entireScriptBalance = checkPredicateOptions
    defaultCheckOptions
    "Successful use of mustSpendAtLeast at script's exact balance"
    (assertValidatedTransactionCount 2)
    (void $ trace scriptBalance scriptBalance)

lessThanScriptBalance :: TestTree
lessThanScriptBalance =
    let amt = scriptBalance - 1
    in  checkPredicateOptions
            defaultCheckOptions
            "Successful use of mustSpendAtLeast below script's balance"
            (assertValidatedTransactionCount 2)
            (void $ trace amt amt )

higherThanScriptBalance :: TestTree
higherThanScriptBalance =
    let amt = scriptBalance + 1
    in  checkPredicateOptions
            defaultCheckOptions
            "Fail validation when mustSpendAtLeast is greater than script's balance"
            (assertContractError (contract amt amt) (Trace.walletInstanceTag w1) (\case { ConstraintResolutionContractError Constraints.OwnPubKeyMissing -> True; _ -> False }) "failed to throw error"
            .&&. assertValidatedTransactionCount 1)
            (void $ trace amt amt)

phase2Failure :: TestTree
phase2Failure =
    let offAmt = scriptBalance
        onAmt  = scriptBalance + 1
    in  checkPredicateOptions
            defaultCheckOptions
            "Fail phase-2 validation when on-chain mustSpendAtLeast is greater than script's balance"
            (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("L5":_) _) -> True; _ -> False }))
            (void $ trace offAmt onAmt)

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

validatorScript :: Validator
validatorScript = Scripts.validatorScript typedValidator

valHash :: ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = Ledger.scriptHashAddress valHash
