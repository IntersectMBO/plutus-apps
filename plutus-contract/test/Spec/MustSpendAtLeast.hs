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
import Data.Map qualified as Map
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)

import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints.OffChain qualified as Constraints (MkTxError (OwnPubKeyMissing), plutusV1OtherScript,
                                                             plutusV1TypedValidatorLookups, unspentOutputs)
import Ledger.Constraints.OnChain.V1 qualified as Constraints (checkScriptContext)
import Ledger.Constraints.TxConstraints qualified as Constraints (mustIncludeDatum, mustPayToTheScript,
                                                                  mustSpendAtLeast, mustSpendScriptOutput)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertContractError, assertValidatedTransactionCount, checkPredicateOptions,
                             defaultCheckOptions, w1, (.&&.))
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (Datum (Datum), ScriptContext, Validator, ValidatorHash)
import Plutus.V1.Ledger.Scripts (unitRedeemer)
import PlutusTx qualified
import PlutusTx.Prelude as P (traceError)
import Prelude hiding (not)

tests :: TestTree
tests =
    testGroup "MustSpendAtLeast"
        [ entireScriptBalance
        , lessThanScriptBalance
        , higherThanScriptBalance
        ]

scriptBalance :: Integer
scriptBalance = 25_000_000

contract :: Integer -> Contract () Empty ContractError ()
contract amt = do
    let lookups1 = Constraints.plutusV1TypedValidatorLookups $ typedValidator amt
        tx1 = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf scriptBalance)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1
    utxos <- utxosAt $ scrAddress amt
    let orefs = fst <$> Map.toList utxos
        lookups2 =
            Constraints.plutusV1OtherScript (validatorScript amt)
            <> Constraints.unspentOutputs utxos
        tx2 =
            foldMap (\oref -> Constraints.mustSpendScriptOutput oref unitRedeemer) orefs
            <> Constraints.mustIncludeDatum (Datum $ PlutusTx.toBuiltinData amt)
            <> Constraints.mustSpendAtLeast (Ada.lovelaceValueOf amt)
    ledgerTx2 <- submitTxConstraintsWith @Void lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

trace :: Integer -> Trace.EmulatorTrace ()
trace amt = do
    void $ Trace.activateContractWallet w1 $ contract amt
    void $ Trace.waitNSlots 1

entireScriptBalance :: TestTree
entireScriptBalance = checkPredicateOptions
    defaultCheckOptions
    "Successful use of mustSpendAtLeast at script's exact balance"
    (assertValidatedTransactionCount 2)
    (void $ trace scriptBalance)

lessThanScriptBalance :: TestTree
lessThanScriptBalance =
    let amt = scriptBalance - 1
    in  checkPredicateOptions
            defaultCheckOptions
            "Successful use of mustSpendAtLeast below script's balance"
            (assertValidatedTransactionCount 2)
            (void $ trace amt)

higherThanScriptBalance :: TestTree -- This should fail with a more accurate contract error than "OwnPubKeyMissing"
higherThanScriptBalance =
    let amt = scriptBalance + 1
    in  checkPredicateOptions
            defaultCheckOptions
            "Fail validation when mustSpendAtLeast is greater than script's balance"
            (assertContractError (contract amt) (Trace.walletInstanceTag w1) (\case { ConstraintResolutionContractError Constraints.OwnPubKeyMissing -> True; _ -> False }) "failed to throw error"
            .&&. assertValidatedTransactionCount 1)
            (void $ trace amt)

{-# INLINEABLE mkValidator #-}
mkValidator :: Integer -> () -> () -> ScriptContext -> Bool
mkValidator amt _ _ ctx = Constraints.checkScriptContext @Void @Void (Constraints.mustSpendAtLeast (Ada.lovelaceValueOf amt)) ctx || P.traceError "mustSpendAtLeast not satisfied"

data UnitTest
instance Scripts.ValidatorTypes UnitTest

typedValidator :: Integer -> Scripts.TypedValidator UnitTest
typedValidator = Scripts.mkTypedValidatorParam @UnitTest
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

validatorScript :: Integer -> Validator
validatorScript = Scripts.validatorScript . typedValidator

valHash :: Integer -> ValidatorHash
valHash = Scripts.validatorHash . typedValidator

scrAddress :: Integer -> Ledger.Address
scrAddress = Ledger.scriptHashAddress . valHash
