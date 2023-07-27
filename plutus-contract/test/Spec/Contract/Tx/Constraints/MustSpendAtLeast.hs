{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.Contract.Tx.Constraints.MustSpendAtLeast(tests) where

import Control.Lens (has)
import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Cardano.Node.Emulator.Internal.Node.Params qualified as Params
import Ledger qualified
import Ledger.Tx qualified as Tx
import Ledger.Tx.Constraints.OffChain qualified as Constraints
import Ledger.Tx.Constraints.OnChain.V1 qualified as Constraints
import Ledger.Tx.Constraints.TxConstraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertContractError, assertEvaluationError, assertValidatedTransactionCount,
                             checkPredicateOptions, defaultCheckOptions, w1, (.&&.))
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Typed qualified as Typed
import Plutus.Trace.Emulator qualified as Trace (EmulatorTrace, activateContractWallet, nextSlot, walletInstanceTag)
import PlutusLedgerApi.V1 (Datum (Datum), ScriptContext, ValidatorHash)
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Prelude hiding (not)

tests :: TestTree
tests = testGroup "MustSpendAtLeast"
    [ entireScriptBalance
    , lessThanScriptBalance
    , higherThanScriptBalance
    , phase2Failure
    ]

scriptBalance :: Integer
scriptBalance = 25_000_000

mustSpendAtLeastContract :: Integer -> Integer -> Contract () Empty ContractError ()
mustSpendAtLeastContract offAmt onAmt = do
    params <- getParams
    let lookups1 = Constraints.typedValidatorLookups typedValidator
        tx1 = Constraints.mustPayToTheScriptWithDatumInTx
                onAmt
                (Ada.lovelaceValueOf scriptBalance)
    ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt $ scrAddress (Params.pNetworkId params)
    let lookups2 = Constraints.typedValidatorLookups typedValidator
            <> Constraints.unspentOutputs utxos
        tx2 =
            Constraints.spendUtxosFromTheScript utxos ()
            <> Constraints.mustIncludeDatumInTx (Datum $ PlutusTx.toBuiltinData onAmt)
            <> Constraints.mustSpendAtLeast (Ada.lovelaceValueOf offAmt)
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

trace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void Trace.nextSlot

entireScriptBalance :: TestTree
entireScriptBalance =
    let contract = mustSpendAtLeastContract scriptBalance scriptBalance
    in  checkPredicateOptions
            defaultCheckOptions
            "Successful use of mustSpendAtLeast at script's exact balance"
            (assertValidatedTransactionCount 2)
            (void $ trace contract)

lessThanScriptBalance :: TestTree
lessThanScriptBalance =
    let amt = scriptBalance - 1
        contract = mustSpendAtLeastContract amt amt
    in  checkPredicateOptions
            defaultCheckOptions
            "Successful use of mustSpendAtLeast below script's balance"
            (assertValidatedTransactionCount 2)
            (void $ trace contract )

higherThanScriptBalance :: TestTree
higherThanScriptBalance =
    let amt = scriptBalance + 5_000_000
        contract = mustSpendAtLeastContract amt amt
    in  checkPredicateOptions
            defaultCheckOptions
            "Validation pass when mustSpendAtLeast is greater than script's balance and wallet's pubkey is included in the lookup"
            (assertContractError contract (Trace.walletInstanceTag w1)
                (has (_ConstraintResolutionContractError . Constraints._DeclaredInputMismatch)) "failed to throw error"
            .&&. assertValidatedTransactionCount 1)
            (void $ trace contract)

phase2Failure :: TestTree
phase2Failure =
    let offAmt = scriptBalance
        onAmt  = scriptBalance + 1
        contract = mustSpendAtLeastContract offAmt onAmt
    in  checkPredicateOptions
            defaultCheckOptions
            "Fail phase-2 validation when on-chain mustSpendAtLeast is greater than script's balance"
            (assertEvaluationError "L5")
            (void $ trace contract)

{-# INLINEABLE mkValidator #-}
mkValidator :: Integer -> () -> ScriptContext -> Bool
mkValidator amt _ ctx = P.traceIfFalse "mustSpendAtLeast not satisfied"
  (Constraints.checkScriptContext @() @() (Constraints.mustSpendAtLeast P.$ Ada.lovelaceValueOf amt) ctx)

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

scrAddress :: Ledger.NetworkId -> Ledger.CardanoAddress
scrAddress = flip Typed.validatorCardanoAddress typedValidator
