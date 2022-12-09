{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.Contract.Tx.Constraints.MustSpendAtLeast(tests) where

import Control.Lens (review, (??), (^.))
import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Cardano.Node.Emulator.Params qualified as Params
import Ledger qualified
import Ledger.Constraints.OffChain qualified as Constraints
import Ledger.Constraints.OnChain.V1 qualified as Constraints
import Ledger.Constraints.TxConstraints qualified as Constraints
import Ledger.Tx qualified as Tx
import Ledger.Tx.Constraints qualified as Tx.Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertContractError, assertEvaluationError, assertValidatedTransactionCount,
                             checkPredicateOptions, defaultCheckOptions, emulatorConfig, w1, (.&&.))
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Typed qualified as Typed
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (Datum (Datum), ScriptContext, ValidatorHash)
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Prelude hiding (not)

tests :: TestTree
tests =
    testGroup "MustSpendAtLeast"
        [ testGroup "ledger constraints" $ tests' ledgerSubmitTx
        , testGroup "cardano constraints" $ tests' cardanoSubmitTx
        ]

tests' :: SubmitTx -> [TestTree]
tests' sub =
    [ entireScriptBalance
    , lessThanScriptBalance
    , higherThanScriptBalance
    , phase2Failure
    ] ?? sub

scriptBalance :: Integer
scriptBalance = 25_000_000

mustSpendAtLeastContract :: SubmitTx -> Integer -> Integer -> Contract () Empty ContractError ()
mustSpendAtLeastContract submitTxFromConstraints offAmt onAmt = do
    params <- getParams
    let lookups1 = Constraints.typedValidatorLookups typedValidator
        tx1 = Constraints.mustPayToTheScriptWithDatumInTx
                onAmt
                (Ada.lovelaceValueOf scriptBalance)
    ledgerTx1 <- submitTxFromConstraints lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt $ scrAddress (Params.pNetworkId params)
    let lookups2 = Constraints.typedValidatorLookups typedValidator
            <> Constraints.unspentOutputs utxos
        tx2 =
            Constraints.collectFromTheScript utxos ()
            <> Constraints.mustIncludeDatumInTx (Datum $ PlutusTx.toBuiltinData onAmt)
            <> Constraints.mustSpendAtLeast (Ada.lovelaceValueOf offAmt)
    ledgerTx2 <- submitTxFromConstraints lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

trace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void Trace.nextSlot

entireScriptBalance :: SubmitTx -> TestTree
entireScriptBalance sub =
    let contract = mustSpendAtLeastContract sub scriptBalance scriptBalance
    in  checkPredicateOptions
            defaultCheckOptions
            "Successful use of mustSpendAtLeast at script's exact balance"
            (assertValidatedTransactionCount 2)
            (void $ trace contract)

lessThanScriptBalance :: SubmitTx -> TestTree
lessThanScriptBalance sub =
    let amt = scriptBalance - 1
        contract = mustSpendAtLeastContract sub amt amt
    in  checkPredicateOptions
            defaultCheckOptions
            "Successful use of mustSpendAtLeast below script's balance"
            (assertValidatedTransactionCount 2)
            (void $ trace contract )

higherThanScriptBalance :: SubmitTx -> TestTree
higherThanScriptBalance sub =
    let amt = scriptBalance + 5_000_000
        contract = mustSpendAtLeastContract sub amt amt
    in  checkPredicateOptions
            defaultCheckOptions
            "Validation pass when mustSpendAtLeast is greater than script's balance and wallet's pubkey is included in the lookup"
            (assertContractError contract (Trace.walletInstanceTag w1)
                (\case
                    { ConstraintResolutionContractError (Constraints.DeclaredInputMismatch _) -> True
                    ; TxConstraintResolutionContractError (Tx.Constraints.LedgerMkTxError (Constraints.DeclaredInputMismatch _)) -> True
                    ; _ -> False }) "failed to throw error"
            .&&. assertValidatedTransactionCount 1)
            (void $ trace contract)

phase2Failure :: SubmitTx -> TestTree
phase2Failure sub =
    let offAmt = scriptBalance
        onAmt  = scriptBalance + 1
        contract = mustSpendAtLeastContract sub offAmt onAmt
    in  checkPredicateOptions
            defaultCheckOptions
            "Fail phase-2 validation when on-chain mustSpendAtLeast is greater than script's balance"
            (assertEvaluationError "L5")
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

scrAddress :: Ledger.NetworkId -> Ledger.CardanoAddress
scrAddress = flip Typed.validatorCardanoAddress typedValidator

type SubmitTx
  =  Constraints.ScriptLookups UnitTest
  -> Constraints.TxConstraints (Scripts.RedeemerType UnitTest) (Scripts.DatumType UnitTest)
  -> Contract () Empty ContractError Tx.CardanoTx

cardanoSubmitTx :: SubmitTx
cardanoSubmitTx lookups tx = let
  p = defaultCheckOptions ^. emulatorConfig . Trace.params
  tx' = Tx.Constraints.mkTx @UnitTest p lookups tx
  in submitUnbalancedTx =<<
    (mapError (review _TxConstraintResolutionContractError) $ either throwError pure tx')

ledgerSubmitTx :: SubmitTx
ledgerSubmitTx = submitTxConstraintsWith
