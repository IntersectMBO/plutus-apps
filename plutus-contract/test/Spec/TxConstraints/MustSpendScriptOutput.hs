{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.TxConstraints.MustSpendScriptOutput(tests) where

import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Data.List as L
import Data.Map as M
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints (TxConstraints)
import Ledger.Constraints.OffChain qualified as Constraints (MkTxError (NoMatchingOutputFound, TxOutRefWrongType),
                                                             plutusV1OtherScript, typedValidatorLookups, unspentOutputs)
import Ledger.Constraints.OnChain.V1 qualified as Constraints (checkScriptContext)
import Ledger.Constraints.TxConstraints qualified as Constraints (mustPayToTheScript, mustSpendScriptOutput,
                                                                  mustSpendScriptOutputWithMatchingDatumAndValue)
import Ledger.Test (asDatum, asRedeemer)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertContractError, assertFailedTransaction, assertValidatedTransactionCount,
                             checkPredicateOptions, defaultCheckOptions, valueAtAddress, w1, (.&&.))
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (ScriptContext, ValidatorHash)
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError))
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Prelude hiding (not)
import Wallet.Emulator.Wallet (mockWalletAddress)

tests :: TestTree
tests =
    testGroup "MustSpendScriptOutput"
        [ validUseOfMustSpendScriptOutputUsingAllScriptOutputs
        , validUseOfMustSpendScriptOutputUsingSomeScriptOutputs
        , contractErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef
        , phase2ErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef
        , phase2ErrorWhenMustSpendScriptOutputUsesWrongRedeemer
        , validUseOfMustSpendScriptOutputWithMatchingDatumAndValue
        , contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum
        , contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue
        , phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum
        , phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue
        , phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongRedeemer
        ]

utxoValue :: Value.Value
utxoValue = Ada.lovelaceValueOf 10_000_000

mustPayToTheScriptWithMultipleOutputs :: Integer -> [TxConstraints i Integer] -> TxConstraints i Integer
mustPayToTheScriptWithMultipleOutputs 0 constraints = mconcat constraints
mustPayToTheScriptWithMultipleOutputs n constraints = mustPayToTheScriptWithMultipleOutputs (n-1) (constraints ++ [Constraints.mustPayToTheScript (n-1) utxoValue])

-- | Contract to create multiple outputs at script address and then uses mustSpendScriptOutputs constraint to spend some of the outputs each with unique datum
mustSpendScriptOutputsContract :: Integer -> Integer -> Contract () Empty ContractError ()
mustSpendScriptOutputsContract nScriptOutputs nScriptOutputsToSpend = do
    let lookups1 = Constraints.typedValidatorLookups typedMustSpendScriptOutputValidator
        tx1 = mustPayToTheScriptWithMultipleOutputs nScriptOutputs []
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    scriptUtxos <- utxosAt mustSpendScriptOutputScrAddress
    let lookups2 = Constraints.typedValidatorLookups typedMustSpendScriptOutputValidator <>
            Constraints.unspentOutputs scriptUtxos
        tx2 = mconcat $ mustSpendScriptOutputs (M.keys $ M.take (fromIntegral nScriptOutputsToSpend) scriptUtxos)
    ledgerTx4 <- submitTxConstraintsWith @MustSpendScriptOutputType lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx4
    where
        mustSpendScriptOutputs :: [Tx.TxOutRef] -> [TxConstraints i o]
        mustSpendScriptOutputs scriptTxOutRefs = fmap (\txOutRef -> Constraints.mustSpendScriptOutput txOutRef (asRedeemer scriptTxOutRefs)) scriptTxOutRefs

mustSpendScriptOutputWithMatchingDatumAndValueContractWithRdmr :: Integer -> (Integer, Integer) -> (Value.Value, Value.Value) -> Redeemer -> Contract () Empty ContractError ()
mustSpendScriptOutputWithMatchingDatumAndValueContractWithRdmr nScriptOutputs (offChainMatchingDatum, _) (offChainMatchingValue, _) rdmr = do
    let lookups1 = Constraints.typedValidatorLookups typedMustSpendScriptOutputWithMatchingDatumAndValueValidator
        tx1 = mustPayToTheScriptWithMultipleOutputs nScriptOutputs []
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    scriptUtxos <- utxosAt mustSpendScriptOutputWithMatchingDatumAndValueScrAddress
    let lookups2 = Constraints.typedValidatorLookups typedMustSpendScriptOutputWithMatchingDatumAndValueValidator <>
            Constraints.unspentOutputs scriptUtxos
        tx2 = Constraints.mustSpendScriptOutputWithMatchingDatumAndValue mustSpendScriptOutputWithMatchingDatumAndValueValHash (\d -> d == asDatum offChainMatchingDatum) (\v -> v == offChainMatchingValue) rdmr
    ledgerTx4 <- submitTxConstraintsWith @MustSpendScriptOutputWithMatchingDatumAndValueType lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx4

-- | Contract to create multiple outputs at script address and then uses mustSpendScriptOutputWithMatchingDatumAndValue constraint to spend one of the outputs
mustSpendScriptOutputWithMatchingDatumAndValueContract :: Integer -> (Integer, Integer) -> (Value.Value, Value.Value) -> Contract () Empty ContractError ()
mustSpendScriptOutputWithMatchingDatumAndValueContract nScriptOutputs (offChainMatchingDatum, onChainMatchingDatum) (offChainMatchingValue, onChainMatchingValue) =
    mustSpendScriptOutputWithMatchingDatumAndValueContractWithRdmr nScriptOutputs (offChainMatchingDatum, onChainMatchingDatum) (offChainMatchingValue, onChainMatchingValue) (asRedeemer [(onChainMatchingDatum, onChainMatchingValue)])

trace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void $ Trace.waitNSlots 5

-- | Uses onchain and offchain constraint mustSpendScriptOutput to spend all UtxOs locked by the script
validUseOfMustSpendScriptOutputUsingAllScriptOutputs :: TestTree
validUseOfMustSpendScriptOutputUsingAllScriptOutputs =
    checkPredicateOptions
    defaultCheckOptions
    "Successful use of mustSpendScriptOutput for all script's UtxOs"
    (valueAtAddress mustSpendScriptOutputScrAddress (== Ada.lovelaceValueOf 0)
    .&&. assertValidatedTransactionCount 2)
    (void $ trace $ mustSpendScriptOutputsContract 5 5)

-- | Uses onchain and offchain constraint mustSpendScriptOutput to spend some of the UtxOs locked by the script
validUseOfMustSpendScriptOutputUsingSomeScriptOutputs :: TestTree
validUseOfMustSpendScriptOutputUsingSomeScriptOutputs =
    checkPredicateOptions
    defaultCheckOptions
    "Successful use of mustSpendScriptOutput for some of the script's UtxOs"
    (valueAtAddress mustSpendScriptOutputScrAddress (== Value.scale 2 utxoValue)
    .&&. assertValidatedTransactionCount 2)
    (void $ trace $ mustSpendScriptOutputsContract 5 3)

-- | Contract error occurs when offchain mustSpendScriptOutput constraint is used with a UTxO belonging to the w1 pubkey address, not the script (not of type ScriptChainIndexTxOut).
contractErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef :: TestTree
contractErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef =
    let contract :: Contract () Empty ContractError () = do
            let lookups1 = Constraints.typedValidatorLookups typedMustSpendScriptOutputValidator
                tx1 = mustPayToTheScriptWithMultipleOutputs 3 []
            ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

            scriptUtxos <- utxosAt mustSpendScriptOutputScrAddress
            w1Utxos <- utxosAt (mockWalletAddress w1)
            let w1Utxo = fst $ M.elemAt 2 w1Utxos
                lookups2 = Constraints.typedValidatorLookups typedMustSpendScriptOutputValidator <>
                    Constraints.unspentOutputs (scriptUtxos <> w1Utxos)
                tx2 = Constraints.mustSpendScriptOutput w1Utxo (asRedeemer [w1Utxo])
            ledgerTx4 <- submitTxConstraintsWith @MustSpendScriptOutputType lookups2 tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx4

    in checkPredicateOptions
        defaultCheckOptions
        "Contract error occurs when offchain mustSpendScriptOutput is used with a UTxO belonging to the w1 pubkey address, not the script."
        (assertContractError contract (Trace.walletInstanceTag w1) (\case ConstraintResolutionContractError (Constraints.TxOutRefWrongType _) -> True; _ -> False) "failed to throw error"
        .&&. assertValidatedTransactionCount 1)
        (void $ trace contract)

-- | Phase-2 validation failure when onchain mustSpendScriptOutput constraint expects a different TxOutRef belinging to the script
phase2ErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef :: TestTree
phase2ErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef =
    let contract :: Contract () Empty ContractError () = do
            let lookups1 = Constraints.typedValidatorLookups typedMustSpendScriptOutputValidator
                tx1 = mustPayToTheScriptWithMultipleOutputs 3 []
            ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

            scriptUtxos <- utxosAt mustSpendScriptOutputScrAddress
            let scriptUtxo1 = fst $ M.elemAt 0 scriptUtxos
                scriptUtxo2 = fst $ M.elemAt 1 scriptUtxos
                lookups2 = Constraints.plutusV1OtherScript mustSpendScriptOutputVal <>
                    Constraints.unspentOutputs scriptUtxos
                tx2 = Constraints.mustSpendScriptOutput scriptUtxo1 (asRedeemer [scriptUtxo2])
            ledgerTx4 <- submitTxConstraintsWith @MustSpendScriptOutputType lookups2 tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx4

    in checkPredicateOptions
        defaultCheckOptions
        "Phase-2 validation failure when onchain mustSpendScriptOutput constraint expects a different TxOutRef"
        (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("L8":_) _) -> True; _ -> False }))
        (void $ trace contract)

-- | Phase-2 validation failure when onchain mustSpendScriptOutput constraint expects a different redeeemer
phase2ErrorWhenMustSpendScriptOutputUsesWrongRedeemer :: TestTree
phase2ErrorWhenMustSpendScriptOutputUsesWrongRedeemer =
    let contract :: Contract () Empty ContractError () = do
            let lookups1 = Constraints.typedValidatorLookups typedMustSpendScriptOutputValidator
                tx1 = mustPayToTheScriptWithMultipleOutputs 3 []
            ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

            scriptUtxos <- utxosAt mustSpendScriptOutputScrAddress
            let scriptUtxo1 = fst $ M.elemAt 0 scriptUtxos
                scriptUtxo2 = fst $ M.elemAt 1 scriptUtxos
                lookups2 = Constraints.plutusV1OtherScript mustSpendScriptOutputVal <>
                    Constraints.unspentOutputs scriptUtxos
                tx2 = Constraints.mustSpendScriptOutput scriptUtxo2 (asRedeemer [scriptUtxo1])
            ledgerTx4 <- submitTxConstraintsWith @MustSpendScriptOutputType lookups2 tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx4

    in checkPredicateOptions
        defaultCheckOptions
        "Phase-2 validation failure when onchain mustSpendScriptOutput constraint expects a different redeemer"
        (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("L8":_) _) -> True; _ -> False }))
        (void $ trace contract)

-- | Uses onchain and offchain constraint mustSpendScriptOutputWithMatchingDatumAndValue to spend a UTxO locked by the script with matching datum and value
validUseOfMustSpendScriptOutputWithMatchingDatumAndValue :: TestTree
validUseOfMustSpendScriptOutputWithMatchingDatumAndValue =
    let nScriptOutputs  = 5
        scriptOutputIdx = nScriptOutputs - 1
    in checkPredicateOptions
        defaultCheckOptions
        "Successful use of mustSpendScriptOutputWithMatchingDatumAndValue to spend a UTxO locked by the script with matching datum and value"
        (valueAtAddress mustSpendScriptOutputWithMatchingDatumAndValueScrAddress (== Value.scale 4 utxoValue)
        .&&. assertValidatedTransactionCount 2)
        (void $ trace $ mustSpendScriptOutputWithMatchingDatumAndValueContract nScriptOutputs (scriptOutputIdx, scriptOutputIdx) (utxoValue, utxoValue))

-- | Contract error occurs when offchain mustSpendScriptOutputWithMatchingDatumAndValue constraint is used with a datum that does not match with one of the script's UTxOs
contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum :: TestTree
contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum =
    let nScriptOutputs  = 5
        wrongDatum = nScriptOutputs -- value is greater than any datum at script address
        contract = mustSpendScriptOutputWithMatchingDatumAndValueContract nScriptOutputs (wrongDatum, wrongDatum) (utxoValue, utxoValue)
    in checkPredicateOptions
        defaultCheckOptions
        "Contract error occurs when offchain mustSpendScriptOutputWithMatchingDatumAndValue constraint is used with a datum that cannot be matched"
        (assertContractError contract (Trace.walletInstanceTag w1) (\case ConstraintResolutionContractError (Constraints.NoMatchingOutputFound _) -> True; _ -> False) "failed to throw error"
        .&&. assertValidatedTransactionCount 1)
        (void $ trace contract)

-- | Contract error occurs when offchain mustSpendScriptOutputWithMatchingDatumAndValue constraint is used with a value that does not match with one of the script's UTxOs
contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue :: TestTree
contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue =
    let nScriptOutputs  = 5
        scriptOutputIdx = nScriptOutputs - 1
        wrongValue = Ada.lovelaceValueOf (1 + (Ada.getLovelace $ Ada.fromValue utxoValue))
        contract = mustSpendScriptOutputWithMatchingDatumAndValueContract nScriptOutputs (scriptOutputIdx, scriptOutputIdx) (wrongValue, wrongValue)
    in checkPredicateOptions
        defaultCheckOptions
        "Contract error occurs when offchain mustSpendScriptOutputWithMatchingDatumAndValue constraint is used with a value that cannot be matched"
        (assertContractError contract (Trace.walletInstanceTag w1) (\case ConstraintResolutionContractError (Constraints.NoMatchingOutputFound _) -> True; _ -> False) "failed to throw error"
        .&&. assertValidatedTransactionCount 1)
        (void $ trace contract)

-- | Phase-2 validation failure when onchain mustSpendScriptOutputWithMatchingDatumAndValue constraint expects a different Datum the tx input
phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum :: TestTree
phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum =
    let nScriptOutputs  = 5
        scriptOutputIdx = nScriptOutputs - 1
        wrongDatum = scriptOutputIdx - 1
    in checkPredicateOptions
        defaultCheckOptions
        "Phase-2 validation failure when onchain mustSpendScriptOutput constraint expects a different TxOutRef"
        (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("Le":_) _) -> True; _ -> False }))
        (void $ trace $ mustSpendScriptOutputWithMatchingDatumAndValueContract nScriptOutputs (scriptOutputIdx, wrongDatum) (utxoValue, utxoValue))

-- | Phase-2 validation failure when onchain mustSpendScriptOutputWithMatchingDatumAndValue constraint expects a different Value the tx input
phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue :: TestTree
phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue =
    let nScriptOutputs  = 5
        scriptOutputIdx = nScriptOutputs - 1
        wrongValue = Ada.lovelaceValueOf (1 + (Ada.getLovelace $ Ada.fromValue utxoValue))
    in checkPredicateOptions
        defaultCheckOptions
        "Phase-2 validation failure when onchain mustSpendScriptOutput constraint expects a different TxOutRef"
        (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("Le":_) _) -> True; _ -> False }))
        (void $ trace $ mustSpendScriptOutputWithMatchingDatumAndValueContract nScriptOutputs (scriptOutputIdx, scriptOutputIdx) (utxoValue, wrongValue))

phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongRedeemer :: TestTree
phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongRedeemer =
    let nScriptOutputs  = 5
        scriptOutputIdx = nScriptOutputs - 1
        wrongValue = Ada.lovelaceValueOf (1 + (Ada.getLovelace $ Ada.fromValue utxoValue))
        wrongRedeemer = (asRedeemer [(utxoValue, utxoValue)])
    in checkPredicateOptions
        defaultCheckOptions
        "Phase-2 validation failure when onchain mustSpendScriptOutput constraint expects a different redeemer"
        (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("Le":_) _) -> True; _ -> False }))
        (void $ trace $ mustSpendScriptOutputWithMatchingDatumAndValueContractWithRdmr nScriptOutputs (scriptOutputIdx, scriptOutputIdx) (utxoValue, wrongValue) wrongRedeemer)

{-# INLINEABLE mkMustSpendScriptOutputValidator #-}
mkMustSpendScriptOutputValidator :: Integer -> [Tx.TxOutRef] -> ScriptContext -> Bool
mkMustSpendScriptOutputValidator _ txOutRefs ctx = P.traceIfFalse "mustSpendScriptOutput not satisfied" (Constraints.checkScriptContext @() @() (P.mconcat mustSpendScriptOutputs) ctx)
    where
        mustSpendScriptOutputs = P.map (\txOutRef -> Constraints.mustSpendScriptOutput txOutRef (Ledger.Redeemer P.$ PlutusTx.toBuiltinData txOutRef)) txOutRefs

data MustSpendScriptOutputType
instance Scripts.ValidatorTypes MustSpendScriptOutputType where
    type instance DatumType MustSpendScriptOutputType = Integer
    type instance RedeemerType MustSpendScriptOutputType = [Tx.TxOutRef]

typedMustSpendScriptOutputValidator :: Scripts.TypedValidator MustSpendScriptOutputType
typedMustSpendScriptOutputValidator = Scripts.mkTypedValidator @MustSpendScriptOutputType
    $$(PlutusTx.compile [||mkMustSpendScriptOutputValidator||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

mustSpendScriptOutputVal :: Scripts.Validator
mustSpendScriptOutputVal = Scripts.validatorScript typedMustSpendScriptOutputValidator

mustSpendScriptOutputValHash :: ValidatorHash
mustSpendScriptOutputValHash = Scripts.validatorHash typedMustSpendScriptOutputValidator

mustSpendScriptOutputScrAddress :: Ledger.Address
mustSpendScriptOutputScrAddress = Ledger.scriptHashAddress mustSpendScriptOutputValHash

-----

data MustSpendScriptOutputWithMatchingDatumAndValueType
instance Scripts.ValidatorTypes MustSpendScriptOutputWithMatchingDatumAndValueType where
    type instance DatumType MustSpendScriptOutputWithMatchingDatumAndValueType = Integer
    type instance RedeemerType MustSpendScriptOutputWithMatchingDatumAndValueType = [(Ledger.Datum, Value.Value)]

{-# INLINEABLE mkMustSpendScriptOutputWithMatchingDatumAndValueValidator #-}
mkMustSpendScriptOutputWithMatchingDatumAndValueValidator :: Integer -> [(Ledger.Datum, Value.Value)] -> ScriptContext -> Bool
mkMustSpendScriptOutputWithMatchingDatumAndValueValidator _ datumsAndValues ctx = P.traceIfFalse "mustSpendScriptOutputWithMatchingDatumAndValue not satisfied" (Constraints.checkScriptContext @() @() (P.mconcat mustSpendScriptOutputsWithMatchingDatumAndValue) ctx)
    where
        mustSpendScriptOutputsWithMatchingDatumAndValue = P.map (\(datum, value) -> Constraints.mustSpendScriptOutputWithMatchingDatumAndValue (Ledger.ownHash ctx) (\d -> d P.== datum) (\v -> v P.==  value) (Ledger.Redeemer P.$ PlutusTx.toBuiltinData datumsAndValues)) datumsAndValues

typedMustSpendScriptOutputWithMatchingDatumAndValueValidator :: Scripts.TypedValidator MustSpendScriptOutputWithMatchingDatumAndValueType
typedMustSpendScriptOutputWithMatchingDatumAndValueValidator = Scripts.mkTypedValidator @MustSpendScriptOutputWithMatchingDatumAndValueType
    $$(PlutusTx.compile [||mkMustSpendScriptOutputWithMatchingDatumAndValueValidator||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

mustSpendScriptOutputWithMatchingDatumAndValueVal :: Scripts.Validator
mustSpendScriptOutputWithMatchingDatumAndValueVal = Scripts.validatorScript typedMustSpendScriptOutputWithMatchingDatumAndValueValidator

mustSpendScriptOutputWithMatchingDatumAndValueValHash :: ValidatorHash
mustSpendScriptOutputWithMatchingDatumAndValueValHash = Scripts.validatorHash typedMustSpendScriptOutputWithMatchingDatumAndValueValidator

mustSpendScriptOutputWithMatchingDatumAndValueScrAddress :: Ledger.Address
mustSpendScriptOutputWithMatchingDatumAndValueScrAddress = Ledger.scriptHashAddress mustSpendScriptOutputWithMatchingDatumAndValueValHash
