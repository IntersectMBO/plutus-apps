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
                                                             plutusV1MintingPolicy, plutusV1OtherScript,
                                                             typedValidatorLookups, unspentOutputs)
import Ledger.Constraints.OnChain.V1 qualified as Constraints (checkScriptContext)
import Ledger.Constraints.TxConstraints qualified as Constraints (mustMintValue, mustMintValueWithRedeemer,
                                                                  mustPayToTheScript, mustSpendScriptOutput,
                                                                  mustSpendScriptOutputWithMatchingDatumAndValue)
import Ledger.Test (asDatum, asRedeemer, someAddress, someTypedValidator, someValidator, someValidatorHash)
import Ledger.Tx qualified as Tx
import Plutus.Contract as Con (Contract, ContractError (ConstraintResolutionContractError), Empty, awaitTxConfirmed,
                               submitTxConstraintsWith, utxosAt)
import Plutus.Contract.Test (assertContractError, assertFailedTransaction, assertValidatedTransactionCount,
                             checkPredicateOptions, defaultCheckOptions, valueAtAddress, w1, (.&&.))
import Plutus.Script.Utils.Scripts as PSU
import Plutus.Script.Utils.Typed as PSU
import Plutus.Script.Utils.V1.Scripts as PSU.V1 hiding (validatorHash)
import Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api qualified as PV1
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError), ValidatorHash, unitRedeemer)
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api qualified as PV2
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
        , noPhase2ErrorWhenV1ScriptUsingMustSpendScriptOutputUsesWrongRedeemer
        --, phase2ErrorWhenV2ScriptUsingMustSpendScriptOutputUsesWrongRedeemer
        , validUseOfMustSpendScriptOutputWithMatchingDatumAndValue
        , contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum
        , contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue
        , phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum
        , phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue
        , noPhase2ErrorWhenV1ScriptUsingMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongRedeemer
        --, phase2ErrorWhenV2ScriptUsingMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongRedeemer
        ]

data UnitTest
instance PSU.ValidatorTypes UnitTest

-- The value in each initial wallet UTxO
utxoValue :: Value.Value
utxoValue = Ada.lovelaceValueOf 10_000_000

mustSpendScriptOutputTokenValue :: Value.Value
mustSpendScriptOutputTokenValue = tokenValue mustSpendScriptOutputPolicyCurrencySymbolV1

mustSpendScriptOutputWithMatchingDatumAndValueTokenValue :: Value.Value
mustSpendScriptOutputWithMatchingDatumAndValueTokenValue = tokenValue mustSpendScriptOutputWithMatchingDatumAndValuePolicyCurrencySymbolV1

tokenValue :: Value.CurrencySymbol -> Value.Value
tokenValue cs = Value.singleton cs "A" 1

mustPayToTheScriptWithMultipleOutputs :: Integer -> [TxConstraints i P.BuiltinData] -> TxConstraints i P.BuiltinData
mustPayToTheScriptWithMultipleOutputs 0 constraints = mconcat constraints
mustPayToTheScriptWithMultipleOutputs n constraints = mustPayToTheScriptWithMultipleOutputs (n-1) (constraints ++ [Constraints.mustPayToTheScript (PlutusTx.toBuiltinData (n-1)) utxoValue])

-- | Contract to create multiple outputs at script address and then uses mustSpendScriptOutputs constraint to spend some of the outputs each with unique datum
mustSpendScriptOutputsContract :: Integer -> Integer -> Bool -> Contract () Empty ContractError ()
mustSpendScriptOutputsContract nScriptOutputs nScriptOutputsToSpend withExpectedRedeemers = do
    let lookups1 = Constraints.typedValidatorLookups someTypedValidator
        tx1 = mustPayToTheScriptWithMultipleOutputs nScriptOutputs []
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    scriptUtxos <- utxosAt someAddress
    let scriptUtxosToSpend = M.keys $ M.take (fromIntegral nScriptOutputsToSpend) scriptUtxos
        expectedRedeemers = L.map (\(a :: Tx.TxOutRef) -> if withExpectedRedeemers then asRedeemer a else asRedeemer a{PV1.txOutRefIdx = PV1.txOutRefIdx a + 1}) scriptUtxosToSpend
        policyRedeemer = asRedeemer $ zip scriptUtxosToSpend expectedRedeemers
        lookups2 = Constraints.typedValidatorLookups someTypedValidator
                <> Constraints.plutusV1MintingPolicy mustSpendScriptOutputPolicyV1
                <> Constraints.unspentOutputs scriptUtxos
        tx2 = mconcat (mustSpendScriptOutputs scriptUtxosToSpend)
           <> Constraints.mustMintValueWithRedeemer policyRedeemer mustSpendScriptOutputTokenValue
    ledgerTx4 <- submitTxConstraintsWith lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx4
    where
        mustSpendScriptOutputs :: [Tx.TxOutRef] -> [TxConstraints P.BuiltinData P.BuiltinData]
        mustSpendScriptOutputs scriptTxOutRefs = fmap (\txOutRef -> Constraints.mustSpendScriptOutput txOutRef (asRedeemer txOutRef)) scriptTxOutRefs

mustSpendScriptOutputUsingWrongRedeemerContract :: Integer -> Contract () Empty ContractError ()
mustSpendScriptOutputUsingWrongRedeemerContract nScriptOutputsToSpend = do
    let lookups1 = Constraints.typedValidatorLookups someTypedValidator
        tx1 = mustPayToTheScriptWithMultipleOutputs nScriptOutputsToSpend []
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    scriptUtxos <- utxosAt someAddress
    let scriptUtxo1 = fst $ M.elemAt 0 scriptUtxos
        scriptUtxo2 = fst $ M.elemAt 1 scriptUtxos
        policyRedeemer = asRedeemer [(scriptUtxo1, asRedeemer scriptUtxo2)]
        lookups2 = Constraints.typedValidatorLookups someTypedValidator
                <> Constraints.plutusV1MintingPolicy mustSpendScriptOutputPolicyV1
                <> Constraints.unspentOutputs scriptUtxos
        tx2 = Constraints.mustSpendScriptOutput scriptUtxo1 unitRedeemer
            <> Constraints.mustMintValueWithRedeemer policyRedeemer mustSpendScriptOutputTokenValue
    ledgerTx4 <- submitTxConstraintsWith lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx4

mustSpendScriptOutputWithMatchingDatumAndValueContractWithRdmr :: Integer -> (Integer, Integer) -> (Value.Value, Value.Value) -> (Ledger.Redeemer, Ledger.Redeemer) -> Contract () Empty ContractError ()
mustSpendScriptOutputWithMatchingDatumAndValueContractWithRdmr nScriptOutputs (offChainMatchingDatum, onChainMatchingDatum) (offChainMatchingValue, onChainMatchingValue) (spendingRedeemer, expectedSpendingRedeemer) = do
    let lookups1 = Constraints.typedValidatorLookups someTypedValidator
        tx1 = mustPayToTheScriptWithMultipleOutputs nScriptOutputs []
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    scriptUtxos <- utxosAt someAddress
    let policyRedeemer = asRedeemer [(someValidatorHash, onChainMatchingDatum, onChainMatchingValue, expectedSpendingRedeemer)]
    let lookups2 = Constraints.typedValidatorLookups someTypedValidator
                <> Constraints.plutusV1MintingPolicy mustSpendScriptOutputWithMatchingDatumAndValuePolicyV1
                <> Constraints.unspentOutputs scriptUtxos
        tx2 = Constraints.mustSpendScriptOutputWithMatchingDatumAndValue someValidatorHash (\d -> d == asDatum offChainMatchingDatum) (\v -> v == offChainMatchingValue) spendingRedeemer
           <> Constraints.mustMintValueWithRedeemer policyRedeemer mustSpendScriptOutputWithMatchingDatumAndValueTokenValue
    ledgerTx4 <- submitTxConstraintsWith lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx4

-- | Contract to create multiple outputs at script address and then uses mustSpendScriptOutputWithMatchingDatumAndValue constraint to spend one of the outputs
mustSpendScriptOutputWithMatchingDatumAndValueContract :: Integer -> (Integer, Integer) -> (Value.Value, Value.Value) -> Contract () Empty ContractError ()
mustSpendScriptOutputWithMatchingDatumAndValueContract nScriptOutputs (offChainMatchingDatum, onChainMatchingDatum) (offChainMatchingValue, onChainMatchingValue) =
    mustSpendScriptOutputWithMatchingDatumAndValueContractWithRdmr nScriptOutputs (offChainMatchingDatum, onChainMatchingDatum) (offChainMatchingValue, onChainMatchingValue) (asRedeemer @P.BuiltinByteString "R", asRedeemer @P.BuiltinByteString "R")

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
    (valueAtAddress someAddress (== Ada.lovelaceValueOf 0)
    .&&. assertValidatedTransactionCount 2)
    (void $ trace $ mustSpendScriptOutputsContract 5 5 True)

-- | Uses onchain and offchain constraint mustSpendScriptOutput to spend some of the UtxOs locked by the script
validUseOfMustSpendScriptOutputUsingSomeScriptOutputs :: TestTree
validUseOfMustSpendScriptOutputUsingSomeScriptOutputs =
    checkPredicateOptions
    defaultCheckOptions
    "Successful use of mustSpendScriptOutput for some of the script's UtxOs"
    (valueAtAddress someAddress (== Value.scale 2 utxoValue)
    .&&. assertValidatedTransactionCount 2)
    (void $ trace $ mustSpendScriptOutputsContract 5 3 True)

-- | Contract error occurs when offchain mustSpendScriptOutput constraint is used with a UTxO belonging to the w1 pubkey address, not the script (not of type ScriptChainIndexTxOut).
contractErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef :: TestTree
contractErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef =
    let contract :: Contract () Empty ContractError () = do
            let lookups1 = Constraints.typedValidatorLookups someTypedValidator
                tx1 = mustPayToTheScriptWithMultipleOutputs 3 []
            ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

            scriptUtxos <- utxosAt someAddress
            w1Utxos <- utxosAt (mockWalletAddress w1)
            let w1Utxo = fst $ M.elemAt 2 w1Utxos
                lookups2 = Constraints.typedValidatorLookups someTypedValidator <>
                    Constraints.unspentOutputs (scriptUtxos <> w1Utxos)
                tx2 = Constraints.mustSpendScriptOutput w1Utxo unitRedeemer
            ledgerTx4 <- submitTxConstraintsWith lookups2 tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx4

    in checkPredicateOptions
        defaultCheckOptions
        "Contract error occurs when offchain mustSpendScriptOutput is used with a UTxO belonging to the w1 pubkey address, not the script."
        (assertContractError contract (Trace.walletInstanceTag w1) (\case ConstraintResolutionContractError (Constraints.TxOutRefWrongType _) -> True; _ -> False) "failed to throw error"
        .&&. assertValidatedTransactionCount 1)
        (void $ trace contract)

-- | Phase-2 validation failure when onchain mustSpendScriptOutput constraint expects a different TxOutRef belonging to the script
phase2ErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef :: TestTree
phase2ErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef =
    let contract :: Contract () Empty ContractError () = do
            let lookups1 = Constraints.typedValidatorLookups someTypedValidator
                tx1 = mustPayToTheScriptWithMultipleOutputs 3 []
            ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

            scriptUtxos <- utxosAt someAddress
            let scriptUtxo1 = fst $ M.elemAt 0 scriptUtxos
                scriptUtxo2 = fst $ M.elemAt 1 scriptUtxos
                policyRedeemer = asRedeemer [(scriptUtxo2, asRedeemer scriptUtxo2)]
                lookups2 = Constraints.typedValidatorLookups someTypedValidator
                        <> Constraints.plutusV1MintingPolicy mustSpendScriptOutputPolicyV1
                        <> Constraints.unspentOutputs scriptUtxos
                tx2 = Constraints.mustSpendScriptOutput scriptUtxo1 unitRedeemer
                   <> Constraints.mustMintValueWithRedeemer policyRedeemer mustSpendScriptOutputTokenValue
            ledgerTx4 <- submitTxConstraintsWith lookups2 tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx4

    in checkPredicateOptions
        defaultCheckOptions
        "Phase-2 validation failure when onchain mustSpendScriptOutput constraint expects a different TxOutRef"
        (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("L8":_) _) -> True; _ -> False }))
        (void $ trace contract)

-- | No phase-2 validation failure when V1 script using onchain mustSpendScriptOutput constraint expects a different redeeemer (check is only V2+)
noPhase2ErrorWhenV1ScriptUsingMustSpendScriptOutputUsesWrongRedeemer :: TestTree
noPhase2ErrorWhenV1ScriptUsingMustSpendScriptOutputUsesWrongRedeemer =
    checkPredicateOptions
    defaultCheckOptions
    "No phase-2 validation failure when V1 script using onchain mustSpendScriptOutput constraint expects a different redeemer"
    (valueAtAddress someAddress (== Value.scale 2 utxoValue)
    .&&. assertValidatedTransactionCount 2)
    (void $ trace $ mustSpendScriptOutputUsingWrongRedeemerContract 3)

-- | Phase-2 validation failure when V2 script using onchain mustSpendScriptOutput constraint expects a different redeeemer
-- phase2ErrorWhenV2MustSpendScriptOutputUsesWrongRedeemer :: TestTree
-- phase2ErrorWhenV2MustSpendScriptOutputUsesWrongRedeemer =
--     let contract :: Contract () Empty ContractError () = do
--             let lookups1 = Constraints.typedValidatorLookups someTypedValidator
--                 tx1 = mustPayToTheScriptWithMultipleOutputs 3 []
--             ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
--             awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

--             scriptUtxos <- utxosAt someAddress
--             let scriptUtxo1 = fst $ M.elemAt 0 scriptUtxos
--                 scriptUtxo2 = fst $ M.elemAt 1 scriptUtxos
--                 policyRedeemer = asRedeemer [(scriptUtxo1, asRedeemer scriptUtxo2)]
--                 lookups2 = Constraints.typedValidatorLookups someTypedValidator
--                         <> Constraints.plutusV1MintingPolicy mustSpendScriptOutputPolicyV1
--                         <> Constraints.unspentOutputs scriptUtxos
--                 tx2 = Constraints.mustSpendScriptOutput scriptUtxo1 unitRedeemer
--                    <> Constraints.mustMintValueWithRedeemer policyRedeemer mustSpendScriptOutputTokenValue
--             ledgerTx4 <- submitTxConstraintsWith lookups2 tx2
--             awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx4

--     in checkPredicateOptions
--         defaultCheckOptions
--         "Phase-2 validation failure when V2 script using onchain mustSpendScriptOutput constraint expects a different redeemer"
--         (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("L8":_) _) -> True; _ -> False }))
--         (void $ trace contract)

-- | Uses onchain and offchain constraint mustSpendScriptOutputWithMatchingDatumAndValue to spend a UTxO locked by the script with matching datum and value
validUseOfMustSpendScriptOutputWithMatchingDatumAndValue :: TestTree
validUseOfMustSpendScriptOutputWithMatchingDatumAndValue =
    let nScriptOutputs  = 5
        scriptOutputIdx = nScriptOutputs - 1
    in checkPredicateOptions
        defaultCheckOptions
        "Successful use of mustSpendScriptOutputWithMatchingDatumAndValue to spend a UTxO locked by the script with matching datum and value"
        (valueAtAddress someAddress (== Value.scale 4 utxoValue)
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
        "Phase-2 validation failure when onchain mustSpendScriptOutputWithMatchingDatumAndValue constraint expects a different TxOutRef"
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
        "Phase-2 validation failure when onchain mustSpendScriptOutputWithMatchingDatumAndValue constraint expects a different TxOutRef"
        (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("Le":_) _) -> True; _ -> False }))
        (void $ trace $ mustSpendScriptOutputWithMatchingDatumAndValueContract nScriptOutputs (scriptOutputIdx, scriptOutputIdx) (utxoValue, wrongValue))

noPhase2ErrorWhenV1ScriptUsingMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongRedeemer :: TestTree
noPhase2ErrorWhenV1ScriptUsingMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongRedeemer =
    let nScriptOutputs  = 5
        scriptOutputIdx = nScriptOutputs - 1
        spendingRedeemer = asRedeemer @Integer 42
        policyRedeemer = asRedeemer @Integer 41
    in checkPredicateOptions
        defaultCheckOptions
        "No phase-2 validation failure when V1 script using onchain mustSpendScriptOutputWithMatchingDatumAndValue constraint expects a different redeemer"
        (valueAtAddress someAddress (== Value.scale 4 utxoValue)
        .&&. assertValidatedTransactionCount 2)
        (void $ trace $ mustSpendScriptOutputWithMatchingDatumAndValueContractWithRdmr nScriptOutputs (scriptOutputIdx, scriptOutputIdx) (utxoValue, utxoValue) (spendingRedeemer, policyRedeemer))

-- phase2ErrorWhenV1ScriptUsingMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongRedeemer :: TestTree
-- phase2ErrorWhenV1ScriptUsingMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongRedeemer =
--     let nScriptOutputs  = 5
--         scriptOutputIdx = nScriptOutputs - 1
--         spendingRedeemer = asRedeemer @Integer 42
--         policyRedeemer = asRedeemer @Integer 41
--     in checkPredicateOptions
--         defaultCheckOptions
--         "Phase-2 validation failure when V2 script using onchain mustSpendScriptOutputWithMatchingDatumAndValue constraint expects a different redeemer"
--         (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("Le":_) _) -> True; _ -> False }))
--         (void $ trace $ mustSpendScriptOutputWithMatchingDatumAndValueContractWithRdmr nScriptOutputs (scriptOutputIdx, scriptOutputIdx) (utxoValue, utxoValue) (spendingRedeemer, policyRedeemer))

{-
   Plutus V1 Policies
-}

{-# INLINEABLE mkMustSpendScriptOutputPolicyV1 #-}
mkMustSpendScriptOutputPolicyV1 :: [(Tx.TxOutRef,  Redeemer)] -> PV1.ScriptContext -> Bool
mkMustSpendScriptOutputPolicyV1 constraintParams ctx = P.traceIfFalse "mustSpendScriptOutput not satisfied" (Constraints.checkScriptContext @() @() (P.mconcat mustSpendScriptOutputs) ctx)
    where
        mustSpendScriptOutputs = P.map (\(txOutRef, redeemer) -> Constraints.mustSpendScriptOutput txOutRef redeemer) constraintParams

mustSpendScriptOutputPolicyV1 :: PSU.V1.MintingPolicy
mustSpendScriptOutputPolicyV1 = PV1.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        wrap = PSU.V1.mkUntypedMintingPolicy mkMustSpendScriptOutputPolicyV1

mustSpendScriptOutputPolicyHashV1 :: Ledger.MintingPolicyHash
mustSpendScriptOutputPolicyHashV1 = PSU.V1.mintingPolicyHash mustSpendScriptOutputPolicyV1

mustSpendScriptOutputPolicyCurrencySymbolV1 :: Value.CurrencySymbol
mustSpendScriptOutputPolicyCurrencySymbolV1 = Value.mpsSymbol mustSpendScriptOutputPolicyHashV1


-----

{-# INLINEABLE mkMustSpendScriptOutputWithMatchingDatumAndValuePolicyV1 #-}
mkMustSpendScriptOutputWithMatchingDatumAndValuePolicyV1 :: [(ValidatorHash, Ledger.Datum, Value.Value, Ledger.Redeemer)] -> PV1.ScriptContext -> Bool
mkMustSpendScriptOutputWithMatchingDatumAndValuePolicyV1 constraintParams ctx = P.traceIfFalse "mustSpendScriptOutputWithMatchingDatumAndValue not satisfied" (Constraints.checkScriptContext @() @() (P.mconcat mustSpendScriptOutputsWithMatchingDatumAndValue) ctx)
    where
        mustSpendScriptOutputsWithMatchingDatumAndValue = P.map (\(vh, datum, value, redeemer) ->
            Constraints.mustSpendScriptOutputWithMatchingDatumAndValue vh (\d -> d P.== datum) (\v -> v P.==  value) redeemer) constraintParams

mustSpendScriptOutputWithMatchingDatumAndValuePolicyV1 :: PSU.V1.MintingPolicy
mustSpendScriptOutputWithMatchingDatumAndValuePolicyV1 = PV1.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        wrap = PSU.V1.mkUntypedMintingPolicy mkMustSpendScriptOutputWithMatchingDatumAndValuePolicyV1

mustSpendScriptOutputWithMatchingDatumAndValuePolicyHashV1 :: Ledger.MintingPolicyHash
mustSpendScriptOutputWithMatchingDatumAndValuePolicyHashV1 = PSU.V1.mintingPolicyHash mustSpendScriptOutputWithMatchingDatumAndValuePolicyV1

mustSpendScriptOutputWithMatchingDatumAndValuePolicyCurrencySymbolV1 :: Value.CurrencySymbol
mustSpendScriptOutputWithMatchingDatumAndValuePolicyCurrencySymbolV1 = Value.mpsSymbol mustSpendScriptOutputWithMatchingDatumAndValuePolicyHashV1

{-
   Plutus V2 Policies
-}
