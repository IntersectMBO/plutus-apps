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
import Ledger.Constraints.OffChain qualified as Cons (MkTxError (NoMatchingOutputFound, TxOutRefWrongType),
                                                      mintingPolicy, typedValidatorLookups, unspentOutputs)
import Ledger.Constraints.OnChain.V1 qualified as Cons.V1
import Ledger.Constraints.OnChain.V2 qualified as Cons.V2
import Ledger.Constraints.TxConstraints qualified as Cons (mustMintValueWithRedeemer, mustPayToTheScript,
                                                           mustSpendScriptOutput,
                                                           mustSpendScriptOutputWithMatchingDatumAndValue)
import Ledger.Test (asDatum, asRedeemer, someAddress, someTypedValidator, someValidatorHash)
import Ledger.Tx qualified as Tx
import Plutus.Contract as Cont (Contract, ContractError (ConstraintResolutionContractError), Empty, awaitTxConfirmed,
                                submitTxConstraintsWith, utxosAt)
import Plutus.Contract.Test (assertContractError, assertFailedTransaction, assertValidatedTransactionCount,
                             checkPredicateOptions, defaultCheckOptions, valueAtAddress, w1, (.&&.))
import Plutus.Script.Utils.Scripts as PSU
import Plutus.Script.Utils.V1.Scripts as PSU.V1 hiding (validatorHash)
import Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import Plutus.Script.Utils.V2.Scripts as PSU.V2 hiding (validatorHash)
import Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api qualified as PV1
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError), unitRedeemer)
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
        , phase2ErrorOnlyWhenMustSpendScriptOutputUsesWrongRedeemerWithV2Script
        , validUseOfMustSpendScriptOutputWithMatchingDatumAndValue
        , contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum
        , contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue
        , phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum
        , phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue
        , phase2ErrorOnlyWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongRedeemerWithV2Script
        ]

-- The value in each initial wallet UTxO
utxoValue :: Value.Value
utxoValue = Ada.lovelaceValueOf 10_000_000

tokenValue :: PSU.Versioned MintingPolicy -> Value.Value
tokenValue mp = Value.singleton (PSU.scriptCurrencySymbol mp) "A" 1

mustPayToTheScriptWithMultipleOutputs :: Integer -> [TxConstraints i P.BuiltinData] -> TxConstraints i P.BuiltinData
mustPayToTheScriptWithMultipleOutputs 0 constraints = mconcat constraints
mustPayToTheScriptWithMultipleOutputs n constraints = mustPayToTheScriptWithMultipleOutputs (n-1) (constraints ++ [Cons.mustPayToTheScript (PlutusTx.toBuiltinData (n-1)) utxoValue])

-- | Contract to create multiple outputs at script address and then uses mustSpendScriptOutputs constraint to spend some of the outputs each with unique datum
mustSpendScriptOutputsContract :: PSU.Language -> Integer -> Integer -> Bool -> Contract () Empty ContractError ()
mustSpendScriptOutputsContract policyVersion nScriptOutputs nScriptOutputsToSpend withExpectedRedeemers = do
    let versionedMintingPolicy = versionedMustSpendScriptOutputPolicy policyVersion
        lookups1 = Cons.typedValidatorLookups someTypedValidator
        tx1 = mustPayToTheScriptWithMultipleOutputs nScriptOutputs []
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    scriptUtxos <- utxosAt someAddress
    let scriptUtxosToSpend = M.keys $ M.take (fromIntegral nScriptOutputsToSpend) scriptUtxos
        expectedRedeemers = L.map (\(a :: Tx.TxOutRef) -> if withExpectedRedeemers then asRedeemer a else asRedeemer a{PV1.txOutRefIdx = PV1.txOutRefIdx a + 1}) scriptUtxosToSpend
        policyRedeemer = asRedeemer $ zip scriptUtxosToSpend expectedRedeemers
        lookups2 = Cons.typedValidatorLookups someTypedValidator
                <> Cons.mintingPolicy versionedMintingPolicy
                <> Cons.unspentOutputs scriptUtxos
        tx2 = mconcat (mustSpendScriptOutputs scriptUtxosToSpend)
           <> Cons.mustMintValueWithRedeemer policyRedeemer (tokenValue versionedMintingPolicy)
    ledgerTx4 <- submitTxConstraintsWith lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx4
    where
        mustSpendScriptOutputs :: [Tx.TxOutRef] -> [TxConstraints P.BuiltinData P.BuiltinData]
        mustSpendScriptOutputs scriptTxOutRefs = fmap (\txOutRef -> Cons.mustSpendScriptOutput txOutRef (asRedeemer txOutRef)) scriptTxOutRefs

mustSpendScriptOutputUsingWrongRedeemerContract :: PSU.Language -> Integer -> Contract () Empty ContractError ()
mustSpendScriptOutputUsingWrongRedeemerContract policyVersion nScriptOutputsToSpend = do
    let versionedMintingPolicy = versionedMustSpendScriptOutputPolicy policyVersion
        lookups1 = Cons.typedValidatorLookups someTypedValidator
        tx1 = mustPayToTheScriptWithMultipleOutputs nScriptOutputsToSpend []
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    scriptUtxos <- utxosAt someAddress
    let scriptUtxo1 = fst $ M.elemAt 0 scriptUtxos
        scriptUtxo2 = fst $ M.elemAt 1 scriptUtxos
        policyRedeemer = asRedeemer [(scriptUtxo1, asRedeemer scriptUtxo2)]
        lookups2 = Cons.typedValidatorLookups someTypedValidator
                <> Cons.mintingPolicy versionedMintingPolicy
                <> Cons.unspentOutputs scriptUtxos
        tx2 = Cons.mustSpendScriptOutput scriptUtxo1 unitRedeemer
           <> Cons.mustMintValueWithRedeemer policyRedeemer (tokenValue versionedMintingPolicy)
    ledgerTx4 <- submitTxConstraintsWith lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx4

mustSpendScriptOutputWithMatchingDatumAndValueContractWithRdmr :: PSU.Language -> Integer -> (Integer, Integer) -> (Value.Value, Value.Value) -> (Ledger.Redeemer, Ledger.Redeemer) -> Contract () Empty ContractError ()
mustSpendScriptOutputWithMatchingDatumAndValueContractWithRdmr policyVersion nScriptOutputs (offChainMatchingDatum, onChainMatchingDatum) (offChainMatchingValue, onChainMatchingValue) (spendingRedeemer, expectedSpendingRedeemer) = do
    let versionedMintingPolicy = versionedMustSpendScriptOutputWithMatchingDatumAndValuePolicy policyVersion
        lookups1 = Cons.typedValidatorLookups someTypedValidator
        tx1 = mustPayToTheScriptWithMultipleOutputs nScriptOutputs []
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    scriptUtxos <- utxosAt someAddress
    let policyRedeemer = asRedeemer [(someValidatorHash, onChainMatchingDatum, onChainMatchingValue, expectedSpendingRedeemer)]
    let lookups2 = Cons.typedValidatorLookups someTypedValidator
                <> Cons.mintingPolicy versionedMintingPolicy
                <> Cons.unspentOutputs scriptUtxos
        tx2 = Cons.mustSpendScriptOutputWithMatchingDatumAndValue someValidatorHash (\d -> d == asDatum offChainMatchingDatum) (\v -> v == offChainMatchingValue) spendingRedeemer
           <> Cons.mustMintValueWithRedeemer policyRedeemer (tokenValue versionedMintingPolicy)
    ledgerTx4 <- submitTxConstraintsWith lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx4

-- | Contract to create multiple outputs at script address and then uses mustSpendScriptOutputWithMatchingDatumAndValue constraint to spend one of the outputs
mustSpendScriptOutputWithMatchingDatumAndValueContract :: PSU.Language -> Integer -> (Integer, Integer) -> (Value.Value, Value.Value) -> Contract () Empty ContractError ()
mustSpendScriptOutputWithMatchingDatumAndValueContract policyVersion nScriptOutputs (offChainMatchingDatum, onChainMatchingDatum) (offChainMatchingValue, onChainMatchingValue) =
    mustSpendScriptOutputWithMatchingDatumAndValueContractWithRdmr policyVersion nScriptOutputs (offChainMatchingDatum, onChainMatchingDatum) (offChainMatchingValue, onChainMatchingValue) (asRedeemer @P.BuiltinByteString "R", asRedeemer @P.BuiltinByteString "R")

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
    (void $ trace $ mustSpendScriptOutputsContract PlutusV1 5 5 True)

-- | Uses onchain and offchain constraint mustSpendScriptOutput to spend some of the UtxOs locked by the script
validUseOfMustSpendScriptOutputUsingSomeScriptOutputs :: TestTree
validUseOfMustSpendScriptOutputUsingSomeScriptOutputs =
    checkPredicateOptions
    defaultCheckOptions
    "Successful use of mustSpendScriptOutput for some of the script's UtxOs"
    (valueAtAddress someAddress (== Value.scale 2 utxoValue)
    .&&. assertValidatedTransactionCount 2)
    (void $ trace $ mustSpendScriptOutputsContract PlutusV1 5 3 True)

-- | Contract error occurs when offchain mustSpendScriptOutput constraint is used with a UTxO belonging to the w1 pubkey address, not the script (not of type ScriptChainIndexTxOut).
contractErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef :: TestTree
contractErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef =
    let contract :: Contract () Empty ContractError () = do
            let lookups1 = Cons.typedValidatorLookups someTypedValidator
                tx1 = mustPayToTheScriptWithMultipleOutputs 3 []
            ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

            scriptUtxos <- utxosAt someAddress
            w1Utxos <- utxosAt (mockWalletAddress w1)
            let w1Utxo = fst $ M.elemAt 2 w1Utxos
                lookups2 = Cons.typedValidatorLookups someTypedValidator <>
                    Cons.unspentOutputs (scriptUtxos <> w1Utxos)
                tx2 = Cons.mustSpendScriptOutput w1Utxo unitRedeemer
            ledgerTx4 <- submitTxConstraintsWith lookups2 tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx4

    in checkPredicateOptions
        defaultCheckOptions
        "Contract error occurs when offchain mustSpendScriptOutput is used with a UTxO belonging to the w1 pubkey address, not the script."
        (assertContractError contract (Trace.walletInstanceTag w1) (\case ConstraintResolutionContractError (Cons.TxOutRefWrongType _) -> True; _ -> False) "failed to throw error"
        .&&. assertValidatedTransactionCount 1)
        (void $ trace contract)

-- | Phase-2 validation failure when onchain mustSpendScriptOutput constraint expects a different TxOutRef belonging to the script
phase2ErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef :: TestTree
phase2ErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef =
    let vMintingPolicy = Versioned mustSpendScriptOutputPolicyV1 PlutusV1
        contract = do
            let lookups1 = Cons.typedValidatorLookups someTypedValidator
                tx1 = mustPayToTheScriptWithMultipleOutputs 3 []
            ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

            scriptUtxos <- utxosAt someAddress
            let scriptUtxo1 = fst $ M.elemAt 0 scriptUtxos
                scriptUtxo2 = fst $ M.elemAt 1 scriptUtxos
                policyRedeemer = asRedeemer [(scriptUtxo2, asRedeemer scriptUtxo2)]
                lookups2 = Cons.typedValidatorLookups someTypedValidator
                        <> Cons.mintingPolicy vMintingPolicy
                        <> Cons.unspentOutputs scriptUtxos
                tx2 = Cons.mustSpendScriptOutput scriptUtxo1 unitRedeemer
                   <> Cons.mustMintValueWithRedeemer policyRedeemer (tokenValue vMintingPolicy)
            ledgerTx4 <- submitTxConstraintsWith lookups2 tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx4

    in checkPredicateOptions
        defaultCheckOptions
        "Phase-2 validation failure when onchain mustSpendScriptOutput constraint expects a different TxOutRef"
        (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("L8":_) _) -> True; _ -> False }))
        (void $ trace contract)

-- | Phase-2 validation failure only when V2 script using onchain mustSpendScriptOutput constraint expects a different redeeemer with V2+ script
phase2ErrorOnlyWhenMustSpendScriptOutputUsesWrongRedeemerWithV2Script :: TestTree
phase2ErrorOnlyWhenMustSpendScriptOutputUsesWrongRedeemerWithV2Script =
    testGroup "phase2 error only when mustSpendScriptOutput uses wrong redeemer with V2+ script"

    [ checkPredicateOptions
      defaultCheckOptions
      "No phase-2 validation failure when V1 script using onchain mustSpendScriptOutput constraint expects a different redeemer"
      (valueAtAddress someAddress (== Value.scale 2 utxoValue)
      .&&. assertValidatedTransactionCount 2)
      (void $ trace $ mustSpendScriptOutputUsingWrongRedeemerContract PlutusV1 3)

    , checkPredicateOptions
      defaultCheckOptions
      "Phase-2 validation failure when V2 script using onchain mustSpendScriptOutput constraint expects a different redeemer"
      (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("L8":_) _) -> True; _ -> False }))
      (void $ trace $ mustSpendScriptOutputUsingWrongRedeemerContract PlutusV2 3)
    ]

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
        (void $ trace $ mustSpendScriptOutputWithMatchingDatumAndValueContract PlutusV1 nScriptOutputs (scriptOutputIdx, scriptOutputIdx) (utxoValue, utxoValue))

-- | Contract error occurs when offchain mustSpendScriptOutputWithMatchingDatumAndValue constraint is used with a datum that does not match with one of the script's UTxOs
contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum :: TestTree
contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum =
    let nScriptOutputs  = 5
        wrongDatum = nScriptOutputs -- value is greater than any datum at script address
        contract = mustSpendScriptOutputWithMatchingDatumAndValueContract PlutusV1 nScriptOutputs (wrongDatum, wrongDatum) (utxoValue, utxoValue)
    in checkPredicateOptions
        defaultCheckOptions
        "Contract error occurs when offchain mustSpendScriptOutputWithMatchingDatumAndValue constraint is used with a datum that cannot be matched"
        (assertContractError contract (Trace.walletInstanceTag w1) (\case ConstraintResolutionContractError (Cons.NoMatchingOutputFound _) -> True; _ -> False) "failed to throw error"
        .&&. assertValidatedTransactionCount 1)
        (void $ trace contract)

-- | Contract error occurs when offchain mustSpendScriptOutputWithMatchingDatumAndValue constraint is used with a value that does not match with one of the script's UTxOs
contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue :: TestTree
contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue =
    let nScriptOutputs  = 5
        scriptOutputIdx = nScriptOutputs - 1
        wrongValue = Ada.lovelaceValueOf (1 + (Ada.getLovelace $ Ada.fromValue utxoValue))
        contract = mustSpendScriptOutputWithMatchingDatumAndValueContract PlutusV1 nScriptOutputs (scriptOutputIdx, scriptOutputIdx) (wrongValue, wrongValue)
    in checkPredicateOptions
        defaultCheckOptions
        "Contract error occurs when offchain mustSpendScriptOutputWithMatchingDatumAndValue constraint is used with a value that cannot be matched"
        (assertContractError contract (Trace.walletInstanceTag w1) (\case ConstraintResolutionContractError (Cons.NoMatchingOutputFound _) -> True; _ -> False) "failed to throw error"
        .&&. assertValidatedTransactionCount 1)
        (void $ trace contract)

-- | Phase-2 validation failure when onchain mustSpendScriptOutputWithMatchingDatumAndValue constraint expects a different Datum
phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum :: TestTree
phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum =
    let nScriptOutputs  = 5
        scriptOutputIdx = nScriptOutputs - 1
        wrongDatum = scriptOutputIdx - 1
    in checkPredicateOptions
        defaultCheckOptions
        "Phase-2 validation failure when onchain mustSpendScriptOutputWithMatchingDatumAndValue constraint expects a different TxOutRef"
        (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("Le":_) _) -> True; _ -> False }))
        (void $ trace $ mustSpendScriptOutputWithMatchingDatumAndValueContract PlutusV1 nScriptOutputs (scriptOutputIdx, wrongDatum) (utxoValue, utxoValue))

-- | Phase-2 validation failure when onchain mustSpendScriptOutputWithMatchingDatumAndValue constraint expects a different Value
phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue :: TestTree
phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue =
    let nScriptOutputs  = 5
        scriptOutputIdx = nScriptOutputs - 1
        wrongValue = Ada.lovelaceValueOf (1 + (Ada.getLovelace $ Ada.fromValue utxoValue))
    in checkPredicateOptions
        defaultCheckOptions
        "Phase-2 validation failure when onchain mustSpendScriptOutputWithMatchingDatumAndValue constraint expects a different TxOutRef"
        (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("Le":_) _) -> True; _ -> False }))
        (void $ trace $ mustSpendScriptOutputWithMatchingDatumAndValueContract PlutusV1 nScriptOutputs (scriptOutputIdx, scriptOutputIdx) (utxoValue, wrongValue))

-- | Phase-2 validation failure only when onchain mustSpendScriptOutputWithMatchingDatumAndValue constraint expects a different Redeemer with V2+ script
phase2ErrorOnlyWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongRedeemerWithV2Script :: TestTree
phase2ErrorOnlyWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongRedeemerWithV2Script =
    let nScriptOutputs  = 5
        scriptOutputIdx = nScriptOutputs - 1
        spendingRedeemer = asRedeemer @Integer 42
        policyRedeemer = asRedeemer @Integer 41

    in testGroup "phase2 error only when mustSpendScriptOutputWithMatchingDatumAndValue uses wrong redeemer with V2+ script"

        [ checkPredicateOptions
          defaultCheckOptions
          "No phase-2 validation failure when V1 script using onchain mustSpendScriptOutputWithMatchingDatumAndValue constraint expects a different redeemer"
          (valueAtAddress someAddress (== Value.scale 4 utxoValue)
            .&&. assertValidatedTransactionCount 2)
          (void $ trace $ mustSpendScriptOutputWithMatchingDatumAndValueContractWithRdmr PlutusV1 nScriptOutputs (scriptOutputIdx, scriptOutputIdx) (utxoValue, utxoValue) (spendingRedeemer, policyRedeemer))

        , checkPredicateOptions
          defaultCheckOptions
          "Phase-2 validation failure when V2 script using onchain mustSpendScriptOutputWithMatchingDatumAndValue constraint expects a different redeemer"
          (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("Le":_) _) -> True; _ -> False }))
          (void $ trace $ mustSpendScriptOutputWithMatchingDatumAndValueContractWithRdmr PlutusV2 nScriptOutputs (scriptOutputIdx, scriptOutputIdx) (utxoValue, utxoValue) (spendingRedeemer, policyRedeemer))
        ]

{-
    Versioned Policies
-}

versionedMustSpendScriptOutputPolicy :: PSU.Language -> PSU.Versioned PV1.MintingPolicy
versionedMustSpendScriptOutputPolicy l = case l of
    PlutusV1 -> Versioned mustSpendScriptOutputPolicyV1 PlutusV1
    PlutusV2 -> Versioned mustSpendScriptOutputPolicyV2 PlutusV2

versionedMustSpendScriptOutputWithMatchingDatumAndValuePolicy :: PSU.Language -> PSU.Versioned PV1.MintingPolicy
versionedMustSpendScriptOutputWithMatchingDatumAndValuePolicy l = case l of
    PlutusV1 -> Versioned mustSpendScriptOutputWithMatchingDatumAndValuePolicyV1 PlutusV1
    PlutusV2 -> Versioned mustSpendScriptOutputWithMatchingDatumAndValuePolicyV2 PlutusV2

{-
    V1 Policies
-}

{-# INLINEABLE mkMustSpendScriptOutputPolicyV1 #-}
mkMustSpendScriptOutputPolicyV1 :: [(Tx.TxOutRef,  Redeemer)] -> PV1.ScriptContext -> Bool
mkMustSpendScriptOutputPolicyV1 constraintParams ctx = P.traceIfFalse "mustSpendScriptOutput not satisfied" (Cons.V1.checkScriptContext @() @() (P.mconcat mustSpendScriptOutputs) ctx)
    where
        mustSpendScriptOutputs = P.map (\(txOutRef, redeemer) -> Cons.mustSpendScriptOutput txOutRef redeemer) constraintParams

mustSpendScriptOutputPolicyV1 :: PV1.MintingPolicy
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
mkMustSpendScriptOutputWithMatchingDatumAndValuePolicyV1 constraintParams ctx = P.traceIfFalse "mustSpendScriptOutputWithMatchingDatumAndValue not satisfied" (Cons.V1.checkScriptContext @() @() (P.mconcat mustSpendScriptOutputsWithMatchingDatumAndValue) ctx)
    where
        mustSpendScriptOutputsWithMatchingDatumAndValue = P.map (\(vh, datum, value, redeemer) ->
            Cons.mustSpendScriptOutputWithMatchingDatumAndValue vh (\d -> d P.== datum) (\v -> v P.==  value) redeemer) constraintParams

mustSpendScriptOutputWithMatchingDatumAndValuePolicyV1 :: PV1.MintingPolicy
mustSpendScriptOutputWithMatchingDatumAndValuePolicyV1 = PV1.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        wrap = PSU.V1.mkUntypedMintingPolicy mkMustSpendScriptOutputWithMatchingDatumAndValuePolicyV1

mustSpendScriptOutputWithMatchingDatumAndValuePolicyHashV1 :: Ledger.MintingPolicyHash
mustSpendScriptOutputWithMatchingDatumAndValuePolicyHashV1 = PSU.V1.mintingPolicyHash mustSpendScriptOutputWithMatchingDatumAndValuePolicyV1

mustSpendScriptOutputWithMatchingDatumAndValuePolicyCurrencySymbolV1 :: Value.CurrencySymbol
mustSpendScriptOutputWithMatchingDatumAndValuePolicyCurrencySymbolV1 = Value.mpsSymbol mustSpendScriptOutputWithMatchingDatumAndValuePolicyHashV1

{-
    V2 Policies
-}

{-# INLINEABLE mkMustSpendScriptOutputPolicyV2 #-}
mkMustSpendScriptOutputPolicyV2 :: [(Tx.TxOutRef,  Redeemer)] -> PV2.ScriptContext -> Bool
mkMustSpendScriptOutputPolicyV2 constraintParams ctx = P.traceIfFalse "mustSpendScriptOutput not satisfied" (Cons.V2.checkScriptContext @() @() (P.mconcat mustSpendScriptOutputs) ctx)
    where
        mustSpendScriptOutputs = P.map (\(txOutRef, redeemer) -> Cons.mustSpendScriptOutput txOutRef redeemer) constraintParams

mustSpendScriptOutputPolicyV2 :: PV2.MintingPolicy
mustSpendScriptOutputPolicyV2 = PV2.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        wrap = PSU.V2.mkUntypedMintingPolicy mkMustSpendScriptOutputPolicyV2

mustSpendScriptOutputPolicyHashV2 :: Ledger.MintingPolicyHash
mustSpendScriptOutputPolicyHashV2 = PSU.V2.mintingPolicyHash mustSpendScriptOutputPolicyV2

mustSpendScriptOutputPolicyCurrencySymbolV2 :: Value.CurrencySymbol
mustSpendScriptOutputPolicyCurrencySymbolV2 = Value.mpsSymbol mustSpendScriptOutputPolicyHashV2


-----

{-# INLINEABLE mkMustSpendScriptOutputWithMatchingDatumAndValuePolicyV2 #-}
mkMustSpendScriptOutputWithMatchingDatumAndValuePolicyV2 :: [(ValidatorHash, Ledger.Datum, Value.Value, Ledger.Redeemer)] -> PV2.ScriptContext -> Bool
mkMustSpendScriptOutputWithMatchingDatumAndValuePolicyV2 constraintParams ctx = P.traceIfFalse "mustSpendScriptOutputWithMatchingDatumAndValue not satisfied" (Cons.V2.checkScriptContext @() @() (P.mconcat mustSpendScriptOutputsWithMatchingDatumAndValue) ctx)
    where
        mustSpendScriptOutputsWithMatchingDatumAndValue = P.map (\(vh, datum, value, redeemer) ->
            Cons.mustSpendScriptOutputWithMatchingDatumAndValue vh (\d -> d P.== datum) (\v -> v P.==  value) redeemer) constraintParams

mustSpendScriptOutputWithMatchingDatumAndValuePolicyV2 :: PSU.V2.MintingPolicy
mustSpendScriptOutputWithMatchingDatumAndValuePolicyV2 = PV2.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        wrap = PSU.V2.mkUntypedMintingPolicy mkMustSpendScriptOutputWithMatchingDatumAndValuePolicyV2

mustSpendScriptOutputWithMatchingDatumAndValuePolicyHashV2 :: Ledger.MintingPolicyHash
mustSpendScriptOutputWithMatchingDatumAndValuePolicyHashV2 = PSU.V2.mintingPolicyHash mustSpendScriptOutputWithMatchingDatumAndValuePolicyV2

mustSpendScriptOutputWithMatchingDatumAndValuePolicyCurrencySymbolV2 :: Value.CurrencySymbol
mustSpendScriptOutputWithMatchingDatumAndValuePolicyCurrencySymbolV2 = Value.mpsSymbol mustSpendScriptOutputWithMatchingDatumAndValuePolicyHashV2
