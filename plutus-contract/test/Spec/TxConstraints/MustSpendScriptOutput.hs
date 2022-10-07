{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.TxConstraints.MustSpendScriptOutput(tests) where

import Control.Lens ((??))
import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Data.List as L
import Data.Map as M
import Data.Maybe (fromJust)
import Data.Void (Void)
import Ledger qualified as L
import Ledger.Ada qualified as Ada
import Ledger.Constraints (TxConstraints)
import Ledger.Constraints.OffChain qualified as Cons (MkTxError (NoMatchingOutputFound, TxOutRefWrongType),
                                                      mintingPolicy, plutusV2OtherScript, typedValidatorLookups,
                                                      unspentOutputs)
import Ledger.Constraints.OnChain.V1 qualified as Cons.V1
import Ledger.Constraints.OnChain.V2 qualified as Cons.V2
import Ledger.Constraints.TxConstraints qualified as Cons (TxConstraints, collectFromPlutusV2Script,
                                                           mustIncludeDatumInTx, mustMintValueWithRedeemer,
                                                           mustPayToOtherScriptWithDatumInTx,
                                                           mustPayToTheScriptWithDatumInTx, mustReferenceOutput,
                                                           mustSpendPubKeyOutput, mustSpendScriptOutput,
                                                           mustSpendScriptOutputWithMatchingDatumAndValue)
import Ledger.Test (asDatum, asRedeemer, someAddress, someTypedValidator, someValidatorHash)
import Ledger.Tx qualified as Tx
import Plutus.Contract as Cont (Contract, ContractError (ConstraintResolutionContractError), Empty, awaitTxConfirmed,
                                mkTxConstraints, ownUtxos, submitTxConfirmed, submitTxConstraintsWith, utxosAt)
import Plutus.Contract.Test (assertContractError, assertFailedTransaction, assertValidatedTransactionCount,
                             checkPredicate, checkPredicateOptions, defaultCheckOptions, valueAtAddress, w1,
                             walletFundsChange, (.&&.))
import Plutus.Script.Utils.Scripts (Language (..))
import Plutus.Script.Utils.Scripts qualified as PSU
import Plutus.Script.Utils.Typed (Any)
import Plutus.Script.Utils.V1.Address qualified as PSU.V1
import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import Plutus.Script.Utils.V2.Address qualified as PSU.V2
import Plutus.Script.Utils.V2.Scripts qualified as PSU.V2
import Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api qualified as PV1
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError), unitRedeemer)
import Plutus.V1.Ledger.Value qualified as V
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Prelude hiding (not)
import Wallet.Emulator.Wallet (mockWalletAddress)

tests :: TestTree
tests = testGroup "MustSpendScriptOutput"
      [ testGroup "ledger constraints" [v1Tests, v2Tests]
      , testGroup "version agnostic tests"
          [ contractErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef
          ]
      ]

v1Tests :: TestTree
v1Tests = testGroup "Plutus V1" $ featuresTests PlutusV1

v2Tests :: TestTree
v2Tests = testGroup "Plutus V2" $ featuresTests PlutusV2

featuresTests :: PSU.Language -> [TestTree]
featuresTests t =
    [ validUseOfMustSpendScriptOutputUsingAllScriptOutputs
    , validUseOfMustSpendScriptOutputUsingSomeScriptOutputs
    , phase2ErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef
    , phase2ErrorOnlyWhenMustSpendScriptOutputUsesWrongRedeemerWithV2Script
    , validUseOfMustSpendScriptOutputWithMatchingDatumAndValue
    , contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum
    , contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue
    , phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum
    , phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue
    , phase2ErrorOnlyWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongRedeemerWithV2Script
    , validUseOfReferenceScript
    ] ?? t

-- The value in each initial wallet UTxO
utxoValue :: V.Value
utxoValue = Ada.lovelaceValueOf 10_000_000

tokenValue :: PSU.Versioned MintingPolicy -> V.Value
tokenValue mp = V.singleton (PSU.scriptCurrencySymbol mp) "A" 1

mustPayToTheScriptWithMultipleOutputs
    :: Integer
    -> TxConstraints i P.BuiltinData
mustPayToTheScriptWithMultipleOutputs n = let
    go x = Cons.mustPayToTheScriptWithDatumInTx (PlutusTx.toBuiltinData x) utxoValue
    in mconcat $ go <$> [0 .. n-1]

-- | Contract to create multiple outputs at script address and then uses mustSpendScriptOutputs
-- constraint to spend some of the outputs each with unique datum
mustSpendScriptOutputsContract :: PSU.Language -> Integer -> Integer -> Contract () Empty ContractError ()
mustSpendScriptOutputsContract a b c = mustSpendScriptOutputsContract' a b c True

mustSpendScriptOutputsContract' :: PSU.Language -> Integer -> Integer -> Bool -> Contract () Empty ContractError ()
mustSpendScriptOutputsContract' policyVersion nScriptOutputs nScriptOutputsToSpend withExpectedRedeemers = do
    let versionedMintingPolicy = getVersionedMintingPolicy MustSpendScriptOutputPolicy policyVersion
        lookups1 = Cons.typedValidatorLookups someTypedValidator
        tx1 = mustPayToTheScriptWithMultipleOutputs nScriptOutputs
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    scriptUtxos <- utxosAt someAddress
    let scriptUtxosToSpend = M.keys $ M.take (fromIntegral nScriptOutputsToSpend) scriptUtxos
        txOutRefToRedeemer a = if withExpectedRedeemers
          then asRedeemer a
          else asRedeemer a{PV1.txOutRefIdx = PV1.txOutRefIdx a + 1}
        expectedRedeemers = L.map txOutRefToRedeemer scriptUtxosToSpend
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
        mustSpendScriptOutputs = fmap (\txOutRef -> Cons.mustSpendScriptOutput txOutRef (asRedeemer txOutRef))

-- | Contract to create multiple outputs at script address and then uses
-- mustSpendScriptOutputWithMatchingDatumAndValue constraint to spend one of the outputs
mustSpendScriptOutputWithMatchingDatumAndValueContract
    :: PSU.Language
    -> Integer
    -> (Integer, V.Value)
    -> (Integer, V.Value)
    -> Contract () Empty ContractError ()
mustSpendScriptOutputWithMatchingDatumAndValueContract a b c d =
    mustSpendScriptOutputWithMatchingDatumAndValueContract' a b c d True

mustSpendScriptOutputWithMatchingDatumAndValueContract'
    :: PSU.Language
    -> Integer
    -> (Integer, V.Value)
    -> (Integer, V.Value)
    -> Bool
    -> Contract () Empty ContractError ()
mustSpendScriptOutputWithMatchingDatumAndValueContract'
    policyVersion
    nScriptOutputs
    (offChainMatchingDatum, offChainMatchingValue)
    (onChainMatchingDatum, onChainMatchingValue)
    withExpectedRedeemer = do
    let versionedMintingPolicy =
            getVersionedMintingPolicy MustSpendScriptOutputWithMatchingDatumAndValuePolicy policyVersion
        lookups1 = Cons.typedValidatorLookups someTypedValidator
        tx1 = mustPayToTheScriptWithMultipleOutputs nScriptOutputs
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    scriptUtxos <- utxosAt someAddress
    let computeExpectedSpendingRedeemer = if withExpectedRedeemer then id else (+ 1)
        (spendingRedeemer, expectedSpendingRedeemer) = (42 :: Integer, computeExpectedSpendingRedeemer spendingRedeemer)
        policyRedeemer = asRedeemer
           [(someValidatorHash, onChainMatchingDatum, onChainMatchingValue, asRedeemer expectedSpendingRedeemer)]
        lookups2 = Cons.typedValidatorLookups someTypedValidator
                <> Cons.mintingPolicy versionedMintingPolicy
                <> Cons.unspentOutputs scriptUtxos
        tx2 = Cons.mustSpendScriptOutputWithMatchingDatumAndValue
                  someValidatorHash
                  (== asDatum offChainMatchingDatum)
                  (== offChainMatchingValue)
                  (asRedeemer spendingRedeemer)
           <> Cons.mustMintValueWithRedeemer policyRedeemer (tokenValue versionedMintingPolicy)
    ledgerTx2 <- submitTxConstraintsWith lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

mustReferenceOutputV2ConTest :: Contract () Empty ContractError ()
mustReferenceOutputV2ConTest = do

    utxos <- ownUtxos
    let (utxoRef, utxo, utxoRefForBalance1, utxoRefForBalance2) = case M.toList utxos of
            (utxoRef', utxo'):(utxoRefForBalance1', _):(utxoRefForBalance2', _):_ ->
                  (utxoRef', utxo', utxoRefForBalance1', utxoRefForBalance2')
            _ ->  error "Spec.Contract.TxConstraints.get3: not enough inputs"
        mustReferenceOutputValidatorAddress = scriptAddress MustReferenceOutputValidator PlutusV2
        vh = fromJust $ L.toValidatorHash mustReferenceOutputValidatorAddress
        lookups = Cons.unspentOutputs utxos
        datum = PV2.Datum $ PlutusTx.toBuiltinData utxoRef
        tx = Cons.mustPayToOtherScriptWithDatumInTx vh datum (Ada.adaValueOf 5)
          <> Cons.mustIncludeDatumInTx datum
          <> Cons.mustSpendPubKeyOutput utxoRefForBalance1
    mkTxConstraints @Void lookups tx >>= submitTxConfirmed

    -- Trying to unlock the Ada in the script address
    scriptUtxos <- utxosAt mustReferenceOutputValidatorAddress
    let mustReferenceOutputValidator = getScript MustReferenceOutputValidator PlutusV2
        addressMap = M.singleton mustReferenceOutputValidatorAddress scriptUtxos
        lookups2 = Cons.unspentOutputs (M.singleton utxoRef utxo <> scriptUtxos)
               <> Cons.plutusV2OtherScript mustReferenceOutputValidator
               <> Cons.unspentOutputs utxos
        tx2 = Cons.mustReferenceOutput utxoRef
          <> Cons.collectFromPlutusV2Script addressMap mustReferenceOutputValidator unitRedeemer
          <> Cons.mustSpendPubKeyOutput utxoRefForBalance2
    mkTxConstraints @Any lookups2 tx2 >>= submitTxConfirmed

trace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void $ Trace.waitNSlots 2

-- | Uses onchain and offchain constraint mustSpendScriptOutput to spend all UtxOs locked
-- by the script
validUseOfMustSpendScriptOutputUsingAllScriptOutputs :: PSU.Language -> TestTree
validUseOfMustSpendScriptOutputUsingAllScriptOutputs l =
    checkPredicate
    "Successful use of mustSpendScriptOutput for all script's UtxOs"
    (valueAtAddress someAddress (== Ada.lovelaceValueOf 0)
    .&&. assertValidatedTransactionCount 2)
    (void $ trace $ mustSpendScriptOutputsContract l 5 5)

-- | Uses onchain and offchain constraint mustSpendScriptOutput to spend some of the UtxOs locked
-- by the script
validUseOfMustSpendScriptOutputUsingSomeScriptOutputs :: PSU.Language -> TestTree
validUseOfMustSpendScriptOutputUsingSomeScriptOutputs l =
    checkPredicate
    "Successful use of mustSpendScriptOutput for some of the script's UtxOs"
    (valueAtAddress someAddress (== V.scale 2 utxoValue)
    .&&. assertValidatedTransactionCount 2)
    (void $ trace $ mustSpendScriptOutputsContract l 5 3)

validUseOfReferenceScript :: PSU.Language -> TestTree
validUseOfReferenceScript _l = let
    tag = "instance 1"
    in checkPredicate
    "mustReferenceOutput can be used on-chain to unlock funds in a PlutusV2 script"
    (walletFundsChange w1 (Ada.adaValueOf 0)
    .&&. valueAtAddress (scriptAddress MustReferenceOutputValidator PlutusV2 ) (== Ada.adaValueOf 0)
    .&&. assertValidatedTransactionCount 2
    )
    $ do void $ Trace.activateContract w1 mustReferenceOutputV2ConTest tag
         void $ Trace.waitNSlots 3


-- | Contract error occurs when offchain mustSpendScriptOutput constraint is used with a UTxO
-- belonging to the w1 pubkey address, not the script (not of type ScriptChainIndexTxOut).
contractErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef :: TestTree
contractErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef =
    let contract :: Contract () Empty ContractError () = do
            let lookups1 = Cons.typedValidatorLookups someTypedValidator
                tx1 = mustPayToTheScriptWithMultipleOutputs 3
            ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

            scriptUtxos <- utxosAt someAddress
            w1Utxos <- utxosAt (mockWalletAddress w1)
            let w1Utxo = fst $ M.elemAt 2 w1Utxos
                lookups2 = Cons.typedValidatorLookups someTypedValidator <>
                    Cons.unspentOutputs (scriptUtxos <> w1Utxos)
                tx2 = Cons.mustSpendScriptOutput w1Utxo unitRedeemer
            ledgerTx2 <- submitTxConstraintsWith lookups2 tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

    in checkPredicate
        "Contract error occurs when offchain mustSpendScriptOutput is used with a UTxO belonging to the w1 pubkey address, not the script."
        (assertContractError
            contract
            (Trace.walletInstanceTag w1)
            (\case
                ConstraintResolutionContractError (Cons.TxOutRefWrongType _) -> True
                _                                                            -> False
            )
        "failed to throw error"
        .&&. assertValidatedTransactionCount 1)
        $ void $ trace contract

-- | Phase-2 validation failure when onchain mustSpendScriptOutput constraint expects a different
-- TxOutRef belonging to the script
phase2ErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef :: PSU.Language -> TestTree
phase2ErrorWhenMustSpendScriptOutputUsesWrongTxoOutRef l =
    let vMintingPolicy = getVersionedMintingPolicy MustSpendScriptOutputPolicy l
        contract = do
            let lookups1 = Cons.typedValidatorLookups someTypedValidator
                tx1 = mustPayToTheScriptWithMultipleOutputs 3
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

    in checkPredicate
        "Phase-2 validation failure when onchain mustSpendScriptOutput constraint expects a different TxOutRef"
        (assertFailedTransaction $ const $ \case
            L.ScriptFailure (EvaluationError ("L8":_) _) -> True
            _                                            -> False
        )
        $ void $ trace contract

-- | Phase-2 validation failure only when V2 script using onchain mustSpendScriptOutput constraint
-- expects a different redeeemer with V2+ script
-- | Phase-2 validation failure only when V2 script using onchain mustSpendScriptOutput constraint expects a different redeeemer with V2+ script
phase2ErrorOnlyWhenMustSpendScriptOutputUsesWrongRedeemerWithV2Script :: PSU.Language -> TestTree
phase2ErrorOnlyWhenMustSpendScriptOutputUsesWrongRedeemerWithV2Script l =
    case l of
        PlutusV1 ->
            checkPredicate
                "No phase-2 validation failure when V1 script using onchain mustSpendScriptOutput constraint expects a different redeemer"
                ( valueAtAddress someAddress (== utxoValue)
                .&&. assertValidatedTransactionCount 2
                )
                $ void $ trace $ mustSpendScriptOutputsContract' l 7 6 False
        PlutusV2 ->
             checkPredicate
                 "Phase-2 validation failure when V2 script using onchain mustSpendScriptOutput constraint expects a different redeemer"
                 ( assertFailedTransaction $ const $
                     \case
                        L.ScriptFailure (EvaluationError ("L8":_) _) -> True
                        _                                            -> False
                 )
                 $ void $ trace $ mustSpendScriptOutputsContract' l 5 5 False

-- | Uses onchain and offchain constraint mustSpendScriptOutputWithMatchingDatumAndValue to spend a
-- UTxO locked by the script with matching datum and value
validUseOfMustSpendScriptOutputWithMatchingDatumAndValue :: PSU.Language -> TestTree
validUseOfMustSpendScriptOutputWithMatchingDatumAndValue l =
    let nScriptOutputs  = 5
        scriptOutputIdx = nScriptOutputs - 1
    in checkPredicateOptions
        defaultCheckOptions
        "Successful use of mustSpendScriptOutputWithMatchingDatumAndValue to spend a UTxO locked by the script with matching datum and value"
        (valueAtAddress someAddress (== V.scale 4 utxoValue)
        .&&. assertValidatedTransactionCount 2)
        $ void
        $ trace
        $ mustSpendScriptOutputWithMatchingDatumAndValueContract
            l nScriptOutputs
            (scriptOutputIdx, utxoValue)
            (scriptOutputIdx, utxoValue)

-- | Contract error occurs when offchain mustSpendScriptOutputWithMatchingDatumAndValue constraint
-- is used with a datum that does not match with one of the script's UTxOs
contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum :: PSU.Language -> TestTree
contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum l =
    let nScriptOutputs  = 5
        wrongDatum = nScriptOutputs -- value is greater than any datum at script address
        contract = mustSpendScriptOutputWithMatchingDatumAndValueContract l nScriptOutputs (wrongDatum, utxoValue) (wrongDatum, utxoValue)
    in checkPredicate
        "Contract error occurs when offchain mustSpendScriptOutputWithMatchingDatumAndValue constraint is used with a datum that cannot be matched"
        (assertContractError
             contract
             (Trace.walletInstanceTag w1)
             (\case
                 ConstraintResolutionContractError (Cons.NoMatchingOutputFound _) -> True
                 _                                                                -> False
             )
             "failed to throw error"
        .&&. assertValidatedTransactionCount 1)
        $ void $ trace contract

-- | Contract error occurs when offchain mustSpendScriptOutputWithMatchingDatumAndValue constraint
-- is used with a value that does not match with one of the script's UTxOs
contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue :: PSU.Language -> TestTree
contractErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue l =
    let nScriptOutputs  = 5
        scriptOutputIdx = nScriptOutputs - 1
        wrongValue = Ada.lovelaceValueOf (1 + (Ada.getLovelace $ Ada.fromValue utxoValue))
        contract = mustSpendScriptOutputWithMatchingDatumAndValueContract l nScriptOutputs (scriptOutputIdx, wrongValue) (scriptOutputIdx, wrongValue)
    in checkPredicateOptions
        defaultCheckOptions
        "Contract error occurs when offchain mustSpendScriptOutputWithMatchingDatumAndValue constraint is used with a value that cannot be matched"
        (assertContractError
            contract
            (Trace.walletInstanceTag w1)
            (\case
                ConstraintResolutionContractError (Cons.NoMatchingOutputFound _) -> True
                _                                                                -> False
            )
            "failed to throw error"
        .&&. assertValidatedTransactionCount 1)
        $ void $ trace contract

-- | Phase-2 validation failure when onchain mustSpendScriptOutputWithMatchingDatumAndValue
-- constraint expects a different Datum
phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum :: PSU.Language -> TestTree
phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongDatum l =
    let nScriptOutputs  = 5
        scriptOutputIdx = nScriptOutputs - 1
        wrongDatum = scriptOutputIdx - 1
    in checkPredicateOptions
        defaultCheckOptions
        "Phase-2 validation failure when onchain mustSpendScriptOutputWithMatchingDatumAndValue constraint expects a different TxOutRef"
        (assertFailedTransaction $ const $ \case
           L.ScriptFailure (EvaluationError ("Le":_) _) -> True
           _                                            -> False)
        $ void
        $ trace
        $ mustSpendScriptOutputWithMatchingDatumAndValueContract
            l nScriptOutputs
            (scriptOutputIdx, utxoValue)
            (wrongDatum, utxoValue)

-- | Phase-2 validation failure when onchain mustSpendScriptOutputWithMatchingDatumAndValue
-- constraint expects a different Value
phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue :: PSU.Language -> TestTree
phase2ErrorWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongValue l =
    let nScriptOutputs  = 5
        scriptOutputIdx = nScriptOutputs - 1
        wrongValue = Ada.lovelaceValueOf (1 + (Ada.getLovelace $ Ada.fromValue utxoValue))
    in checkPredicateOptions
        defaultCheckOptions
        "Phase-2 validation failure when onchain mustSpendScriptOutputWithMatchingDatumAndValue constraint expects a different TxOutRef"
        ( assertFailedTransaction $ const $ \case
           L.ScriptFailure (EvaluationError ("Le":_) _) -> True
           _                                            -> False
        )
        $ void $ trace
        $ mustSpendScriptOutputWithMatchingDatumAndValueContract
              l nScriptOutputs
              (scriptOutputIdx, utxoValue)
              (scriptOutputIdx, wrongValue)

-- | Phase-2 validation failure only when onchain mustSpendScriptOutputWithMatchingDatumAndValue
-- constraint expects a different Redeemer with V2+ script
phase2ErrorOnlyWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongRedeemerWithV2Script :: PSU.Language -> TestTree
phase2ErrorOnlyWhenMustSpendScriptOutputWithMatchingDatumAndValueUsesWrongRedeemerWithV2Script l =
    let check = case l of
            PlutusV1 ->
                checkPredicate
                "No phase-2 validation failure when V1 script using onchain mustSpendScriptOutputWithMatchingDatumAndValue constraint expects a different redeemer"
                (valueAtAddress someAddress (== V.scale 4 utxoValue)
                .&&. assertValidatedTransactionCount 2)
            PlutusV2 ->
                checkPredicate
                "Phase-2 validation failure when V2 script using onchain mustSpendScriptOutputWithMatchingDatumAndValue constraint expects a different redeemer"
                $ assertFailedTransaction $ const $ \case
                     L.ScriptFailure (EvaluationError ("Le":_) _) -> True
                     _                                            -> False
        nScriptOutputs  = 5
        scriptOutputIdx = nScriptOutputs - 1
    in check
     $ void
     $ trace
     $ mustSpendScriptOutputWithMatchingDatumAndValueContract'
         l nScriptOutputs
         (scriptOutputIdx, utxoValue)
         (scriptOutputIdx, utxoValue)
         False

mkMustSpendScriptOutputPolicy :: (Cons.TxConstraints () () -> sc -> Bool) -> [(Tx.TxOutRef, L.Redeemer)] -> sc -> Bool
mkMustSpendScriptOutputPolicy checkScriptContext constraintParams ctx =
    P.traceIfFalse
        "mustSpendScriptOutput not satisfied"
        (checkScriptContext (P.mconcat mustSpendScriptOutputs) ctx)
    where
        mustSpendScriptOutputs = P.map (uncurry Cons.mustSpendScriptOutput) constraintParams

mkMustSpendScriptOutputWithMatchingDatumAndValuePolicy
    :: (Cons.TxConstraints () () -> sc -> Bool)
    -> [(PV2.ValidatorHash, L.Datum, V.Value, L.Redeemer)] -> sc -> Bool
mkMustSpendScriptOutputWithMatchingDatumAndValuePolicy checkScriptContext constraintParams ctx =
    P.traceIfFalse
        "mustSpendScriptOutputWithMatchingDatumAndValue not satisfied"
        (checkScriptContext (P.mconcat mustSpendScriptOutputsWithMatchingDatumAndValue) ctx)
    where
        mustSpendScriptOutputsWithMatchingDatumAndValue = P.map (\(vh, datum, value, redeemer) ->
            Cons.mustSpendScriptOutputWithMatchingDatumAndValue vh (P.== datum) (P.==  value) redeemer) constraintParams

mkMustReferenceOutputValidator
    :: (Cons.TxConstraints Void Void -> sc -> Bool)
    -> PV1.TxOutRef -> () -> sc -> Bool
mkMustReferenceOutputValidator checkScriptContext txOutRef _ = checkScriptContext (Cons.mustReferenceOutput txOutRef)


{- Plutus version supports -}

data Script a where
   MustSpendScriptOutputPolicy :: Script MintingPolicy
   MustSpendScriptOutputWithMatchingDatumAndValuePolicy :: Script MintingPolicy
   MustReferenceOutputValidator :: Script Validator

getScript :: Script a -> PSU.Language -> a
getScript MustSpendScriptOutputPolicy PlutusV1 = mustSpendScriptOutputPolicyV1
getScript MustSpendScriptOutputPolicy PlutusV2 = mustSpendScriptOutputPolicyV2
getScript MustSpendScriptOutputWithMatchingDatumAndValuePolicy PlutusV1 =
    mustSpendScriptOutputWithMatchingDatumAndValuePolicyV1
getScript MustSpendScriptOutputWithMatchingDatumAndValuePolicy PlutusV2 =
    mustSpendScriptOutputWithMatchingDatumAndValuePolicyV2
getScript MustReferenceOutputValidator PlutusV1 = mustReferenceOutputValidatorV1
getScript MustReferenceOutputValidator PlutusV2 = mustReferenceOutputValidatorV2

getVersionedMintingPolicy :: Script a -> PSU.Language -> PSU.Versioned a
getVersionedMintingPolicy script l = PSU.Versioned (getScript script l) l

mintingPolicyHash :: Script MintingPolicy -> PSU.Language -> L.MintingPolicyHash
mintingPolicyHash script = \case
  PlutusV1 -> PSU.V1.mintingPolicyHash (getScript script PlutusV1)
  PlutusV2 -> PSU.V2.mintingPolicyHash (getScript script PlutusV2)

mintingPolicyCurrencySymbol :: Script MintingPolicy -> PSU.Language -> L.CurrencySymbol
mintingPolicyCurrencySymbol script = V.mpsSymbol . mintingPolicyHash script

scriptAddress :: Script Validator -> PSU.Language -> PV1.Address
scriptAddress x = \case
  PlutusV1 -> PSU.V1.mkValidatorAddress $ getScript x PlutusV1
  PlutusV2 -> PSU.V2.mkValidatorAddress $ getScript x PlutusV2

{-
    V1 Policies
-}

{-# INLINEABLE mustSpendScriptOutputPolicyV1 #-}
mustSpendScriptOutputPolicyV1 :: PV1.MintingPolicy
mustSpendScriptOutputPolicyV1 = PV1.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        mkMustSpendScriptOutputPolicyV1 = mkMustSpendScriptOutputPolicy Cons.V1.checkScriptContext
        wrap = PSU.V1.mkUntypedMintingPolicy mkMustSpendScriptOutputPolicyV1

{-# INLINEABLE mustSpendScriptOutputWithMatchingDatumAndValuePolicyV1 #-}
mustSpendScriptOutputWithMatchingDatumAndValuePolicyV1 :: PV1.MintingPolicy
mustSpendScriptOutputWithMatchingDatumAndValuePolicyV1 = PV1.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        mkMustSpendScriptOutputWithMatchingDatumAndValuePolicyV1
            = mkMustSpendScriptOutputWithMatchingDatumAndValuePolicy Cons.V1.checkScriptContext
        wrap = PSU.V1.mkUntypedMintingPolicy mkMustSpendScriptOutputWithMatchingDatumAndValuePolicyV1

{-# INLINABLE mustReferenceOutputValidatorV1 #-}
mustReferenceOutputValidatorV1 :: PV1.Validator
mustReferenceOutputValidatorV1 = PV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     mkMustReferenceOutputV1Validator = mkMustReferenceOutputValidator Cons.V1.checkScriptContext
     wrap = PSU.V1.mkUntypedValidator mkMustReferenceOutputV1Validator


{-
    V2 Policies
-}

{-# INLINEABLE mustSpendScriptOutputPolicyV2 #-}
mustSpendScriptOutputPolicyV2 :: PV2.MintingPolicy
mustSpendScriptOutputPolicyV2 = PV2.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        mkMustSpendScriptOutputPolicyV2 = mkMustSpendScriptOutputPolicy Cons.V2.checkScriptContext
        wrap = PSU.V2.mkUntypedMintingPolicy mkMustSpendScriptOutputPolicyV2

{-# INLINEABLE mustSpendScriptOutputWithMatchingDatumAndValuePolicyV2 #-}
mustSpendScriptOutputWithMatchingDatumAndValuePolicyV2 :: PV2.MintingPolicy
mustSpendScriptOutputWithMatchingDatumAndValuePolicyV2 = PV2.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        mkMustSpendScriptOutputWithMatchingDatumAndValuePolicyV2 =
            mkMustSpendScriptOutputWithMatchingDatumAndValuePolicy Cons.V2.checkScriptContext
        wrap = PSU.V2.mkUntypedMintingPolicy mkMustSpendScriptOutputWithMatchingDatumAndValuePolicyV2

{-# INLINABLE mustReferenceOutputValidatorV2 #-}
mustReferenceOutputValidatorV2 :: PV2.Validator
mustReferenceOutputValidatorV2 = PV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     mkMustReferenceOutputV2Validator = mkMustReferenceOutputValidator Cons.V2.checkScriptContext
     wrap = PSU.V2.mkUntypedValidator mkMustReferenceOutputV2Validator
