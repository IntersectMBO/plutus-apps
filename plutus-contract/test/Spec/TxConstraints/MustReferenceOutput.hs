{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.TxConstraints.MustReferenceOutput(tests) where

import Control.Lens ((??), (^.))
import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as Text
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Cons
import Ledger.Constraints.OnChain.V1 qualified as Cons.V1
import Ledger.Constraints.OnChain.V2 qualified as Cons.V2
import Ledger.Scripts (ScriptError (EvaluationError))
import Ledger.Test (asDatum, asRedeemer, someAddress, someValidatorHash)
import Ledger.Tx qualified as Tx
import Ledger.Tx.Constraints qualified as Tx.Cons
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.ChainIndex.Emulator (diskState)
import Plutus.ChainIndex.Emulator.DiskState (addressMap, unCredentialMap)
import Plutus.Contract as Con
import Plutus.Contract.Test (assertFailedTransaction, assertValidatedTransactionCount, checkPredicateOptions,
                             defaultCheckOptions, emulatorConfig, w1)
import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Script.Utils.V2.Scripts qualified as PSU.V2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2.Scripts
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Wallet.Emulator.Wallet (WalletState, chainIndexEmulatorState)

tests :: TestTree
tests =
    testGroup "MustReferenceOutput"
      [ testGroup "ledger constraints" $ [v1Tests, v2Tests] ?? ledgerSubmitTx
      --, testGroup "cardano constraints" $ [v1Tests, v2Tests] ?? cardanoSubmitTx
      ]

v1Tests :: SubmitTx -> TestTree
v1Tests sub = testGroup "Plutus V1" $
   [ v1FeaturesNotAvailableTests
   ] ?? sub ?? languageContextV1

v2Tests :: SubmitTx -> TestTree
v2Tests sub = testGroup "Plutus V2" $
  [ v2FeaturesTests
  ] ?? sub ?? languageContextV2

v1FeaturesNotAvailableTests :: SubmitTx -> LanguageContext -> TestTree
v1FeaturesNotAvailableTests sub t = testGroup "Plutus V1 features" $
    [ ledgerValidationtErrorWhenUsingV1Script
    , phase2FailureWhenUsingV1Script
    ] ?? sub ?? t

v2FeaturesTests :: SubmitTx -> LanguageContext -> TestTree
v2FeaturesTests sub t = testGroup "Plutus V2 features" $
    [ mustReferenceOutputWithSinglePubkeyOutput
    , mustReferenceOutputWithMultiplePubkeyOutputs
    , mustReferenceOutputWithSingleScriptOutput
    , ledgerValidationErrorWhenReferencingNonExistingTxo
    , phase2FailureWhenUsingV2Script
    ] ?? sub ?? t

tknValue :: LanguageContext -> Value.Value
tknValue tc = Value.singleton (mustPayToOtherScriptPolicyCurrencySymbol tc) "mint-me" 1

nonExistentTxoRef :: Tx.TxOutRef
nonExistentTxoRef =
    Tx.TxOutRef "bcf4064aed337a2d7d481d046b03c43457a020df7ae6ff182d1387979a30abcd" 123

defTrace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
defTrace contract = do
    void $ Trace.activateContractWallet w1 contract
    void $ Trace.waitNSlots 1

-- | Contract to a single transaction with mustSpendScriptOutputs offchain
-- constraint and mint with policy using matching onchain constraint.
mustReferenceOutputContract
    :: SubmitTx
    -> LanguageContext
    -> [Tx.TxOutRef]
    -> [Tx.TxOutRef]
    -> Contract () Empty ContractError ()
mustReferenceOutputContract submitTxFromConstraints lc offChainTxoRefs onChainTxoRefs = do
    let lookups1 = mintingPolicy lc $ mustReferenceOutputPolicy lc
        tx1 = mconcat mustReferenceOutputs
           <> Cons.mustMintValueWithRedeemer (asRedeemer onChainTxoRefs) (tknValue lc)
    ledgerTx1 <- submitTxFromConstraints lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1
        where
        mustReferenceOutputs :: [Cons.TxConstraints [Tx.TxOutRef] ()]
        mustReferenceOutputs = Cons.mustReferenceOutput <$> offChainTxoRefs

txoRefsFromWalletState :: WalletState -> Set Tx.TxOutRef
txoRefsFromWalletState ws =
    head $ M.elems $ ws ^. chainIndexEmulatorState . diskState . addressMap . unCredentialMap

-- needed to workaround bug 695
overrideW1TxOutRefs :: [Tx.TxOutRef] -> [Tx.TxOutRef]
overrideW1TxOutRefs = overrideTxOutRefIdxes 50

overrideTxOutRefIdxes :: Integer -> [Tx.TxOutRef] -> [Tx.TxOutRef]
overrideTxOutRefIdxes i = fmap (\r@Tx.TxOutRef{Tx.txOutRefIdx=idx} -> r{Tx.txOutRefIdx= idx + i})
--

-- | Ledger validation error occurs when attempting use of offchain mustReferenceOutput
--   constraint with V1 script
ledgerValidationtErrorWhenUsingV1Script :: SubmitTx -> LanguageContext -> TestTree
ledgerValidationtErrorWhenUsingV1Script submitTxFromConstraints lc =
    let contract = mustReferenceOutputContract submitTxFromConstraints lc
                    [nonExistentTxoRef] [nonExistentTxoRef]

    in checkPredicateOptions defaultCheckOptions
    ("Ledger validation error occurs when attempting use of offchain mustReferenceOutput " ++
     "constraint with V1 script")
    (assertFailedTransaction (\_ err ->
        case err of {Ledger.CardanoLedgerValidationError msg ->
            Text.isInfixOf "ReferenceInputsNotSupported" msg; _ -> False  }))
    (void $ defTrace contract)

-- | Phase-2 validation error occurs when attempting to use onchain mustReferenceOutput
-- constraint with V1 script
phase2FailureWhenUsingV1Script :: SubmitTx -> LanguageContext -> TestTree
phase2FailureWhenUsingV1Script = phase2FailureWithMustReferenceOutput
    ("Phase-2 validation error occurs when attempting to use onchain mustReferenceOutput " ++
    "constraint with V1 script")

-- | Phase-2 validation error occurs when using onchain mustReferenceOutput
-- constraint with V2 script and and reference input is not in txbody
phase2FailureWhenUsingV2Script :: SubmitTx -> LanguageContext -> TestTree
phase2FailureWhenUsingV2Script = phase2FailureWithMustReferenceOutput
    ("Phase-2 validation error occurs when using onchain mustReferenceOutput" ++
    "constraint with V2 script and and reference input is not in txbody")

phase2FailureWithMustReferenceOutput :: String -> SubmitTx -> LanguageContext -> TestTree
phase2FailureWithMustReferenceOutput testDescription submitTxFromConstraints lc =
    let contractWithoutOffchainConstraint = do
            let lookups1 = mintingPolicy lc $ mustReferenceOutputPolicy lc
                tx1 = Cons.mustMintValueWithRedeemer (asRedeemer [nonExistentTxoRef]) (tknValue lc)
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicateOptions defaultCheckOptions
    testDescription
    (assertFailedTransaction (\_ err ->
        case err of {Ledger.ScriptFailure (EvaluationError ("Lf":_) _) -> True; _ -> False }))
    (void $ defTrace contractWithoutOffchainConstraint)

-- | Valid scenario using offchain and onchain constraint
-- mustReferenceOutput once for a single pubkey output.
mustReferenceOutputWithSinglePubkeyOutput :: SubmitTx -> LanguageContext -> TestTree
mustReferenceOutputWithSinglePubkeyOutput submitTxFromConstraints lc =
    let trace = do
            w1State <- Trace.agentState w1
            let w1TxoRefs = txoRefsFromWalletState w1State
                w1MiddleTxoRef = [S.elemAt (length w1TxoRefs `div` 2) w1TxoRefs]
                overridedW1TxoRefs = overrideW1TxOutRefs w1MiddleTxoRef -- need to override index due to bug 695
                contract =
                    mustReferenceOutputContract submitTxFromConstraints lc
                    overridedW1TxoRefs overridedW1TxoRefs
            void $ Trace.activateContractWallet w1 contract
            void $ Trace.waitNSlots 1

    in checkPredicateOptions defaultCheckOptions
    ("Valid scenario using offchain and onchain constraint " ++
    "mustReferenceOutput once for a single pubkey output")
    (assertValidatedTransactionCount 1)
    (void trace)

-- | Valid scenario using offchain and onchain constraint
-- mustReferenceOutput once for multiple pubkey outputs.
mustReferenceOutputWithMultiplePubkeyOutputs :: SubmitTx -> LanguageContext -> TestTree
mustReferenceOutputWithMultiplePubkeyOutputs submitTxFromConstraints lc =
    let trace = do
            w1State <- Trace.agentState w1
            let w1TxoRefs = txoRefsFromWalletState w1State
                overridedW1TxoRefs = overrideW1TxOutRefs $ S.toList w1TxoRefs -- need to override index due to bug 695
                contract =
                    mustReferenceOutputContract submitTxFromConstraints lc
                    overridedW1TxoRefs overridedW1TxoRefs
            void $ Trace.activateContractWallet w1 contract
            void $ Trace.waitNSlots 1

    in checkPredicateOptions defaultCheckOptions
    ("Valid scenario using offchain and onchain constraint " ++
    "mustReferenceOutput once for multiple pubkey outputs.")
    (assertValidatedTransactionCount 1)
    (void trace)

-- | Valid scenario using offchain and onchain constraint
-- mustReferenceOutput once for a single script output.
mustReferenceOutputWithSingleScriptOutput :: SubmitTx -> LanguageContext -> TestTree
mustReferenceOutputWithSingleScriptOutput submitTxFromConstraints lc =
    let contractWithScriptOutput = do
            let tx1 = Cons.mustPayToOtherScript someValidatorHash
                      (asDatum $ PlutusTx.toBuiltinData ()) (Ada.lovelaceValueOf 2_000_000)
            ledgerTx1 <- submitTx tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

            scriptUtxos <- utxosAt someAddress
            let scriptUtxo = head $ M.keys scriptUtxos
                lookups2 = mintingPolicy lc (mustReferenceOutputPolicy lc)
                        <> Cons.unspentOutputs scriptUtxos
                tx2 = Cons.mustReferenceOutput scriptUtxo
                   <> Cons.mustMintValueWithRedeemer (asRedeemer [scriptUtxo]) (tknValue lc)
            ledgerTx2 <- submitTxFromConstraints lookups2 tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

    in checkPredicateOptions defaultCheckOptions
    ("Valid scenario using offchain and onchain constraint " ++
    "mustReferenceOutput once for a single script output")
    (assertValidatedTransactionCount 2)
    (void $ defTrace contractWithScriptOutput)

-- | Ledger validation error occurs when attempting use of offchain mustReferenceOutput
--   constraint with a txo that doesn't exist
ledgerValidationErrorWhenReferencingNonExistingTxo :: SubmitTx -> LanguageContext -> TestTree
ledgerValidationErrorWhenReferencingNonExistingTxo submitTxFromConstraints lc =
    let contract = mustReferenceOutputContract submitTxFromConstraints lc
                    [nonExistentTxoRef] [nonExistentTxoRef]

    in checkPredicateOptions defaultCheckOptions
    ("Ledger validation error occurs when using offchain mustReferenceOutput " ++
     "constraint with a txo that doesn't exist")
    (assertFailedTransaction (\_ err ->
        case err of {Ledger.CardanoLedgerValidationError msg ->
            Text.isInfixOf "TranslationLogicMissingInput" msg; _ -> False  }))
    (void $ defTrace contract)

data UnitTest
instance Scripts.ValidatorTypes UnitTest where
    type instance DatumType UnitTest = ()
    type instance RedeemerType UnitTest = [Tx.TxOutRef]

mkMustReferenceOutputPolicy :: (Cons.TxConstraints () () -> sc -> Bool)
                            -> [Tx.TxOutRef ] -> sc -> Bool
mkMustReferenceOutputPolicy checkScriptContext txOutRefs =
    checkScriptContext (P.mconcat mustReferenceOutputs)
    where
        mustReferenceOutputs = Cons.mustReferenceOutput P.<$> txOutRefs

mustReferenceOutputPolicyV1 :: Ledger.MintingPolicy
mustReferenceOutputPolicyV1 = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        checkedMkMustPayToOtherScriptPolicy = mkMustReferenceOutputPolicy Cons.V1.checkScriptContext
        wrap = Scripts.mkUntypedMintingPolicy checkedMkMustPayToOtherScriptPolicy

mustReferenceOutputPolicyV2 :: Ledger.MintingPolicy
mustReferenceOutputPolicyV2 = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        checkedMkMustPayToOtherScriptPolicy = mkMustReferenceOutputPolicy Cons.V2.checkScriptContext
        wrap = V2.Scripts.mkUntypedMintingPolicy checkedMkMustPayToOtherScriptPolicy

data LanguageContext
   = LanguageContext
   { mustReferenceOutputPolicy :: Ledger.MintingPolicy
   , mintingPolicy             :: forall a. Ledger.MintingPolicy -> Cons.ScriptLookups a
   , mintingPolicyHash         :: Ledger.MintingPolicy -> Ledger.MintingPolicyHash
   }

languageContextV1 :: LanguageContext
languageContextV1 = LanguageContext
    mustReferenceOutputPolicyV1
    Cons.plutusV1MintingPolicy
    PSU.V1.mintingPolicyHash


languageContextV2 :: LanguageContext
languageContextV2 = LanguageContext
    mustReferenceOutputPolicyV2
    Cons.plutusV2MintingPolicy
    PSU.V2.mintingPolicyHash


type SubmitTx
  =  Cons.ScriptLookups UnitTest
  -> Cons.TxConstraints (Scripts.RedeemerType UnitTest) (Scripts.DatumType UnitTest)
  -> Contract () Empty ContractError Tx.CardanoTx

cardanoSubmitTx :: SubmitTx
cardanoSubmitTx lookups tx = let
  p = defaultCheckOptions ^. emulatorConfig . Trace.params
  in submitUnbalancedTx $ either (error . show) id $ Tx.Cons.mkTx @UnitTest p lookups tx

ledgerSubmitTx :: SubmitTx
ledgerSubmitTx = submitTxConstraintsWith


mustPayToOtherScriptPolicyHash :: LanguageContext -> Ledger.MintingPolicyHash
mustPayToOtherScriptPolicyHash lc = mintingPolicyHash lc $ mustReferenceOutputPolicy lc

mustPayToOtherScriptPolicyCurrencySymbol :: LanguageContext -> Ledger.CurrencySymbol
mustPayToOtherScriptPolicyCurrencySymbol = Value.mpsSymbol . mustPayToOtherScriptPolicyHash
