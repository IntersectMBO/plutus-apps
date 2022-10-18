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
module Spec.TxConstraints.MustReferenceOutput(tests) where

import Control.Lens ((??), (^.))
import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as Text
import Ledger qualified as L
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
import Plutus.Script.Utils.Scripts qualified as PSU
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
   ] ?? sub ?? PSU.PlutusV1

v2Tests :: SubmitTx -> TestTree
v2Tests sub = testGroup "Plutus V2" $
  [ v2FeaturesTests
  ] ?? sub ?? PSU.PlutusV2

v1FeaturesNotAvailableTests :: SubmitTx -> PSU.Language -> TestTree
v1FeaturesNotAvailableTests sub t = testGroup "Plutus V1 features" $
    [ ledgerValidationtErrorWhenUsingV1Script
    , phase2FailureWhenUsingV1Script
    ] ?? sub ?? t

v2FeaturesTests :: SubmitTx -> PSU.Language -> TestTree
v2FeaturesTests sub t = testGroup "Plutus V2 features" $
    [ mustReferenceOutputWithSinglePubkeyOutput
    , mustReferenceOutputWithMultiplePubkeyOutputs
    , mustReferenceOutputWithSingleScriptOutput
    , ledgerValidationErrorWhenReferencingNonExistingTxo
    , phase2FailureWhenUsingV2Script
    ] ?? sub ?? t

tknValue :: PSU.Language -> Value.Value
tknValue l = Value.singleton (PSU.scriptCurrencySymbol $ getVersionedScript MustReferenceOutputPolicy l) "mint-me" 1

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
    -> PSU.Language
    -> [Tx.TxOutRef]
    -> [Tx.TxOutRef]
    -> Contract () Empty ContractError ()
mustReferenceOutputContract submitTxFromConstraints l offChainTxoRefs onChainTxoRefs = do
    let lookups1 = Cons.mintingPolicy (getVersionedScript MustReferenceOutputPolicy l)
        tx1 = mconcat mustReferenceOutputs
           <> Cons.mustMintValueWithRedeemer (asRedeemer onChainTxoRefs) (tknValue l)
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
ledgerValidationtErrorWhenUsingV1Script :: SubmitTx -> PSU.Language -> TestTree
ledgerValidationtErrorWhenUsingV1Script submitTxFromConstraints l =
    let contract = mustReferenceOutputContract submitTxFromConstraints l
                    [nonExistentTxoRef] [nonExistentTxoRef]

    in checkPredicateOptions defaultCheckOptions
    ("Ledger validation error occurs when attempting use of offchain mustReferenceOutput " ++
     "constraint with V1 script")
    (assertFailedTransaction (\_ err ->
        case err of {L.CardanoLedgerValidationError msg ->
            Text.isInfixOf "ReferenceInputsNotSupported" msg; _ -> False  }))
    (void $ defTrace contract)

-- | Phase-2 validation error occurs when attempting to use onchain mustReferenceOutput
-- constraint with V1 script
phase2FailureWhenUsingV1Script :: SubmitTx -> PSU.Language -> TestTree
phase2FailureWhenUsingV1Script = phase2FailureWithMustReferenceOutput
    ("Phase-2 validation error occurs when attempting to use onchain mustReferenceOutput " ++
    "constraint with V1 script")

-- | Phase-2 validation error occurs when using onchain mustReferenceOutput
-- constraint with V2 script and and reference input is not in txbody
phase2FailureWhenUsingV2Script :: SubmitTx -> PSU.Language -> TestTree
phase2FailureWhenUsingV2Script = phase2FailureWithMustReferenceOutput
    ("Phase-2 validation error occurs when using onchain mustReferenceOutput" ++
    "constraint with V2 script and and reference input is not in txbody")

phase2FailureWithMustReferenceOutput :: String -> SubmitTx -> PSU.Language -> TestTree
phase2FailureWithMustReferenceOutput testDescription submitTxFromConstraints l =
    let contractWithoutOffchainConstraint = do
            let lookups1 = Cons.mintingPolicy (getVersionedScript MustReferenceOutputPolicy l)
                tx1 = Cons.mustMintValueWithRedeemer (asRedeemer [nonExistentTxoRef]) (tknValue l)
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicateOptions defaultCheckOptions
    testDescription
    (assertFailedTransaction (\_ err ->
        case err of {L.ScriptFailure (EvaluationError ("Lf":_) _) -> True; _ -> False }))
    (void $ defTrace contractWithoutOffchainConstraint)

-- | Valid scenario using offchain and onchain constraint
-- mustReferenceOutput once for a single pubkey output.
mustReferenceOutputWithSinglePubkeyOutput :: SubmitTx -> PSU.Language -> TestTree
mustReferenceOutputWithSinglePubkeyOutput submitTxFromConstraints l =
    let trace = do
            w1State <- Trace.agentState w1
            let w1TxoRefs = txoRefsFromWalletState w1State
                w1MiddleTxoRef = [S.elemAt (length w1TxoRefs `div` 2) w1TxoRefs]
                overridedW1TxoRefs = overrideW1TxOutRefs w1MiddleTxoRef -- need to override index due to bug 695
                contract =
                    mustReferenceOutputContract submitTxFromConstraints l
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
mustReferenceOutputWithMultiplePubkeyOutputs :: SubmitTx -> PSU.Language -> TestTree
mustReferenceOutputWithMultiplePubkeyOutputs submitTxFromConstraints l =
    let trace = do
            w1State <- Trace.agentState w1
            let w1TxoRefs = txoRefsFromWalletState w1State
                overridedW1TxoRefs = overrideW1TxOutRefs $ S.toList w1TxoRefs -- need to override index due to bug 695
                contract =
                    mustReferenceOutputContract submitTxFromConstraints l
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
mustReferenceOutputWithSingleScriptOutput :: SubmitTx -> PSU.Language -> TestTree
mustReferenceOutputWithSingleScriptOutput submitTxFromConstraints l =
    let contractWithScriptOutput = do
            let tx1 = Cons.mustPayToOtherScript someValidatorHash
                      (asDatum $ PlutusTx.toBuiltinData ()) (Ada.lovelaceValueOf 2_000_000)
            ledgerTx1 <- submitTx tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

            scriptUtxos <- utxosAt someAddress
            let scriptUtxo = head $ M.keys scriptUtxos
                lookups2 = Cons.mintingPolicy (getVersionedScript MustReferenceOutputPolicy l)
                        <> Cons.unspentOutputs scriptUtxos
                tx2 = Cons.mustReferenceOutput scriptUtxo
                   <> Cons.mustMintValueWithRedeemer (asRedeemer [scriptUtxo]) (tknValue l)
            ledgerTx2 <- submitTxFromConstraints lookups2 tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

    in checkPredicateOptions defaultCheckOptions
    ("Valid scenario using offchain and onchain constraint " ++
    "mustReferenceOutput once for a single script output")
    (assertValidatedTransactionCount 2)
    (void $ defTrace contractWithScriptOutput)

-- | Ledger validation error occurs when attempting use of offchain mustReferenceOutput
--   constraint with a txo that doesn't exist
ledgerValidationErrorWhenReferencingNonExistingTxo :: SubmitTx -> PSU.Language -> TestTree
ledgerValidationErrorWhenReferencingNonExistingTxo submitTxFromConstraints l =
    let contract = mustReferenceOutputContract submitTxFromConstraints l
                    [nonExistentTxoRef] [nonExistentTxoRef]

    in checkPredicateOptions defaultCheckOptions
    ("Ledger validation error occurs when using offchain mustReferenceOutput " ++
     "constraint with a txo that doesn't exist")
    (assertFailedTransaction (\_ err ->
        case err of {L.CardanoLedgerValidationError msg ->
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

mustReferenceOutputPolicyV1 :: L.MintingPolicy
mustReferenceOutputPolicyV1 = L.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        checkedMkMustPayToOtherScriptPolicy = mkMustReferenceOutputPolicy Cons.V1.checkScriptContext
        wrap = Scripts.mkUntypedMintingPolicy checkedMkMustPayToOtherScriptPolicy

mustReferenceOutputPolicyV2 :: L.MintingPolicy
mustReferenceOutputPolicyV2 = L.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        checkedMkMustPayToOtherScriptPolicy = mkMustReferenceOutputPolicy Cons.V2.checkScriptContext
        wrap = V2.Scripts.mkUntypedMintingPolicy checkedMkMustPayToOtherScriptPolicy
data Script a where
   MustReferenceOutputPolicy :: Script L.MintingPolicy

getScript :: Script a -> PSU.Language -> a
getScript MustReferenceOutputPolicy PSU.PlutusV1 = mustReferenceOutputPolicyV1
getScript MustReferenceOutputPolicy PSU.PlutusV2 = mustReferenceOutputPolicyV2

getVersionedScript :: Script a -> PSU.Language -> PSU.Versioned a
getVersionedScript script l = PSU.Versioned (getScript script l) l

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
