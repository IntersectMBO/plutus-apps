{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Spec.Contract.Tx.Constraints.MustUseOutputAsCollateral(tests) where

import Control.Lens ((??), (^.))
import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Cardano.Api.Shelley (protocolParamMaxCollateralInputs)
import Data.Default (Default (def))
import Data.Map as M
import Data.Maybe (fromJust)
import Ledger qualified as L
import Ledger qualified as PSU
import Ledger.Ada qualified as Ada
import Ledger.CardanoWallet (paymentPrivateKey)
import Ledger.Constraints qualified as Cons
import Ledger.Constraints.OffChain qualified as OffCon
import Ledger.Constraints.OnChain.V1 qualified as Cons
import Ledger.Constraints.OnChain.V2 qualified as V2.Cons
import Ledger.Test (asRedeemer, someAddressV2, someTypedValidatorV2)
import Ledger.Tx qualified as Tx
import Ledger.Tx.Constraints qualified as Tx.Cons
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertUnbalancedTx, assertValidatedTransactionCount,
                             assertValidatedTransactionCountOfTotal, checkPredicateOptions, defaultCheckOptions,
                             emulatorConfig, mockWalletPaymentPubKeyHash, w1, w2, (.&&.))
import Plutus.Script.Utils.Typed (Any)
import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Script.Utils.V2.Scripts qualified as PSU.V2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2.Scripts
import Plutus.Trace.Emulator qualified as Trace (EmulatorTrace, activateContractWallet, nextSlot, params,
                                                 setSigningProcess, walletInstanceTag)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Wallet.Emulator.Wallet as Wallet (signPrivateKeys, walletToMockWallet')

tests :: TestTree
tests =
    testGroup "MustUseOutputAsCollateral"
      [ testGroup "ledger constraints" $ [v1Tests, v2Tests] ?? ledgerSubmitTx
      --, testGroup "cardano constraints" $ [v1Tests, v2Tests] ?? cardanoSubmitTx
      ]

v1Tests :: SubmitTx -> TestTree
v1Tests sub = testGroup "Plutus V1" $
   [ v1FeaturesTests
   --, v2FeaturesNotAvailableTests
   ] ?? sub ?? PSU.PlutusV1

v2Tests :: SubmitTx -> TestTree
v2Tests sub = testGroup "Plutus V2" $
  [ v1FeaturesTests
  --, v2FeaturesTests
  ] ?? sub ?? PSU.PlutusV2

v1FeaturesTests :: SubmitTx -> PSU.Language -> TestTree
v1FeaturesTests sub t = testGroup "Plutus V1 features" $
    [ singleUseOfMustUseOutputAsCollateral
    , multipleUseOfMustUseOutputAsCollateral
    , usingMustUseOutputAsCollateralWithOtherWalletUtxo
    , useOfMustUseOutputAsCollateralWithoutPlutusScript
    , ledgerValidationErrorWhenUsingMustUseOutputAsCollateralWithScriptUtxo
    , ledgerValidationErrorWhenMustUseOutputAsCollateralExceedsMaximumCollateralInputs
    -- (no phase2 tests because this error can never occur)
    -- (no tests to check balanced tx for when balancer does/doesn't include collateral inputs)
    ] ?? sub ?? t

utxoValue :: Value.Value
utxoValue = Ada.lovelaceValueOf 10_000_000

tknAmount :: Integer
tknAmount = 5_000_000

tknValueOf :: Integer -> PSU.Language -> Value.Value
tknValueOf x tc = Value.singleton (mustUseOutputAsCollateralPolicyCurrencySymbol tc) "mint-me" x

tknValue :: PSU.Language -> Value.Value
tknValue = tknValueOf tknAmount

w1PaymentPubKeyHash :: L.PaymentPubKeyHash
w1PaymentPubKeyHash = mockWalletPaymentPubKeyHash w1

w2PaymentPubKeyHash :: L.PaymentPubKeyHash
w2PaymentPubKeyHash = mockWalletPaymentPubKeyHash w2

maximumCollateralInputs :: Integer
maximumCollateralInputs = fromIntegral $ fromJust $ protocolParamMaxCollateralInputs $ def

trace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void Trace.nextSlot

-- | Contract with a single transaction using mustUseOutputAsCollateral offchain constraint and
-- mint with policy using matching onchain constraint.
mustUseOutputAsCollateralContract :: SubmitTx -> PSU.Language -> Integer ->
                                     L.PaymentPubKeyHash -> Contract () Empty ContractError ()
mustUseOutputAsCollateralContract submitTxFromConstraints lc numberOfCollateralInputs pkh = do
    pubKeyUtxos <- utxosAt $ L.pubKeyHashAddress pkh Nothing
    let collateralUtxos = M.keys $ M.take (fromIntegral numberOfCollateralInputs) pubKeyUtxos
        lookups1 = Cons.unspentOutputs pubKeyUtxos
                <> mintingPolicy lc (mustUseOutputAsCollateralPolicy lc)
        tx1 = mconcat (mustUseOutputsAsCollateral collateralUtxos)
           <> Cons.mustMintValueWithRedeemer (asRedeemer collateralUtxos) (tknValue lc)
    ledgerTx1 <- submitTxFromConstraints lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1
    where
        mustUseOutputsAsCollateral utxos = Cons.mustUseOutputAsCollateral <$> utxos

-- | Valid scenario using offchain and onchain constraint mustUseOutputAsCollateral to select
-- a specific utxo to use as collateral input
singleUseOfMustUseOutputAsCollateral :: SubmitTx -> PSU.Language -> TestTree
singleUseOfMustUseOutputAsCollateral submitTxFromConstraints lc =
    let contract = mustUseOutputAsCollateralContract submitTxFromConstraints lc 1 w1PaymentPubKeyHash
    in checkPredicateOptions defaultCheckOptions
      ("Successful use of offchain and onchain mustUseOutputAsCollateral for a single " ++
       "collateral input")
      (assertValidatedTransactionCount 1 .&&.
        (assertUnbalancedTx contract
        (Trace.walletInstanceTag w1)
        (\unT -> length (Tx.getCardanoTxCollateralInputs $ Tx.EmulatorTx $ unT ^. OffCon.tx) ==  1)
        "correct number of collateral inputs"))
      (void $ trace contract)

-- | Valid scenario using offchain and onchain constraint mustUseOutputAsCollateral to select
-- multiple utxos to use as collateral input
multipleUseOfMustUseOutputAsCollateral :: SubmitTx -> PSU.Language -> TestTree
multipleUseOfMustUseOutputAsCollateral submitTxFromConstraints lc =
    let contract = mustUseOutputAsCollateralContract submitTxFromConstraints
                  lc maximumCollateralInputs w1PaymentPubKeyHash
    in checkPredicateOptions defaultCheckOptions
      ("Successful use of offchain and onchain mustUseOutputAsCollateral for the maximum " ++
       "number of allowed collateral inputs")
      (assertValidatedTransactionCount 1 .&&.
        (assertUnbalancedTx contract
        (Trace.walletInstanceTag w1)
        (\unT ->
          length (Tx.getCardanoTxCollateralInputs $ Tx.EmulatorTx $ unT ^. OffCon.tx)
            ==  fromIntegral maximumCollateralInputs)
        "correct number of collateral inputs"))
      (void $ trace contract)

-- | Valid scenario when using offchain and onchain constraint mustUseOutputAsCollateral with other
-- wallet's utxo
usingMustUseOutputAsCollateralWithOtherWalletUtxo
    :: SubmitTx
    -> PSU.Language
    -> TestTree
usingMustUseOutputAsCollateralWithOtherWalletUtxo submitTxFromConstraints lc =
    let numberOfCollateralInputs = 2
        contract = mustUseOutputAsCollateralContract submitTxFromConstraints lc
                    numberOfCollateralInputs w1PaymentPubKeyHash
        traceWithW2Signing = do
            Trace.setSigningProcess w1 (Just $ signPrivateKeys
                [paymentPrivateKey $ walletToMockWallet' w1,
                paymentPrivateKey $ walletToMockWallet' w2])
            void $ Trace.activateContractWallet w1 contract
            Trace.nextSlot
    in checkPredicateOptions defaultCheckOptions
    "Successful use of offchain and onchain mustUseOutputAsCollateral when using other wallet's utxo"
    (assertValidatedTransactionCount 1 .&&.
    (assertUnbalancedTx contract
    (Trace.walletInstanceTag w1)
    (\unT ->
        length (Tx.getCardanoTxCollateralInputs $ Tx.EmulatorTx $ unT ^. OffCon.tx)
        ==  fromIntegral numberOfCollateralInputs)
    "correct number of collateral inputs"))
    (void traceWithW2Signing)

-- | Valid scenario where offchain constraints mustUseOutputAsCollateral is used when there
-- are no plutus scripts in the tx. Collateral input is still included in tx
useOfMustUseOutputAsCollateralWithoutPlutusScript :: SubmitTx -> PSU.Language -> TestTree
useOfMustUseOutputAsCollateralWithoutPlutusScript submitTxFromConstraints _ =
    let numberOfCollateralInputs = 1
        contract = do
            ownPkh <- ownPaymentPubKeyHash
            pubKeyUtxos <- utxosAt $ L.pubKeyHashAddress ownPkh Nothing
            let utxo = head $ (M.keys $ M.take numberOfCollateralInputs pubKeyUtxos)
                lookups1 = Cons.unspentOutputs pubKeyUtxos
                tx1 = Cons.mustUseOutputAsCollateral utxo
                  <> Cons.mustPayToPubKey ownPkh (Ada.adaValueOf 5)
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicateOptions defaultCheckOptions
      ("Use of offchain mustUseOutputAsCollateral when there are no plutus scripts in tx " ++
      "still includes collateral input")
      (assertValidatedTransactionCount 1 .&&.
        (assertUnbalancedTx contract
        (Trace.walletInstanceTag w1)
        (\unT -> length (Tx.getCardanoTxCollateralInputs $ Tx.EmulatorTx $ unT ^. OffCon.tx)
            ==  numberOfCollateralInputs)
        "correct number of collateral inputs"))
      (void $ trace contract)

-- | Ledger validation error scenario when offchain constraint mustUseOutputAsCollateral is used
-- with a script's utxo
ledgerValidationErrorWhenUsingMustUseOutputAsCollateralWithScriptUtxo :: SubmitTx -> PSU.Language -> TestTree
ledgerValidationErrorWhenUsingMustUseOutputAsCollateralWithScriptUtxo submitTxFromConstraints lc =
    let numberOfCollateralInputs = 1
        contract :: Contract () Empty ContractError ()
        contract = do
            let lookups1 = Cons.typedValidatorLookups someTypedValidatorV2
                tx1 = Cons.mustPayToTheScriptWithDatumInTx (PlutusTx.toBuiltinData ()) utxoValue
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

            ownPkh <- ownPaymentPubKeyHash
            pubKeyUtxos <- utxosAt $ L.pubKeyHashAddress ownPkh Nothing
            scriptUtxos <- utxosAt someAddressV2
            let collaterealUtxo = head $ (M.keys $ M.take numberOfCollateralInputs scriptUtxos)
                lookups2 = Cons.typedValidatorLookups someTypedValidatorV2
                        <> Cons.unspentOutputs scriptUtxos
                        <> Cons.unspentOutputs pubKeyUtxos
                        <> mintingPolicy lc (mustUseOutputAsCollateralPolicy lc)
                tx2 = Cons.mustUseOutputAsCollateral collaterealUtxo
                   <> Cons.mustPayToPubKey ownPkh (Ada.adaValueOf 5)
                   <> Cons.mustMintValueWithRedeemer (asRedeemer [collaterealUtxo]) (tknValue lc)
            ledgerTx2 <- submitTxFromConstraints lookups2 tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

    in checkPredicateOptions defaultCheckOptions
        ("Ledger error when offchain constraint mustUseOutputAsCollateralToSatisfyAllCollateral is " ++
         "used with script's utxo (ScriptsNotPaidUTxO)")
        (assertValidatedTransactionCountOfTotal 1 2 .&&.
        assertUnbalancedTx contract
            (Trace.walletInstanceTag w1)
            (\unT ->
                length (Tx.getCardanoTxCollateralInputs $ Tx.EmulatorTx $ unT ^. OffCon.tx)
                    ==  numberOfCollateralInputs)
                "correct number of collateral inputs")
        (void $ trace contract)

-- | Ledger validation error scenario when offchain constraint mustUseOutputAsCollateral is used
-- to exceed allowed maximum collatereal inputs (network protocol param)
ledgerValidationErrorWhenMustUseOutputAsCollateralExceedsMaximumCollateralInputs
    :: SubmitTx
    -> PSU.Language
    -> TestTree
ledgerValidationErrorWhenMustUseOutputAsCollateralExceedsMaximumCollateralInputs submitTxFromConstraints lc =
    let moreThanMaximumCollateralInputs = (succ maximumCollateralInputs)
        contract = mustUseOutputAsCollateralContract submitTxFromConstraints lc
                    moreThanMaximumCollateralInputs w1PaymentPubKeyHash
    in checkPredicateOptions defaultCheckOptions
    ("Ledger error when offchain mustUseOutputAsCollateralToSatisfyAllCollateral is used more " ++
     "than maximum number of allowed collateral inputs (TooManyCollateralInputs)")
    (assertValidatedTransactionCountOfTotal 0 1 .&&.
        assertUnbalancedTx contract
            (Trace.walletInstanceTag w1)
            (\unT ->
                length (Tx.getCardanoTxCollateralInputs $ Tx.EmulatorTx $ unT ^. OffCon.tx)
                    ==  fromIntegral moreThanMaximumCollateralInputs)
                "correct number of collateral inputs")
    (void $ trace contract)

mkMustUseOutputAsCollateralPolicy :: (Cons.TxConstraints () () -> sc -> Bool) -> [Tx.TxOutRef] -> sc -> Bool
mkMustUseOutputAsCollateralPolicy checkScriptContext txOutRefs = checkScriptContext (P.mconcat mustUseOutputsAsCollateral)
    where
        mustUseOutputsAsCollateral = Cons.mustUseOutputAsCollateral P.<$> txOutRefs

mustUseOutputAsCollateralPolicyV1 :: L.MintingPolicy
mustUseOutputAsCollateralPolicyV1 = L.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        checkedMkMustUseOutputAsCollateralPolicy = mkMustUseOutputAsCollateralPolicy Cons.checkScriptContext
        wrap = Scripts.mkUntypedMintingPolicy checkedMkMustUseOutputAsCollateralPolicy

mustUseOutputAsCollateralPolicyV2 :: L.MintingPolicy
mustUseOutputAsCollateralPolicyV2 = L.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        checkedMkMustUseOutputAsCollateralPolicy = mkMustUseOutputAsCollateralPolicy V2.Cons.checkScriptContext
        wrap = V2.Scripts.mkUntypedMintingPolicy checkedMkMustUseOutputAsCollateralPolicy

mustUseOutputAsCollateralPolicy :: PSU.Language -> L.MintingPolicy
mustUseOutputAsCollateralPolicy = \case
  PSU.PlutusV1 -> mustUseOutputAsCollateralPolicyV1
  PSU.PlutusV2 -> mustUseOutputAsCollateralPolicyV2

mintingPolicy :: PSU.Language -> forall a. L.MintingPolicy -> Cons.ScriptLookups a
mintingPolicy = \case
  PSU.PlutusV1 -> Cons.plutusV1MintingPolicy
  PSU.PlutusV2 -> Cons.plutusV2MintingPolicy

mintingPolicyHash :: PSU.Language -> L.MintingPolicy -> L.MintingPolicyHash
mintingPolicyHash = \case
  PSU.PlutusV1 -> PSU.V1.mintingPolicyHash
  PSU.PlutusV2 -> PSU.V2.mintingPolicyHash

type SubmitTx
  =  Cons.ScriptLookups Any
  -> Cons.TxConstraints (Scripts.RedeemerType Any) (Scripts.DatumType Any)
  -> Contract () Empty ContractError Tx.CardanoTx

cardanoSubmitTx :: SubmitTx
cardanoSubmitTx lookups tx = let
  p = defaultCheckOptions ^. emulatorConfig . Trace.params
  in submitUnbalancedTx $ either (error . show) id $ Tx.Cons.mkTx @Any p lookups tx

ledgerSubmitTx :: SubmitTx
ledgerSubmitTx = submitTxConstraintsWith

mustUseOutputAsCollateralPolicyHash :: PSU.Language -> L.MintingPolicyHash
mustUseOutputAsCollateralPolicyHash lc = mintingPolicyHash lc $ mustUseOutputAsCollateralPolicy lc

mustUseOutputAsCollateralPolicyCurrencySymbol :: PSU.Language -> L.CurrencySymbol
mustUseOutputAsCollateralPolicyCurrencySymbol = Value.mpsSymbol . mustUseOutputAsCollateralPolicyHash
