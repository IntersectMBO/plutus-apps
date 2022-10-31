{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
-- disabled these options to simplify applyNowToTimeValidity
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates
                -fno-warn-incomplete-uni-patterns
                #-}
module Spec.Contract.Tx.Constraints.MustSatisfyAnyOf(tests) where

import Control.Lens ((??), (^.))
import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Data.Default (Default (def))
import Data.Maybe (isJust)
import GHC.Generics (Generic)
import Ledger (ScriptError (EvaluationError), unitDatum)
import Ledger qualified as L
import Ledger.Ada qualified as Ada
import Ledger.Constraints.OffChain qualified as Cons (ScriptLookups, mintingPolicy, plutusV1MintingPolicy,
                                                      plutusV2MintingPolicy)
import Ledger.Constraints.OnChain.V1 qualified as Cons.V1
import Ledger.Constraints.OnChain.V2 qualified as Cons.V2
import Ledger.Constraints.TxConstraints qualified as Cons (TxConstraints, mustBeSignedBy, mustIncludeDatumInTx,
                                                           mustMintValue, mustMintValueWithRedeemer,
                                                           mustPayToOtherScript, mustPayToOtherScriptWithDatumInTx,
                                                           mustPayToPubKey, mustPayToTheScript, mustProduceAtLeast,
                                                           mustSatisfyAnyOf, mustSpendAtLeast, mustValidateIn)
import Ledger.Test (asDatum, asRedeemer, someValidatorHash)
import Ledger.Tx qualified as Tx
import Ledger.Tx.Constraints qualified as Tx.Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertFailedTransaction, assertValidatedTransactionCount, checkPredicateOptions,
                             defaultCheckOptions, emulatorConfig, mockWalletPaymentPubKeyHash, w1, w2)
import Plutus.Script.Utils.V1.Generators (alwaysSucceedPolicyVersioned, someTokenValue)
import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Script.Utils.V2.Scripts qualified as PSU.V2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2.Scripts
import Plutus.Trace.Emulator qualified as Trace (EmulatorTrace, activateContractWallet, params, waitNSlots)
import Plutus.V1.Ledger.Api (to)
import Plutus.V1.Ledger.Value
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

-- Constraint's functions should soon be changed to use
-- Address instead of PaymentPubKeyHash and StakeKeyHash
tests :: TestTree
tests =
    testGroup "MustSatisfyAnyOf"
      [ testGroup "ledger constraints" $ [v1Tests, v2Tests] ?? ledgerSubmitTx
      --, testGroup "cardano constraints" $ [v1Tests, v2Tests] ?? cardanoSubmitTx
      ]

v1Tests :: SubmitTx -> TestTree
v1Tests sub = testGroup "Plutus V1" $
   [ v1FeaturesTests
   ] ?? sub ?? languageContextV1

v2Tests :: SubmitTx -> TestTree
v2Tests sub = testGroup "Plutus V2" $
  [ v1FeaturesTests
  ] ?? sub ?? languageContextV2

v1FeaturesTests :: SubmitTx -> LanguageContext -> TestTree
v1FeaturesTests sub t = testGroup "Plutus V1 features" $
    [ mustSatisfyAnyOfUsingAllOfTheSameConstraintsOnAndOffChain,
      mustSatisfyAnyOfUsingSomeOfTheSameConstraintsOnAndOffChain,
      phase2ErrorWhenUsingMustSatisfyAnyOf
    ] ?? sub ?? t

someDatum :: L.Datum
someDatum = asDatum @P.BuiltinByteString "datum"

otherDatum :: L.Datum
otherDatum = asDatum @P.BuiltinByteString "other datum"

utxoValue :: Value
utxoValue = Ada.lovelaceValueOf 10_000_000

adaAmount :: Integer
adaAmount = 5_000_000

adaValue :: Value
adaValue = Ada.lovelaceValueOf adaAmount

tknValue :: LanguageContext -> Value
tknValue tc = singleton (mustSatisfyAnyOfPolicyCurrencySymbol tc) "mint-me" 1

otherTokenValue :: Value
otherTokenValue = someTokenValue "someToken" 1

w1Pkh :: L.PaymentPubKeyHash
w1Pkh = mockWalletPaymentPubKeyHash w1

w2Pkh :: L.PaymentPubKeyHash
w2Pkh = mockWalletPaymentPubKeyHash w2

allConstraintsValid :: ConstraintParams
allConstraintsValid = ConstraintParams
    { mustValidateIn = Just $ MustValidateIn 1000,
      mustBeSignedBy = Just $ MustBeSignedBy w1Pkh,
      mustIncludeDatumInTx = Just MustIncludeDatumInTx,
      mustPayToTheScript = Just $ MustPayToTheScript adaValue,
      mustPayToPubKey = Just $ MustPayToPubKey w2Pkh adaValue,
      mustPayToOtherScript = Just $ MustPayToOtherScript someValidatorHash adaValue,
      mustMintValue = Just $ MustMintValue otherTokenValue,
      mustSpendAtLeast = Just $ MustSpendAtLeast adaValue,
      mustProduceAtLeast = Just $ MustProduceAtLeast adaValue
    }

trace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void $ Trace.waitNSlots 1

mustSatisfyAnyOfContract :: SubmitTx -> LanguageContext -> ConstraintParams
                            -> ConstraintParams -> Contract () Empty ContractError ()
mustSatisfyAnyOfContract
    submitTxFromConstraints lc offChainConstraints onChainConstraints = do
    now <- snd <$> Con.currentNodeClientTimeRange
    let offChainConstraintsWithNow =
            buildConstraints (applyNowToTimeValidity offChainConstraints now)
        onChainConstraintsWithNow  = applyNowToTimeValidity onChainConstraints now
        lookups1 = mintingPolicy lc (mustSatisfyAnyOfPolicy lc)
                <> Cons.mintingPolicy alwaysSucceedPolicyVersioned
        policyRedeemer = asRedeemer onChainConstraintsWithNow
        tx1 = Cons.mustMintValueWithRedeemer policyRedeemer (tknValue lc)
           <> payToScript -- mustSatisfyAnyOfUsingSomeOfTheSameConstraintsOnAndOffChain will fail if this comes after mustSatisfyOf due to bug PLT-1018
           <> Cons.mustSatisfyAnyOf offChainConstraintsWithNow
    ledgerTx1 <- submitTxFromConstraints lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1
        where
            applyNowToTimeValidity :: ConstraintParams -> L.POSIXTime ->  ConstraintParams
            applyNowToTimeValidity cps@ConstraintParams{mustValidateIn = maybeMvi} now =
                cps{mustValidateIn =
                    (\mvi@MustValidateIn{timeTo = offset} ->
                        Just mvi{timeTo = now + offset})
                            =<< maybeMvi}

            payToScript :: Cons.TxConstraints () ()
            payToScript =
                if isJust (mustIncludeDatumInTx offChainConstraints)
                then Cons.mustPayToOtherScriptWithDatumInTx someValidatorHash unitDatum (adaValue <> tknValue lc)
                else Cons.mustPayToOtherScript someValidatorHash unitDatum (adaValue <> tknValue lc)

-- | Valid scenario using offchain and onchain constraint mustSatisfyAnyOf with all of the same
-- | constraints onchain and offchain
mustSatisfyAnyOfUsingAllOfTheSameConstraintsOnAndOffChain :: SubmitTx -> LanguageContext -> TestTree
mustSatisfyAnyOfUsingAllOfTheSameConstraintsOnAndOffChain submitTxFromConstraints lc =
    testGroup "all of the same constraints on and off chain"
    [
        let constraints = allConstraintsValid
            contract = mustSatisfyAnyOfContract submitTxFromConstraints
                       lc constraints constraints
        in checkPredicateOptions defaultCheckOptions
           ("Valid scenario using offchain and onchain constraint " ++
            "mustSatisfyAnyOf with all constraints onchain and offchain")
           (assertValidatedTransactionCount 1)
           (void $ trace contract)

        ,

        let constraints = def
                { mustPayToPubKey = Just $ MustPayToPubKey w2Pkh adaValue,
                  mustSpendAtLeast = Just $ MustSpendAtLeast adaValue }
            contract = mustSatisfyAnyOfContract submitTxFromConstraints
                       lc constraints constraints
        in checkPredicateOptions defaultCheckOptions
           ("Valid scenario using offchain and onchain constraint " ++
            "mustSatisfyAnyOf with mustPayToPubKey and mustSpendAtLeast constraints" ++
            "onchain and offchain")
           (assertValidatedTransactionCount 1)
           (void $ trace contract)

        ,

        let constraints = def
                { mustProduceAtLeast = Just $ MustProduceAtLeast adaValue,
                  mustValidateIn = Just $ MustValidateIn 1000 }
            contract = mustSatisfyAnyOfContract submitTxFromConstraints
                       lc constraints constraints
        in checkPredicateOptions defaultCheckOptions
           ("Valid scenario using offchain and onchain constraint " ++
            "mustSatisfyAnyOf with mustProduceAtLeast and mustValidateIn constraints" ++
            "onchain and offchain")
           (assertValidatedTransactionCount 1)
           (void $ trace contract)

        ,

        let constraints = def { mustMintValue = Just $ MustMintValue otherTokenValue}
            contract = mustSatisfyAnyOfContract submitTxFromConstraints
                       lc constraints constraints
        in checkPredicateOptions defaultCheckOptions
           ("Valid scenario using offchain and onchain constraint " ++
            "mustSatisfyAnyOf with only mustMintValue constraint" ++
            "onchain and offchain")
           (assertValidatedTransactionCount 1)
           (void $ trace contract)
    ]

-- | Valid scenario using offchain and onchain constraint mustSatisfyAnyOf with some of same
-- | constraints onchain and offchain
mustSatisfyAnyOfUsingSomeOfTheSameConstraintsOnAndOffChain :: SubmitTx -> LanguageContext -> TestTree
mustSatisfyAnyOfUsingSomeOfTheSameConstraintsOnAndOffChain submitTxFromConstraints lc =
    testGroup "some of the same constraints on and off chain"
    [
        let offChainConstraints = def { mustIncludeDatumInTx = Just MustIncludeDatumInTx }
            contract = mustSatisfyAnyOfContract submitTxFromConstraints
                       lc offChainConstraints allConstraintsValid
        in checkPredicateOptions defaultCheckOptions
           ("Valid scenario using offchain and onchain constraint " ++
            "mustSatisfyAnyOf where only mustIncludeDatumInTx is satisfied onchain")
           (assertValidatedTransactionCount 1)
           (void $ trace contract)

        ,

        let offChainConstraints = def { mustMintValue = Just $ MustMintValue otherTokenValue }
            contract = mustSatisfyAnyOfContract submitTxFromConstraints
                       lc offChainConstraints allConstraintsValid
        in checkPredicateOptions defaultCheckOptions
           ("Valid scenario using offchain and onchain constraint " ++
            "mustSatisfyAnyOf where only mustMintValue is satisfied onchain")
           (assertValidatedTransactionCount 1)
           (void $ trace contract)

        ,

        let offChainConstraints = def { mustValidateIn = Just $ MustValidateIn 2000,
                                        mustPayToPubKey = Just $ MustPayToPubKey w2Pkh adaValue }
            contract = mustSatisfyAnyOfContract submitTxFromConstraints
                       lc offChainConstraints allConstraintsValid
        in checkPredicateOptions defaultCheckOptions
           ("Valid scenario using offchain and onchain constraint " ++
            "mustSatisfyAnyOf where only mustPayToPubKey is satisfied onchain")
           (assertValidatedTransactionCount 1)
           (void $ trace contract)
    ]

-- | Phase-2 validation failure when  scenario using offchain and onchain constraint
-- | mustSatisfyAnyOf with the same constraints onchain and offchain
phase2ErrorWhenUsingMustSatisfyAnyOf :: SubmitTx -> LanguageContext -> TestTree
phase2ErrorWhenUsingMustSatisfyAnyOf submitTxFromConstraints lc =
    testGroup "phase-2 failures"
    [
        let offChainConstraints = def { mustValidateIn = Just $ MustValidateIn 1001 }
            onChainConstraints  = def { mustValidateIn = Just $ MustValidateIn 1000 }
            contract = mustSatisfyAnyOfContract submitTxFromConstraints
                        lc offChainConstraints onChainConstraints
        in checkPredicateOptions defaultCheckOptions
            ("Phase 2 failure when onchain mustSatisfyAnyOf expects a validity interval " ++
            "with a closer end boundary")
            (assertFailedTransaction (\_ err ->
                case err of {L.ScriptFailure (EvaluationError ("L3":_) _) -> True; _ -> False }))
            (void $ trace contract)
    ,
        let offChainConstraints = def { mustMintValue = Just $ MustMintValue otherTokenValue,
                                        mustPayToTheScript = Just $ MustPayToTheScript adaValue }
            onChainConstraints  = def { mustValidateIn = Just $ MustValidateIn 1000 }
            contract = mustSatisfyAnyOfContract submitTxFromConstraints
                        lc offChainConstraints onChainConstraints
        in checkPredicateOptions defaultCheckOptions
            ("Phase 2 failure when onchain mustSatisfyAnyOf uses mustValidateIn but offchain " ++
            "constraint does not")
            (assertFailedTransaction (\_ err ->
                case err of {L.ScriptFailure (EvaluationError ("L3":_) _) -> True; _ -> False }))
            (void $ trace contract)
    ]

data UnitTest
instance Scripts.ValidatorTypes UnitTest

mkMustSatisfyAnyOfPolicy :: (Cons.TxConstraints () () -> sc -> Bool) -> ConstraintParams -> sc -> Bool
mkMustSatisfyAnyOfPolicy checkScriptContext cps = checkScriptContext (Cons.mustSatisfyAnyOf constraints)
    where
        constraints = buildConstraints cps

mustSatisfyAnyOfPolicyV1 :: L.MintingPolicy
mustSatisfyAnyOfPolicyV1 = L.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        checkedMkMustPayToOtherScriptPolicy = mkMustSatisfyAnyOfPolicy Cons.V1.checkScriptContext
        wrap = Scripts.mkUntypedMintingPolicy checkedMkMustPayToOtherScriptPolicy

mustSatisfyAnyOfPolicyV2 :: L.MintingPolicy
mustSatisfyAnyOfPolicyV2 = L.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        checkedMkMustPayToOtherScriptPolicy = mkMustSatisfyAnyOfPolicy Cons.V2.checkScriptContext
        wrap = V2.Scripts.mkUntypedMintingPolicy checkedMkMustPayToOtherScriptPolicy

data LanguageContext
   = LanguageContext
   { mustSatisfyAnyOfPolicy :: L.MintingPolicy
   , mintingPolicy          :: forall a. L.MintingPolicy -> Cons.ScriptLookups a
   , mintingPolicyHash      :: L.MintingPolicy -> L.MintingPolicyHash
   }

languageContextV1 :: LanguageContext
languageContextV1 = LanguageContext
    mustSatisfyAnyOfPolicyV1
    Cons.plutusV1MintingPolicy
    PSU.V1.mintingPolicyHash

languageContextV2 :: LanguageContext
languageContextV2 = LanguageContext
    mustSatisfyAnyOfPolicyV2
    Cons.plutusV2MintingPolicy
    PSU.V2.mintingPolicyHash

type SubmitTx
  =  Cons.ScriptLookups UnitTest
  -> Cons.TxConstraints (Scripts.RedeemerType UnitTest) (Scripts.DatumType UnitTest)
  -> Contract () Empty ContractError Tx.CardanoTx

cardanoSubmitTx :: SubmitTx
cardanoSubmitTx lookups tx = let
  p = defaultCheckOptions ^. emulatorConfig . Trace.params
  in submitUnbalancedTx $ either (error . show) id $ Tx.Constraints.mkTx @UnitTest p lookups tx

ledgerSubmitTx :: SubmitTx
ledgerSubmitTx = submitTxConstraintsWith

mustSatisfyAnyOfPolicyHash :: LanguageContext -> L.MintingPolicyHash
mustSatisfyAnyOfPolicyHash lc = mintingPolicyHash lc $ mustSatisfyAnyOfPolicy lc

mustSatisfyAnyOfPolicyCurrencySymbol :: LanguageContext -> L.CurrencySymbol
mustSatisfyAnyOfPolicyCurrencySymbol = mpsSymbol . mustSatisfyAnyOfPolicyHash

{-# INLINABLE buildConstraints #-}
buildConstraints :: ConstraintParams -> [Cons.TxConstraints () ()]
buildConstraints cps = do
    let maybeConstraints :: [Maybe (Cons.TxConstraints () ())] = [
            P.maybe Nothing (\cp ->
                Just $ Cons.mustValidateIn $ to (timeTo cp)) (mustValidateIn cps),
            P.maybe Nothing (\cp ->
                Just $ Cons.mustBeSignedBy $ ppkh cp) (mustBeSignedBy cps),
            P.maybe Nothing (\_ ->
                Just $ Cons.mustIncludeDatumInTx unitDatum) (mustIncludeDatumInTx cps),
            P.maybe Nothing (\cp ->
                Just $ Cons.mustPayToTheScript () (value cp)) (mustPayToTheScript cps),
            P.maybe Nothing (\cp ->
                Just $ Cons.mustPayToPubKey (ppkh cp) (value cp)) (mustPayToPubKey cps),
            P.maybe Nothing (\cp ->
                Just $
                Cons.mustPayToOtherScript (vh cp) unitDatum (value cp)) (mustPayToOtherScript cps),
            P.maybe Nothing (\cp ->
                Just $ Cons.mustMintValue (value cp)) (mustMintValue cps),
            P.maybe Nothing (\cp ->
                Just $ Cons.mustSpendAtLeast (value cp)) (mustSpendAtLeast cps),
            P.maybe Nothing (\cp ->
                Just $ Cons.mustProduceAtLeast (value cp)) (mustProduceAtLeast cps)
            ]
    catMaybes maybeConstraints
    where
        catMaybes :: [Maybe (Cons.TxConstraints () ())] -> [Cons.TxConstraints () ()]
        catMaybes ls = [x | Just x <- ls]

data ConstraintParam = MustValidateIn        {timeTo :: L.POSIXTime}
                     | MustBeSignedBy        {ppkh :: L.PaymentPubKeyHash}
                     | MustIncludeDatumInTx
                     | MustPayToTheScript    {value :: Value}
                     | MustPayToPubKey       {ppkh  :: L.PaymentPubKeyHash,
                                              value :: Value}
                     | MustPayToOtherScript  {vh    :: L.ValidatorHash,
                                              value :: Value}
                     | MustMintValue         {value :: Value}
                     | MustSpendAtLeast      {value :: Value}
                     | MustProduceAtLeast    {value :: Value}

data ConstraintParams
    = ConstraintParams
    {
    mustValidateIn,
    mustBeSignedBy,
    mustIncludeDatumInTx,
    mustPayToTheScript,
    mustPayToPubKey,
    mustPayToOtherScript,
    mustMintValue,
    mustSpendAtLeast,
    mustProduceAtLeast :: Maybe ConstraintParam
    } deriving (Default, Generic)

PlutusTx.unstableMakeIsData ''ConstraintParam
PlutusTx.unstableMakeIsData ''ConstraintParams
