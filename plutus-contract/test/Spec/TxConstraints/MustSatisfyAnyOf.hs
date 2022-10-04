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
module Spec.TxConstraints.MustSatisfyAnyOf(tests) where

import Control.Lens ((??), (^.))
import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Data.Default (Default (def))
import GHC.Generics (Generic)
import Ledger qualified as L
import Ledger.Ada qualified as Ada
import Ledger.Constraints.OffChain qualified as Cons (ScriptLookups, plutusV1MintingPolicy, plutusV2MintingPolicy)
import Ledger.Constraints.OnChain.V1 qualified as Cons.V1
import Ledger.Constraints.OnChain.V2 qualified as Cons.V2
import Ledger.Constraints.TxConstraints qualified as Cons (TxConstraints, mustBeSignedBy, mustIncludeDatum,
                                                           mustMintValueWithRedeemer, mustSatisfyAnyOf, mustValidateIn)
import Ledger.Test (asDatum, asRedeemer)
import Ledger.Tx qualified as Tx
import Ledger.Tx.Constraints qualified as Tx.Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertValidatedTransactionCount, checkPredicateOptions, defaultCheckOptions,
                             emulatorConfig, mockWalletPaymentPubKeyHash, w1)
import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Script.Utils.V2.Scripts qualified as PSU.V2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2.Scripts
import Plutus.Trace.Emulator qualified as Trace (EmulatorTrace, activateContractWallet, params, waitNSlots)
import Plutus.V1.Ledger.Api (to)
import Plutus.V1.Ledger.Value
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

-- Constraint's functions should soon be changed to use Address instead of PaymentPubKeyHash and StakeKeyHash
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
    [ mustSatisfyAnyOfMatchingAllConstraints
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

-- adaAndTokenValue :: LanguageContext -> Value.Value
-- adaAndTokenValue = (adaValue <>) . tknValue

--otherTokenValue :: Value.Value
--otherTokenValue = someTokenValue "someToken" 1

trace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void $ Trace.waitNSlots 1

mustSatisfyAnyOfSignerOrTimeContract :: SubmitTx -> LanguageContext -> ConstraintParams -> ConstraintParams -> Contract () Empty ContractError ()
mustSatisfyAnyOfSignerOrTimeContract submitTxFromConstraints lc offChainConstraints onChainConstraints = do
    now <- Con.currentTime
    let offChainConstraintsWithNow = buildConstraints (applyNowToTimeValidity offChainConstraints now)
        onChainConstraintsWithNow  = applyNowToTimeValidity onChainConstraints now
        lookups1 = mintingPolicy lc $ mustSatisfyAnyOfPolicy lc
        policyRedeemer = asRedeemer onChainConstraintsWithNow
    let tx1 = Cons.mustMintValueWithRedeemer policyRedeemer (tknValue lc)
           <> Cons.mustSatisfyAnyOf offChainConstraintsWithNow
    ledgerTx1 <- submitTxFromConstraints lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1
        where
            applyNowToTimeValidity :: ConstraintParams -> L.POSIXTime ->  ConstraintParams
            applyNowToTimeValidity cps@ConstraintParams{mustValidateIn = maybeMvi} now =
                cps{mustValidateIn =
                    (\mvi@MustValidateIn{timeTo = offset} ->
                        Just mvi{timeTo = now + offset}) =<< maybeMvi}

-- | Valid scenario using offchain and onchain constraint mustSatisfyAnyOf with the same constraints
-- | onchain and offchain
mustSatisfyAnyOfMatchingAllConstraints :: SubmitTx -> LanguageContext -> TestTree
mustSatisfyAnyOfMatchingAllConstraints submitTxFromConstraints lc =
    let pkh = mockWalletPaymentPubKeyHash w1
        cps = def {mustValidateIn = Just $ MustValidateIn 1000,
                   mustBeSignedBy = Just $ MustBeSignedBy pkh}
        contract = mustSatisfyAnyOfSignerOrTimeContract submitTxFromConstraints lc cps cps
    in checkPredicateOptions defaultCheckOptions
    "Valid scenario using offchain and onchain constraint mustSatisfyAnyOf with the same constraints onchain and offchain"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

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
            P.maybe Nothing (\cp -> Just $ Cons.mustValidateIn $ to (timeTo cp)) (mustValidateIn cps),
            P.maybe Nothing (\cp -> Just $ Cons.mustBeSignedBy $ ppkh cp) (mustBeSignedBy cps),
            P.maybe Nothing (\cp -> Just $ Cons.mustIncludeDatum $ datum cp) (mustIncludeDatum cps)
            ]
    catMaybes maybeConstraints
    where
        catMaybes :: [Maybe (Cons.TxConstraints () ())] -> [Cons.TxConstraints () ()]
        catMaybes ls = [x | Just x <- ls]

data ConstraintParam = MustValidateIn        {timeTo :: L.POSIXTime}
                     | MustBeSignedBy        {ppkh :: L.PaymentPubKeyHash}
                     | MustIncludeDatum      {datum :: L.Datum}
                     | MustPayToTheScript    {value :: Value}
                     | MustPayToPubKey       {ppkh  :: L.PaymentPubKeyHash,
                                              value :: Value}
                     | MustPayToOtherScript  {vh    :: L.ValidatorHash,
                                              datum :: L.Datum,
                                              value :: Value}
                     | MustMintValue         {value :: Value}
                     | MustSpendAtLeast      {value :: Value}
                     | MustProduceAtLeast    {value :: Value}
                     | MustSpendPubKeyOutput {txOutRef :: L.TxOutRef}

data ConstraintParams
    = ConstraintParams
    {
    mustValidateIn,
    mustBeSignedBy,
    mustIncludeDatum,
    mustPayToTheScript,
    mustPayToPubKey,
    mustPayToOtherScript,
    mustMintValue,
    mustSpendAtLeast,
    mustProduceAtLeast,
    mustSpendPubKeyOutput :: Maybe ConstraintParam
    } deriving (Default, Generic)

PlutusTx.unstableMakeIsData ''ConstraintParam
PlutusTx.unstableMakeIsData ''ConstraintParams
