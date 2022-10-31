{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module Spec.Contract.Tx.Constraints.MustPayToPubKeyAddress(tests) where

import Control.Lens (_1, _head, has, makeClassyPrisms, only, (??), (^.))
import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Data.Text qualified as Text
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OnChain.V1 qualified as Constraints
import Ledger.Constraints.OnChain.V2 qualified as V2.Constraints
import Ledger.Test (asDatum, asRedeemer)
import Ledger.Tx qualified as Tx
import Ledger.Tx.Constraints qualified as Tx.Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertFailedTransaction, assertValidatedTransactionCount, checkPredicate,
                             defaultCheckOptions, emulatorConfig, mockWalletPaymentPubKeyHash, w1, w2)
import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Script.Utils.V2.Scripts qualified as PSU.V2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2.Scripts
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

makeClassyPrisms ''Ledger.ScriptError

-- Constraint's functions should soon be changed to use Address instead of PaymentPubKeyHash and StakeKeyHash
tests :: TestTree
tests = testGroup "MustPayToPubKeyAddress"
      [ testGroup "ledger constraints" $ [v1Tests, v2Tests] ?? ledgerSubmitTx
      --, testGroup "cardano constraints" $ [v1Tests, v2Tests] ?? cardanoSubmitTx
      ]

v1Tests :: SubmitTx -> TestTree
v1Tests sub = testGroup "Plutus V1" $
   [ v1FeaturesTests
   , v2FeaturesNotAvailableTests
   ] ?? sub ?? languageContextV1

v2Tests :: SubmitTx -> TestTree
v2Tests sub = testGroup "Plutus V2" $
  [ v1FeaturesTests
  , v2FeaturesTests
  ] ?? sub ?? languageContextV2

v1FeaturesTests :: SubmitTx -> LanguageContext -> TestTree
v1FeaturesTests sub t = testGroup "Plutus V1 features" $
    [ successfulUseOfMustPayToPubKeyWithMintedTokenValue
    , successfulUseOfMustPayToPubKeyWhenOffchainIncludesTokenAndOnchainChecksOnlyToken
    , successfulUseOfMustPayToPubKeyWhenOffchainIncludesTokenAndOnchainChecksOnlyAda
    , successfulUseOfMustPayToPubKeyExpectingALowerAdaValue
    , successfulUseOfMustPayToPubKeyAddress
    , successfulUseOfMustPayWithDatumInTxToPubKey
    , successfulUseOfMustPayWithDatumInTxToPubKeyAddress
    , phase2FailureWhenUsingUnexpectedPaymentPubKeyHash
    --, phase2FailureWhenUsingUnexpectedStakePubKeyHash -- onchain check not implemented
    , phase2FailureWhenUsingUnexpectedDatum
    , phase2FailureWhenUsingUnexpectedValue
    ] ?? sub ?? t

v2FeaturesTests :: SubmitTx -> LanguageContext -> TestTree
v2FeaturesTests sub t = testGroup "Plutus V2 features" $
    [ successfulUseOfMustPayWithInlineDatumToPubKeyV2
    ] ?? sub ?? t

v2FeaturesNotAvailableTests :: SubmitTx -> LanguageContext -> TestTree
v2FeaturesNotAvailableTests sub t = testGroup "Plutus V2 features" $
    [ phase1FailureWhenUsingInlineDatumWithV1
    ] ?? sub ?? t

evaluationError :: Text.Text -> Ledger.ValidationError -> Bool
evaluationError errCode = has $ Ledger._ScriptFailure . _EvaluationError . _1 . _head . only errCode

someDatum :: Ledger.Datum
someDatum = asDatum @P.BuiltinByteString "datum"

otherDatum :: Ledger.Datum
otherDatum = asDatum @P.BuiltinByteString "other datum"

adaAmount :: Integer
adaAmount = 5_000_000

adaValue :: Value.Value
adaValue = Ada.lovelaceValueOf adaAmount

tknValue :: LanguageContext -> Value.Value
tknValue tc = Value.singleton (mustPayToPubKeyAddressPolicyCurrencySymbol tc) "mint-me" 1

w1PaymentPubKeyHash :: Ledger.PaymentPubKeyHash
w1PaymentPubKeyHash = mockWalletPaymentPubKeyHash w1

w2PaymentPubKeyHash :: Ledger.PaymentPubKeyHash
w2PaymentPubKeyHash = mockWalletPaymentPubKeyHash w2

w1StakePubKeyHash :: Ledger.StakePubKeyHash
w1StakePubKeyHash = Ledger.StakePubKeyHash $ Ledger.unPaymentPubKeyHash w1PaymentPubKeyHash -- fromJust $ stakePubKeyHash $ walletToMockWallet' w1 -- is Nothing

w2StakePubKeyHash :: Ledger.StakePubKeyHash
w2StakePubKeyHash = Ledger.StakePubKeyHash $ Ledger.unPaymentPubKeyHash w2PaymentPubKeyHash -- fromJust $ stakePubKeyHash $ walletToMockWallet' w2 -- is Nothing

trace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void $ Trace.waitNSlots 1

-- | Valid scenario using offchain and onchain constraint mustPayToPubKey with exact token value being minted
successfulUseOfMustPayToPubKeyWithMintedTokenValue :: SubmitTx -> LanguageContext -> TestTree
successfulUseOfMustPayToPubKeyWithMintedTokenValue submitTxFromConstraints tc =
    let adaAndTokenValue = adaValue <> tknValue tc
        onChainConstraint = asRedeemer $ MustPayToPubKey w2PaymentPubKeyHash adaAndTokenValue
        contract = do
            let lookups1 = mintingPolicy tc (mustPayToPubKeyAddressPolicy tc)
                tx1 = Constraints.mustPayToPubKey w2PaymentPubKeyHash adaAndTokenValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint (tknValue tc)
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Successful use of offchain and onchain mustPayToPubKey constraint for native token value"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using mustPayToPubKey offchain constraint to include ada and token whilst onchain constraint checks for token value only
successfulUseOfMustPayToPubKeyWhenOffchainIncludesTokenAndOnchainChecksOnlyToken :: SubmitTx -> LanguageContext -> TestTree
successfulUseOfMustPayToPubKeyWhenOffchainIncludesTokenAndOnchainChecksOnlyToken submitTxFromConstraints tc =
    let adaAndTokenValue = adaValue <> tknValue tc
        onChainConstraint = asRedeemer $ MustPayToPubKey w2PaymentPubKeyHash (tknValue tc)
        contract = do
            let lookups1 = mintingPolicy tc (mustPayToPubKeyAddressPolicy tc)
                tx1 = Constraints.mustPayToPubKey w2PaymentPubKeyHash adaAndTokenValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint (tknValue tc)
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Successful use of mustPayToPubKey offchain constraint to include ada and token whilst onchain constraint checks for token value only"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using mustPayToPubKey offchain constraint to include ada and token whilst onchain constraint checks for ada value only
successfulUseOfMustPayToPubKeyWhenOffchainIncludesTokenAndOnchainChecksOnlyAda :: SubmitTx -> LanguageContext -> TestTree
successfulUseOfMustPayToPubKeyWhenOffchainIncludesTokenAndOnchainChecksOnlyAda submitTxFromConstraints tc =
    let adaAndTokenValue = adaValue <> tknValue tc
        onChainConstraint = asRedeemer $ MustPayToPubKey w2PaymentPubKeyHash adaValue
        contract = do
            let lookups1 = mintingPolicy tc $ mustPayToPubKeyAddressPolicy tc
                tx1 = Constraints.mustPayToPubKey w2PaymentPubKeyHash adaAndTokenValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint (tknValue tc)
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Successful use of mustPayToPubKey offchain constraint to include ada and token whilst onchain constraint checks for ada value only"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario where the onchain mustPayToPubKey constraint expects less ada than the actual value
successfulUseOfMustPayToPubKeyExpectingALowerAdaValue :: SubmitTx -> LanguageContext -> TestTree
successfulUseOfMustPayToPubKeyExpectingALowerAdaValue submitTxFromConstraints tc =
    let onChainConstraint = asRedeemer $ MustPayToPubKey w2PaymentPubKeyHash (Ada.lovelaceValueOf $ adaAmount - 1)
        contract = do
            let lookups1 = mintingPolicy tc $ mustPayToPubKeyAddressPolicy tc
                tx1 = Constraints.mustPayToPubKey w2PaymentPubKeyHash adaValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint (tknValue tc)
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Successful use of onchain mustPayToPubKey constraint when it expects less ada than the actual value"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using offchain and onchain constraint mustPayToPubKeyAddress with ada-only value
successfulUseOfMustPayToPubKeyAddress :: SubmitTx -> LanguageContext -> TestTree
successfulUseOfMustPayToPubKeyAddress submitTxFromConstraints tc =
    let onChainConstraint = asRedeemer $ MustPayToPubKeyAddress w2PaymentPubKeyHash w2StakePubKeyHash adaValue
        contract = do
            let lookups1 = mintingPolicy tc $ mustPayToPubKeyAddressPolicy tc
                tx1 = Constraints.mustPayToPubKeyAddress w2PaymentPubKeyHash w2StakePubKeyHash adaValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint (tknValue tc)
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Successful use of offchain and onchain mustPayToPubKeyAddress constraint for ada-only value"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using offchain and onchain constraint mustPayWithDatumInTxToPubKey with bytestring datum and ada value
successfulUseOfMustPayWithDatumInTxToPubKey :: SubmitTx -> LanguageContext -> TestTree
successfulUseOfMustPayWithDatumInTxToPubKey submitTxFromConstraints tc =
    let onChainConstraint = asRedeemer $ MustPayWithDatumInTxToPubKey w2PaymentPubKeyHash someDatum adaValue
        contract = do
            let lookups1 = mintingPolicy tc $ mustPayToPubKeyAddressPolicy tc
                tx1 = Constraints.mustPayWithDatumInTxToPubKey w2PaymentPubKeyHash someDatum adaValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint (tknValue tc)
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Successful use of offchain and onchain mustPayWithDatumInTxToPubKey constraint with bytestring datum and ada value"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using offchain and onchain constraint mustPayWithDatumInTxToPubKeyAddress with bytestring datum and ada value
successfulUseOfMustPayWithDatumInTxToPubKeyAddress :: SubmitTx -> LanguageContext -> TestTree
successfulUseOfMustPayWithDatumInTxToPubKeyAddress submitTxFromConstraints tc =
    let onChainConstraint = asRedeemer $ MustPayWithDatumInTxToPubKeyAddress w2PaymentPubKeyHash w2StakePubKeyHash someDatum adaValue
        contract = do
            let lookups1 = mintingPolicy tc $ mustPayToPubKeyAddressPolicy tc
                tx1 = Constraints.mustPayWithDatumInTxToPubKeyAddress
                        w2PaymentPubKeyHash
                        w2StakePubKeyHash
                        someDatum
                        adaValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint (tknValue tc)
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Successful use of offchain and onchain mustPayWithDatumInTxToPubKeyAddress constraint with bytestring datum and ada value"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Phase-2 failure when onchain mustPayWithDatumInTxToPubKeyAddress constraint cannot verify the PaymentPubkeyHash"
phase2FailureWhenUsingUnexpectedPaymentPubKeyHash :: SubmitTx -> LanguageContext -> TestTree
phase2FailureWhenUsingUnexpectedPaymentPubKeyHash submitTxFromConstraints tc =
    let onChainConstraint = asRedeemer $ MustPayWithDatumInTxToPubKeyAddress w2PaymentPubKeyHash w2StakePubKeyHash someDatum adaValue
        contract = do
            let lookups1 = mintingPolicy tc $ mustPayToPubKeyAddressPolicy tc
                tx1 = Constraints.mustPayWithDatumInTxToPubKeyAddress
                        w1PaymentPubKeyHash
                        w2StakePubKeyHash
                        someDatum
                        adaValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint (tknValue tc)
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Phase-2 validation failure occurs when onchain mustPayWithDatumInTxToPubKeyAddress constraint sees an unexpected PaymentPubkeyHash"
    (assertFailedTransaction $ const $ evaluationError "La")
    (void $ trace contract)

-- | Phase-2 failure when onchain mustPayWithDatumInTxToPubKeyAddress constraint cannot verify the Datum"
phase2FailureWhenUsingUnexpectedDatum :: SubmitTx -> LanguageContext -> TestTree
phase2FailureWhenUsingUnexpectedDatum submitTxFromConstraints tc =
    let onChainConstraint = asRedeemer $ MustPayWithDatumInTxToPubKeyAddress w2PaymentPubKeyHash w2StakePubKeyHash otherDatum adaValue
        contract = do
            let lookups1 = mintingPolicy tc $ mustPayToPubKeyAddressPolicy tc
                tx1 = Constraints.mustPayWithDatumInTxToPubKeyAddress w2PaymentPubKeyHash w2StakePubKeyHash someDatum adaValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint (tknValue tc)
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Phase-2 validation failure occurs when onchain mustPayWithDatumInTxToPubKeyAddress constraint sees an unexpected Datum"
    (assertFailedTransaction $ const $ evaluationError "La")
    (void $ trace contract)

-- | Phase-2 failure when onchain mustPayWithDatumInTxToPubKeyAddress constraint cannot verify the Value"
phase2FailureWhenUsingUnexpectedValue :: SubmitTx -> LanguageContext -> TestTree
phase2FailureWhenUsingUnexpectedValue submitTxFromConstraints tc =
    let onChainConstraint = asRedeemer $ MustPayWithDatumInTxToPubKeyAddress w2PaymentPubKeyHash w2StakePubKeyHash someDatum (Ada.lovelaceValueOf $ adaAmount + 1)
        contract = do
            let lookups1 = mintingPolicy tc $ mustPayToPubKeyAddressPolicy tc
                tx1 = Constraints.mustPayWithDatumInTxToPubKeyAddress w2PaymentPubKeyHash w2StakePubKeyHash someDatum adaValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint (tknValue tc)
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Phase-2 validation failure occurs when onchain mustPayWithDatumInTxToPubKeyAddress constraint sees an unexpected Value"
    (assertFailedTransaction $ const $ evaluationError "La")
    (void $ trace contract)



-- | Valid scenario using offchain and onchain constraint mustPayWithDatumInTxToPubKey with inline bytestring datum and ada value
successfulUseOfMustPayWithInlineDatumToPubKeyV2 :: SubmitTx -> LanguageContext -> TestTree
successfulUseOfMustPayWithInlineDatumToPubKeyV2 submitTxFromConstraints tc =
    let onChainConstraint = asRedeemer $ MustPayWithInlineDatumToPubKey w2PaymentPubKeyHash someDatum adaValue
        contract = do
            let lookups1 = mintingPolicy tc $ mustPayToPubKeyAddressPolicy tc
                tx1 = Constraints.mustPayWithInlineDatumToPubKey w2PaymentPubKeyHash someDatum adaValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint (tknValue tc)
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Successful use of offchain and onchain mustPayWithDatumInTxToPubKey constraint with inline bytestring datum and ada value"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Phase-1 failure when mustPayToPubKeyAddress in a V1 script use inline datum
phase1FailureWhenUsingInlineDatumWithV1 :: SubmitTx -> LanguageContext -> TestTree
phase1FailureWhenUsingInlineDatumWithV1 submitTxFromConstraints tc =
    let onChainConstraint = asRedeemer $ MustPayWithInlineDatumToPubKey w2PaymentPubKeyHash someDatum adaValue
        contract = do
            let lookups1 = mintingPolicy tc $ mustPayToPubKeyAddressPolicy tc
                tx1 = Constraints.mustPayWithInlineDatumToPubKey w2PaymentPubKeyHash someDatum adaValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint (tknValue tc)
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Phase-1 failure when mustPayToPubKeyAddress in a V1 script use inline datum"
    (assertFailedTransaction (const $ has Ledger._CardanoLedgerValidationError))
    (void $ trace contract)


data UnitTest
instance Scripts.ValidatorTypes UnitTest

data LanguageContext
   = LanguageContext
   { mustPayToPubKeyAddressPolicy :: Ledger.MintingPolicy
   , mintingPolicy                :: forall a. Ledger.MintingPolicy -> Constraints.ScriptLookups a
   , mintingPolicyHash            :: Ledger.MintingPolicy -> Ledger.MintingPolicyHash
   }

mustPayToPubKeyAddressPolicyV1 :: Ledger.MintingPolicy
mustPayToPubKeyAddressPolicyV1 = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        checkedMkMustPayToPubKeyAddressPolicy = mkMustPayToPubKeyAddressPolicy Constraints.checkScriptContext
        wrap = Scripts.mkUntypedMintingPolicy checkedMkMustPayToPubKeyAddressPolicy

mustPayToPubKeyAddressPolicyV2 :: Ledger.MintingPolicy
mustPayToPubKeyAddressPolicyV2 = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        checkedMkMustPayToPubKeyAddressPolicy = mkMustPayToPubKeyAddressPolicy V2.Constraints.checkScriptContext
        wrap = V2.Scripts.mkUntypedMintingPolicy checkedMkMustPayToPubKeyAddressPolicy

languageContextV1 :: LanguageContext
languageContextV1 = LanguageContext
    mustPayToPubKeyAddressPolicyV1
    Constraints.plutusV1MintingPolicy
    PSU.V1.mintingPolicyHash


languageContextV2 :: LanguageContext
languageContextV2 = LanguageContext
    mustPayToPubKeyAddressPolicyV2
    Constraints.plutusV2MintingPolicy
    PSU.V2.mintingPolicyHash


mkMustPayToPubKeyAddressPolicy :: (Constraints.TxConstraints () () -> sc -> Bool) -> ConstraintParams -> sc -> Bool
mkMustPayToPubKeyAddressPolicy checkScriptContext = \case
    MustPayToPubKey ppkh v ->
        checkScriptContext (Constraints.mustPayToPubKey ppkh v)
    MustPayToPubKeyAddress ppkh spkh v ->
        checkScriptContext (Constraints.mustPayToPubKeyAddress ppkh spkh v)
    MustPayWithDatumInTxToPubKey ppkh d v ->
        checkScriptContext (Constraints.mustPayWithDatumInTxToPubKey ppkh d v)
    MustPayWithDatumInTxToPubKeyAddress ppkh spkh d v ->
        checkScriptContext (Constraints.mustPayWithDatumInTxToPubKeyAddress ppkh spkh d v)
    MustPayWithInlineDatumToPubKey ppkh d v ->
        checkScriptContext (Constraints.mustPayWithInlineDatumToPubKey ppkh d v)
    MustPayWithInlineDatumToPubKeyAddress ppkh spkh d v ->
        checkScriptContext (Constraints.mustPayWithInlineDatumToPubKeyAddress ppkh spkh d v)

mustPayToPubKeyAddressPolicyHash :: LanguageContext -> Ledger.MintingPolicyHash
mustPayToPubKeyAddressPolicyHash tc = mintingPolicyHash tc $ mustPayToPubKeyAddressPolicy tc

mustPayToPubKeyAddressPolicyCurrencySymbol :: LanguageContext -> Ledger.CurrencySymbol
mustPayToPubKeyAddressPolicyCurrencySymbol = Value.mpsSymbol . mustPayToPubKeyAddressPolicyHash

type SubmitTx
  =  Constraints.ScriptLookups UnitTest
  -> Constraints.TxConstraints (Scripts.RedeemerType UnitTest) (Scripts.DatumType UnitTest)
  -> Contract () Empty ContractError Tx.CardanoTx

cardanoSubmitTx :: SubmitTx
cardanoSubmitTx lookups tx = let
  p = defaultCheckOptions ^. emulatorConfig . Trace.params
  in submitUnbalancedTx $ either (error . show) id $ Tx.Constraints.mkTx @UnitTest p lookups tx

ledgerSubmitTx :: SubmitTx
ledgerSubmitTx = submitTxConstraintsWith

data ConstraintParams = MustPayToPubKey Ledger.PaymentPubKeyHash Value.Value
                      | MustPayToPubKeyAddress Ledger.PaymentPubKeyHash Ledger.StakePubKeyHash Value.Value
                      | MustPayWithDatumInTxToPubKey Ledger.PaymentPubKeyHash Ledger.Datum Value.Value
                      | MustPayWithDatumInTxToPubKeyAddress Ledger.PaymentPubKeyHash Ledger.StakePubKeyHash Ledger.Datum Value.Value
                      | MustPayWithInlineDatumToPubKey Ledger.PaymentPubKeyHash Ledger.Datum Value.Value
                      | MustPayWithInlineDatumToPubKeyAddress Ledger.PaymentPubKeyHash Ledger.StakePubKeyHash Ledger.Datum Value.Value
    deriving (Show)

PlutusTx.unstableMakeIsData ''ConstraintParams
