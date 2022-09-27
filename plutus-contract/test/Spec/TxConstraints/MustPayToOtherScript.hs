{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.TxConstraints.MustPayToOtherScript(tests) where

import Control.Lens ((&), (??), (^.))
import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints (ScriptLookups, TxConstraints, mustMintValueWithRedeemer,
                                                    mustPayToOtherScript, mustPayToOtherScriptAddress,
                                                    mustPayToOtherScriptAddressInlineDatum,
                                                    mustPayToOtherScriptInlineDatum,
                                                    mustSpendScriptOutputWithMatchingDatumAndValue,
                                                    plutusV1MintingPolicy, plutusV1OtherScript, plutusV2MintingPolicy,
                                                    unspentOutputs)
import Ledger.Constraints.OnChain.V1 qualified as Constraints (checkScriptContext)
import Ledger.Constraints.OnChain.V2 qualified as V2.Constraints
import Ledger.Generators (someTokenValue)
import Ledger.Scripts (Redeemer, ScriptError (EvaluationError))
import Ledger.Test (asDatum, asRedeemer, someValidator, someValidatorHash)
import Ledger.Tx qualified as Tx
import Ledger.Tx.Constraints qualified as Tx.Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertContractError, assertFailedTransaction, assertValidatedTransactionCount,
                             changeInitialWalletValue, checkPredicateOptions, defaultCheckOptions, emulatorConfig, w1)
import Plutus.Script.Utils.V1.Generators (alwaysSucceedValidatorHash)
import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Script.Utils.V2.Scripts qualified as PSU.V2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2.Scripts
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Wallet (WalletAPIError (InsufficientFunds))

-- Constraint's functions should soon be changed to use Address instead of PaymentPubKeyHash and StakeKeyHash
tests :: TestTree
tests =
    testGroup "MustPayToOtherScript"
      [ testGroup "ledger constraints"
        [ v1Tests ledgerSubmitTx
        , v2Tests ledgerSubmitTx
        ]
      , testGroup "cardano constraints"
        [ v1Tests ledgerSubmitTx
        , v2Tests ledgerSubmitTx
        ]
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
    [ successfulUseOfMustPayToOtherScriptWithMintedToken
    , successfulUseOfMustPayToOtherScriptWhenOffchainIncludesTokenAndOnchainChecksOnlyToken
    --, successfulUseOfMustPayToOtherScriptWhenOffchainIncludesTokenAndOnchainChecksOnlyAda -- FAILING when onchain checks for only ada value and token is present -- PLT-885
    , successfulUseOfMustPayToOtherScriptWithScriptsExactTokenBalance
    , successfulUseOfMustPayToOtherScriptWhenOnchainExpectsLowerAdaValue
    , contractErrorWhenAttemptingToSpendMoreThanAdaBalance
    , contractErrorWhenAttemptingToSpendMoreThanTokenBalance
    , phase2ErrorWhenExpectingMoreThanValue
    ] ?? sub ?? t

v2FeaturesTests :: SubmitTx -> LanguageContext -> TestTree
v2FeaturesTests sub t = testGroup "Plutus V2 features" $
    [ successfulUseOfMustPayToOtherScriptWithMintedTokenV2
    ] ?? sub ?? t

v2FeaturesNotAvailableTests :: SubmitTx -> LanguageContext -> TestTree
v2FeaturesNotAvailableTests sub t = testGroup "Plutus V2 features" $
    [ phase1FailureWhenPayToOtherScriptV1ScriptUseInlineDatum
    ] ?? sub ?? t

someDatum :: Ledger.Datum
someDatum = asDatum @P.BuiltinByteString "datum"

otherDatum :: Ledger.Datum
otherDatum = asDatum @P.BuiltinByteString "other datum"

utxoValue :: Value.Value
utxoValue = Ada.lovelaceValueOf 10_000_000

adaAmount :: Integer
adaAmount = 5_000_000

adaValue :: Value.Value
adaValue = Ada.lovelaceValueOf adaAmount

tknValue :: LanguageContext -> Value.Value
tknValue tc = Value.singleton (mustPayToOtherScriptPolicyCurrencySymbol tc) "mint-me" 1

adaAndTokenValue :: LanguageContext -> Value.Value
adaAndTokenValue = (adaValue <>) . tknValue

otherTokenValue :: Value.Value
otherTokenValue = someTokenValue "someToken" 1

trace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void $ Trace.waitNSlots 1

-- | Contract to a single transaction with mustSpendScriptOutputs offchain constraint and mint with policy using matching onchain constraint
mustPayToOtherScriptContract :: SubmitTx -> LanguageContext -> Value.Value -> Ledger.Redeemer -> Contract () Empty ContractError ()
mustPayToOtherScriptContract submitTxFromConstraints lc offChainValue onChainConstraint = do
    let lookups1 = mintingPolicy lc $ mustPayToOtherScriptPolicy lc
        tx1 = Constraints.mustPayToOtherScript someValidatorHash someDatum offChainValue
           <> Constraints.mustMintValueWithRedeemer onChainConstraint (tknValue lc)
    ledgerTx1 <- submitTxFromConstraints lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Valid scenario using offchain and onchain constraint mustPayToOtherScript with exact token value being minted
successfulUseOfMustPayToOtherScriptWithMintedToken :: SubmitTx -> LanguageContext -> TestTree
successfulUseOfMustPayToOtherScriptWithMintedToken submitTxFromConstraints lc =
    let onChainConstraint = asRedeemer $ MustPayToOtherScript someValidatorHash someDatum (adaAndTokenValue lc)
        contract = mustPayToOtherScriptContract submitTxFromConstraints lc (adaAndTokenValue lc) onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Successful use of offchain and onchain mustPayToOtherScript constraint with wallet's exact ada balance"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Contract to a single transaction with mustSpendScriptOutputs offchain constraint and mint with policy using
-- matching onchain constraint, using Plutus V2 script and inline datum
mustPayToOtherScriptInlineContractV2 :: SubmitTx -> LanguageContext -> Value.Value -> Redeemer -> Contract () Empty ContractError ()
mustPayToOtherScriptInlineContractV2 submitTxFromConstraints lc offChainValue onChainConstraint = do
    let lookups1 = mintingPolicy lc $ mustPayToOtherScriptPolicy lc
        tx1 = Constraints.mustPayToOtherScriptInlineDatum someValidatorHash someDatum offChainValue
           <> Constraints.mustMintValueWithRedeemer onChainConstraint (tknValue lc)
    ledgerTx1 <- submitTxFromConstraints lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Valid scenario using offchain and onchain constraint mustPayToOtherScript with exact token value being minted
-- using inline datum
successfulUseOfMustPayToOtherScriptWithMintedTokenV2 :: SubmitTx -> LanguageContext -> TestTree
successfulUseOfMustPayToOtherScriptWithMintedTokenV2 submitTxFromConstraints lc =
    let onChainConstraint = asRedeemer $ MustPayToOtherScript someValidatorHash someDatum (adaAndTokenValue lc)
        contract = mustPayToOtherScriptInlineContractV2 submitTxFromConstraints lc (adaAndTokenValue lc) onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Successful use of offchain and onchain mustPayToOtherScript constraint with wallet's exact ada balance with inline datum"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using mustPayToOtherScript offchain constraint to include ada and token whilst onchain constraint checks for token value only
successfulUseOfMustPayToOtherScriptWhenOffchainIncludesTokenAndOnchainChecksOnlyToken :: SubmitTx -> LanguageContext -> TestTree
successfulUseOfMustPayToOtherScriptWhenOffchainIncludesTokenAndOnchainChecksOnlyToken submitTxFromConstraints lc =
    let onChainConstraint = asRedeemer $ MustPayToOtherScript someValidatorHash someDatum (tknValue lc)
        contract = mustPayToOtherScriptContract submitTxFromConstraints lc (adaAndTokenValue lc) onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Successful use of mustPayToOtherScript offchain constraint to include ada and token whilst onchain constraint checks for token value only"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using mustPayToOtherScript offchain constraint to include ada and token whilst onchain constraint checks for ada value only
-- FAILING when onchain checks for only ada value and token is present -- PLT-885
successfulUseOfMustPayToOtherScriptWhenOffchainIncludesTokenAndOnchainChecksOnlyAda :: SubmitTx -> LanguageContext -> TestTree
successfulUseOfMustPayToOtherScriptWhenOffchainIncludesTokenAndOnchainChecksOnlyAda submitTxFromConstraints lc =
    let onChainConstraint = asRedeemer $ MustPayToOtherScript someValidatorHash someDatum adaValue
        contract = mustPayToOtherScriptContract submitTxFromConstraints lc (adaAndTokenValue lc) onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Successful use of mustPayToOtherScript offchain constraint to include ada and token whilst onchain constraint checks for ada value only"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using offchain and onchain constraint mustPayToOtherScript in combination with mustSpendScriptOutputWithMatchingDatumAndValue to spend script's exact token balance
successfulUseOfMustPayToOtherScriptWithScriptsExactTokenBalance :: SubmitTx -> LanguageContext -> TestTree
successfulUseOfMustPayToOtherScriptWithScriptsExactTokenBalance submitTxFromConstraints lc =
    let otherValidatorHash = alwaysSucceedValidatorHash
        adaAndOtherTokenValue = adaValue <> otherTokenValue
        onChainConstraint = asRedeemer $ MustPayToOtherScript someValidatorHash someDatum otherTokenValue
        options = defaultCheckOptions & changeInitialWalletValue w1 (otherTokenValue <>)
        contract = do
            let lookups1 = Constraints.plutusV1OtherScript someValidator
                tx1 = Constraints.mustPayToOtherScript someValidatorHash someDatum adaAndOtherTokenValue
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

            scriptUtxos <- utxosAt $ Ledger.scriptHashAddress someValidatorHash
            let lookups2 = Constraints.plutusV1OtherScript someValidator
                        <> Constraints.unspentOutputs scriptUtxos
                        <> mintingPolicy lc (mustPayToOtherScriptPolicy lc)
                tx2 = Constraints.mustPayToOtherScript otherValidatorHash someDatum adaAndOtherTokenValue
                    <> Constraints.mustSpendScriptOutputWithMatchingDatumAndValue someValidatorHash (\d -> d == someDatum) (\v -> v == adaAndOtherTokenValue) (asRedeemer ())
                    <> Constraints.mustMintValueWithRedeemer onChainConstraint (tknValue lc)
            ledgerTx2 <- submitTxFromConstraints lookups2 tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2
    in checkPredicateOptions options
    "Successful use of offchain and onchain mustPayToOtherScript constraint in combination with mustSpendScriptOutputWithMatchingDatumAndValue to spend script's exact token balance"
    (assertValidatedTransactionCount 2)
    (void $ trace contract)

-- | Valid scenario where onchain mustPayToOtherScript constraint expects less ada than the actual value
successfulUseOfMustPayToOtherScriptWhenOnchainExpectsLowerAdaValue :: SubmitTx -> LanguageContext -> TestTree
successfulUseOfMustPayToOtherScriptWhenOnchainExpectsLowerAdaValue submitTxFromConstraints lc =
    let onChainConstraint = asRedeemer $ MustPayToOtherScript someValidatorHash someDatum (Ada.lovelaceValueOf $ adaAmount - 1)
        contract = mustPayToOtherScriptContract submitTxFromConstraints lc adaValue onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Successful use of mustPayToOtherScript onchain constraint when it expects less ada than the actual value"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Invalid contract that tries to use inline datum in a V1 script
mustPayToOtherScriptInlineContract :: SubmitTx -> LanguageContext -> Value.Value -> Redeemer -> Contract () Empty ContractError ()
mustPayToOtherScriptInlineContract submitTxFromConstraints lc offChainValue onChainConstraint = do
    let lookups1 = mintingPolicy lc $ mustPayToOtherScriptPolicy lc
        tx1 = Constraints.mustPayToOtherScriptInlineDatum someValidatorHash someDatum offChainValue
           <> Constraints.mustMintValueWithRedeemer onChainConstraint (tknValue lc)
    ledgerTx1 <- submitTxFromConstraints lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Contract error when ada amount to send to other script is greater than wallet balance
contractErrorWhenAttemptingToSpendMoreThanAdaBalance :: SubmitTx -> LanguageContext -> TestTree
contractErrorWhenAttemptingToSpendMoreThanAdaBalance submitTxFromConstraints lc =
    let onChainConstraint = asRedeemer $ MustPayToOtherScript someValidatorHash someDatum adaValue
        walletAdaBalance = Value.scale 10 utxoValue -- with fees this exceeds wallet balance
        contract = mustPayToOtherScriptContract submitTxFromConstraints lc walletAdaBalance onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Contract error when ada amount to send to other script is greater than wallet balance"
    (assertContractError contract (Trace.walletInstanceTag w1) (\case WalletContractError (InsufficientFunds _) -> True; _ -> False) "failed to throw error")
    (void $ trace contract)

-- | Contract error when token amount to send to other script is greater than wallet balance
contractErrorWhenAttemptingToSpendMoreThanTokenBalance :: SubmitTx -> LanguageContext -> TestTree
contractErrorWhenAttemptingToSpendMoreThanTokenBalance submitTxFromConstraints lc =
    let onChainConstraint = asRedeemer $ MustPayToOtherScript someValidatorHash someDatum otherTokenValue
        contract = mustPayToOtherScriptContract submitTxFromConstraints lc otherTokenValue onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Contract error when token amount to send to other script is greater than wallet balance"
    (assertContractError contract (Trace.walletInstanceTag w1) (\case WalletContractError (InsufficientFunds _) -> True; _ -> False) "failed to throw error")
    (void $ trace contract)

-- | Phase-1 failure when mustPayToOtherScript in a V1 script use inline datum
phase1FailureWhenPayToOtherScriptV1ScriptUseInlineDatum :: SubmitTx -> LanguageContext -> TestTree
phase1FailureWhenPayToOtherScriptV1ScriptUseInlineDatum submitTxFromConstraints lc =
    let onChainConstraint = asRedeemer $ MustPayToOtherScript someValidatorHash someDatum (adaAndTokenValue lc)
        contract = mustPayToOtherScriptInlineContract submitTxFromConstraints lc (adaAndTokenValue lc) onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Phase-1 failure when mustPayToOtherScript in a V1 script use inline datum"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.CardanoLedgerValidationError _ -> True; _ -> False }))
    (void $ trace contract)



-- | Phase-2 validation failure when onchain mustSpendScriptOutput constraint expects more than actual ada value
phase2ErrorWhenExpectingMoreThanValue :: SubmitTx -> LanguageContext -> TestTree
phase2ErrorWhenExpectingMoreThanValue submitTxFromConstraints lc =
    let onChainConstraint = asRedeemer $ MustPayToOtherScript someValidatorHash someDatum otherTokenValue
        contract = mustPayToOtherScriptContract submitTxFromConstraints lc adaValue onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Phase-2 validation failure when when token amount sent to other script is lower than actual value"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("Lb":_) _) -> True; _ -> False }))
    (void $ trace contract)


data UnitTest
instance Scripts.ValidatorTypes UnitTest

mkMustPayToOtherScriptPolicy :: (Constraints.TxConstraints () () -> sc -> Bool) -> ConstraintParams -> sc -> Bool
mkMustPayToOtherScriptPolicy checkScriptContext t = case t of
    MustPayToOtherScript vh d v            -> checkScriptContext (Constraints.mustPayToOtherScript vh d v)
    MustPayToOtherScriptAddress vh svh d v -> checkScriptContext (Constraints.mustPayToOtherScriptAddress vh svh d v)

mustPayToOtherScriptPolicyV1 :: Ledger.MintingPolicy
mustPayToOtherScriptPolicyV1 = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        checkedMkMustPayToOtherScriptPolicy = mkMustPayToOtherScriptPolicy Constraints.checkScriptContext
        wrap = Scripts.mkUntypedMintingPolicy checkedMkMustPayToOtherScriptPolicy

mustPayToOtherScriptPolicyV2 :: Ledger.MintingPolicy
mustPayToOtherScriptPolicyV2 = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        checkedMkMustPayToOtherScriptPolicy = mkMustPayToOtherScriptPolicy V2.Constraints.checkScriptContext
        wrap = V2.Scripts.mkUntypedMintingPolicy checkedMkMustPayToOtherScriptPolicy

data LanguageContext
   = LanguageContext
   { mustPayToOtherScriptPolicy :: Ledger.MintingPolicy
   , mintingPolicy              :: forall a. Ledger.MintingPolicy -> Constraints.ScriptLookups a
   , mintingPolicyHash          :: Ledger.MintingPolicy -> Ledger.MintingPolicyHash
   }

languageContextV1 :: LanguageContext
languageContextV1 = LanguageContext
    mustPayToOtherScriptPolicyV1
    Constraints.plutusV1MintingPolicy
    PSU.V1.mintingPolicyHash


languageContextV2 :: LanguageContext
languageContextV2 = LanguageContext
    mustPayToOtherScriptPolicyV2
    Constraints.plutusV2MintingPolicy
    PSU.V2.mintingPolicyHash


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


mustPayToOtherScriptPolicyHash :: LanguageContext -> Ledger.MintingPolicyHash
mustPayToOtherScriptPolicyHash lc = mintingPolicyHash lc $ mustPayToOtherScriptPolicy lc

mustPayToOtherScriptPolicyCurrencySymbol :: LanguageContext -> Ledger.CurrencySymbol
mustPayToOtherScriptPolicyCurrencySymbol = Value.mpsSymbol . mustPayToOtherScriptPolicyHash

data ConstraintParams = MustPayToOtherScript PSU.V1.ValidatorHash Ledger.Datum Value.Value
                      | MustPayToOtherScriptAddress PSU.V1.ValidatorHash PSU.V1.StakeValidatorHash Ledger.Datum Value.Value
    deriving (Show)

PlutusTx.unstableMakeIsData ''ConstraintParams
