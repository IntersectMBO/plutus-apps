{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.Contract.Tx.Constraints.MustPayToOtherScript(tests) where

import Control.Lens ((&), (??), (^.))
import Control.Monad (void)
import Spec.Contract.Error (cardanoLedgerErrorContaining, insufficientFundsError)
import Test.Tasty (TestTree, testGroup)

import Cardano.Api qualified as C
import Cardano.Node.Emulator.Generators (someTokenValue)
import Cardano.Node.Emulator.Params qualified as Params
import Ledger qualified
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OnChain.V1 qualified as Constraints
import Ledger.Constraints.OnChain.V2 qualified as V2.Constraints
import Ledger.Scripts (Redeemer)
import Ledger.Test (asDatum, asRedeemer, someCardanoAddress, someValidator, someValidatorHash)
import Ledger.Tx qualified as Tx
import Ledger.Tx.Constraints qualified as Tx.Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value.CardanoAPI qualified as Value
import Plutus.Contract as Con
import Plutus.Contract.Test (assertContractError, assertEvaluationError, assertFailedTransaction,
                             assertValidatedTransactionCount, changeInitialWalletValue, checkPredicateOptions,
                             defaultCheckOptions, emulatorConfig, w1)
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.V1.Generators (alwaysSucceedValidatorHash)
import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Trace qualified as Trace
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

-- Constraint's functions should soon be changed to use Address instead of PaymentPubKeyHash and StakeKeyHash
tests :: TestTree
tests =
    testGroup "MustPayToOtherScript"
      [ testGroup "ledger constraints" $ [v1Tests, v2Tests] ?? ledgerSubmitTx
      --, testGroup "cardano constraints" $ [v1Tests, v2Tests] ?? cardanoSubmitTx
      ]

v1Tests :: SubmitTx -> TestTree
v1Tests sub = testGroup "Plutus V1" $
   [ v1FeaturesTests
   , v2FeaturesNotAvailableTests
   ] ?? sub ?? Ledger.PlutusV1

v2Tests :: SubmitTx -> TestTree
v2Tests sub = testGroup "Plutus V2" $
  [ v1FeaturesTests
  , v2FeaturesTests
  ] ?? sub ?? Ledger.PlutusV2

v1FeaturesTests :: SubmitTx -> Ledger.Language -> TestTree
v1FeaturesTests sub t = testGroup "Plutus V1 features" $
    [ successfulUseOfMustPayToOtherScriptWithDatumInTxWithMintedToken
    , successfulUseOfMustPayToOtherScriptWithDatumInTxWhenOffchainIncludesTokenAndOnchainChecksOnlyToken
    , successfulUseOfMustPayToOtherScriptWithDatumInTxWhenOffchainIncludesTokenAndOnchainChecksOnlyAda
    , successfulUseOfMustPayToOtherScriptWithDatumInTxWithScriptsExactTokenBalance
    , successfulUseOfMustPayToOtherScriptWithDatumInTxWhenOnchainExpectsLowerAdaValue
    , successfulUseOfMustPayToOtherScriptWithDatumInTxWhenOnchainExpectsLowerTokenValue
    , contractErrorWhenAttemptingToSpendMoreThanAdaBalance
    , contractErrorWhenAttemptingToSpendMoreThanTokenBalance
    , phase2ErrorWhenExpectingMoreThanValue
    ] ?? sub ?? t

v2FeaturesTests :: SubmitTx -> Ledger.Language -> TestTree
v2FeaturesTests sub t = testGroup "Plutus V2 features" $
    [ successfulUseOfMustPayToOtherScriptWithInlineDatumWithMintedTokenV2
    ] ?? sub ?? t

v2FeaturesNotAvailableTests :: SubmitTx -> Ledger.Language -> TestTree
v2FeaturesNotAvailableTests sub t = testGroup "Plutus V2 features" $
    [ phase1FailureWhenPayToOtherScriptV1ScriptUseInlineDatum
    ] ?? sub ?? t

someDatum :: Ledger.Datum
someDatum = asDatum @P.BuiltinByteString "datum"

otherDatum :: Ledger.Datum
otherDatum = asDatum @P.BuiltinByteString "other datum"

adaAmount :: Integer
adaAmount = 25_000_000

tknAmount :: Integer
tknAmount = 5_000_000

adaValue :: C.Value
adaValue = Value.lovelaceValueOf adaAmount

tknValueOf :: Integer -> Ledger.Language -> C.Value
tknValueOf x tc = Value.singleton (mustPayToOtherScriptPolicyId tc) "mint-me" x

tknValue :: Ledger.Language -> C.Value
tknValue = tknValueOf tknAmount

adaAndTokenValue :: Ledger.Language -> C.Value
adaAndTokenValue = (adaValue <>) . tknValue

otherTokenValue :: C.Value
otherTokenValue = someTokenValue "someToken" 10

trace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void Trace.nextSlot

-- | Contract to a single transaction with mustSpendScriptOutputs offchain
-- constraint and mint with policy using matching onchain constraint.
mustPayToOtherScriptWithDatumInTxContract
    :: SubmitTx
    -> Ledger.Language
    -> C.Value
    -> Ledger.Redeemer
    -> Contract () Empty ContractError ()
mustPayToOtherScriptWithDatumInTxContract submitTxFromConstraints lc offChainValue onChainConstraint = do
    let lookups1 = Constraints.mintingPolicy $ mustPayToOtherScriptPolicy lc
        tx1 =
            Constraints.mustPayToOtherScriptWithDatumInTx
                someValidatorHash
                someDatum
                (Value.fromCardanoValue offChainValue)
           <> Constraints.mustMintValueWithRedeemer onChainConstraint (Value.fromCardanoValue $ tknValue lc)
    ledgerTx1 <- submitTxFromConstraints lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Valid scenario using offchain and onchain constraint
-- 'mustPayToOtherScriptWithDatumInTx' with exact token value being minted.
successfulUseOfMustPayToOtherScriptWithDatumInTxWithMintedToken :: SubmitTx -> Ledger.Language -> TestTree
successfulUseOfMustPayToOtherScriptWithDatumInTxWithMintedToken submitTxFromConstraints lc =
    let onChainConstraint =
            asRedeemer
            $ MustPayToOtherScriptWithDatumInTx
                someValidatorHash
                someDatum
                (Value.fromCardanoValue $ adaAndTokenValue lc)
        contract =
            mustPayToOtherScriptWithDatumInTxContract
                submitTxFromConstraints
                lc
                (adaAndTokenValue lc)
                onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Successful use of offchain and onchain mustPayToOtherScriptWithDatumInTx constraint with wallet's exact ada balance"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Contract to a single transaction with mustSpendScriptOutputs offchain constraint and mint with policy using
-- matching onchain constraint, using Plutus V2 script and inline datum
mustPayToOtherScriptWithInlineDatumContractV2
    :: SubmitTx
    -> Ledger.Language
    -> C.Value
    -> Redeemer
    -> Contract () Empty ContractError ()
mustPayToOtherScriptWithInlineDatumContractV2 submitTxFromConstraints lc offChainValue onChainConstraint = do
    let lookups1 = Constraints.mintingPolicy $ mustPayToOtherScriptPolicy lc
        tx1 =
            Constraints.mustPayToOtherScriptWithInlineDatum
                someValidatorHash
                someDatum
                (Value.fromCardanoValue offChainValue)
           <> Constraints.mustMintValueWithRedeemer onChainConstraint (Value.fromCardanoValue $ tknValue lc)
    ledgerTx1 <- submitTxFromConstraints lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Valid scenario using offchain and onchain constraint mustPayToOtherScriptWithDatumHash with exact token value being minted
-- using inline datum.
successfulUseOfMustPayToOtherScriptWithInlineDatumWithMintedTokenV2
    :: SubmitTx
    -> Ledger.Language
    -> TestTree
successfulUseOfMustPayToOtherScriptWithInlineDatumWithMintedTokenV2 submitTxFromConstraints lc =
    let onChainConstraint =
            asRedeemer
            $ MustPayToOtherScriptWithInlineDatum
                someValidatorHash
                someDatum
                (Value.fromCardanoValue $ adaAndTokenValue lc)
        contract =
            mustPayToOtherScriptWithInlineDatumContractV2
                submitTxFromConstraints
                lc
                (adaAndTokenValue lc)
                onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Successful use of offchain and onchain mustPayToOtherScriptWithInlineDatum constraint with wallet's exact ada balance with inline datum"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using mustPayToOtherScriptWithDatumHash offchain constraint to include ada and token whilst onchain constraint checks for token value only
successfulUseOfMustPayToOtherScriptWithDatumInTxWhenOffchainIncludesTokenAndOnchainChecksOnlyToken
    :: SubmitTx
    -> Ledger.Language
    -> TestTree
successfulUseOfMustPayToOtherScriptWithDatumInTxWhenOffchainIncludesTokenAndOnchainChecksOnlyToken
        submitTxFromConstraints lc =
    let onChainConstraint =
            asRedeemer
            $ MustPayToOtherScriptWithDatumInTx someValidatorHash someDatum (Value.fromCardanoValue $ tknValue lc)
        contract =
            mustPayToOtherScriptWithDatumInTxContract
                submitTxFromConstraints
                lc
                (adaAndTokenValue lc)
                onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Successful use of mustPayToOtherScriptWithDatumHash offchain constraint to include ada and token whilst onchain constraint checks for token value only"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using mustPayToOtherScriptWithDatumHash offchain constraint to include ada and token whilst onchain constraint checks for ada value only
successfulUseOfMustPayToOtherScriptWithDatumInTxWhenOffchainIncludesTokenAndOnchainChecksOnlyAda
    :: SubmitTx
    -> Ledger.Language
    -> TestTree
successfulUseOfMustPayToOtherScriptWithDatumInTxWhenOffchainIncludesTokenAndOnchainChecksOnlyAda
        submitTxFromConstraints lc =
    let onChainConstraint = asRedeemer $ MustPayToOtherScriptWithDatumInTx someValidatorHash someDatum (Value.fromCardanoValue adaValue)
        contract =
            mustPayToOtherScriptWithDatumInTxContract
                submitTxFromConstraints
                lc
                (adaAndTokenValue lc)
                onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Successful use of mustPayToOtherScriptWithDatumInTx offchain constraint to include ada and token whilst onchain constraint checks for ada value only"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using offchain and onchain constraint mustPayToOtherScriptWithDatumHash
-- in combination with mustSpendScriptOutputWithMatchingDatumAndValue to spend
-- script's exact token balance.
successfulUseOfMustPayToOtherScriptWithDatumInTxWithScriptsExactTokenBalance
    :: SubmitTx
    -> Ledger.Language
    -> TestTree
successfulUseOfMustPayToOtherScriptWithDatumInTxWithScriptsExactTokenBalance submitTxFromConstraints lc =
    let otherValidatorHash = alwaysSucceedValidatorHash
        adaAndOtherTokenValue = adaValue <> otherTokenValue
        onChainConstraint = asRedeemer $ MustPayToOtherScriptWithDatumInTx someValidatorHash someDatum (Value.fromCardanoValue otherTokenValue)
        options = defaultCheckOptions & changeInitialWalletValue w1 (otherTokenValue <>)
        contract = do
            networkId <- Params.pNetworkId <$> getParams
            let lookups1 = Constraints.plutusV1OtherScript someValidator
                tx1 =
                    Constraints.mustPayToOtherScriptWithDatumInTx
                        someValidatorHash
                        someDatum
                        (Value.fromCardanoValue adaAndOtherTokenValue)
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

            scriptUtxos <- utxosAt $ someCardanoAddress networkId
            let lookups2 = Constraints.plutusV1OtherScript someValidator
                        <> Constraints.unspentOutputs scriptUtxos
                        <> Constraints.mintingPolicy (mustPayToOtherScriptPolicy lc)
                tx2 = Constraints.mustPayToOtherScriptWithDatumInTx
                        otherValidatorHash
                        someDatum
                        (Value.fromCardanoValue adaAndOtherTokenValue)
                   <> Constraints.mustSpendScriptOutputWithMatchingDatumAndValue
                        someValidatorHash
                        (\d -> d == someDatum)
                        (\v -> v == Value.fromCardanoValue adaAndOtherTokenValue)
                        (asRedeemer ())
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint (Value.fromCardanoValue $ tknValue lc)
            ledgerTx2 <- submitTxFromConstraints lookups2 tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2
    in checkPredicateOptions options
    "Successful use of offchain and onchain mustPayToOtherScriptWithDatumInTx constraint in combination with mustSpendScriptOutputWithMatchingDatumAndValue to spend script's exact token balance"
    (assertValidatedTransactionCount 2)
    (void $ trace contract)

-- | Valid scenario where onchain mustPayToOtherScriptWithDatumHash constraint expects less ada than the actual value
successfulUseOfMustPayToOtherScriptWithDatumInTxWhenOnchainExpectsLowerAdaValue
    :: SubmitTx
    -> Ledger.Language
    -> TestTree
successfulUseOfMustPayToOtherScriptWithDatumInTxWhenOnchainExpectsLowerAdaValue
        submitTxFromConstraints lc =
    let onChainConstraint =
            asRedeemer
            $ MustPayToOtherScriptWithDatumInTx
                someValidatorHash
                someDatum
                (Ada.lovelaceValueOf $ adaAmount - 1)
        contract =
            mustPayToOtherScriptWithDatumInTxContract
                submitTxFromConstraints
                lc
                adaValue
                onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Successful use of mustPayToOtherScriptWithDatumInTx onchain constraint when it expects less ada than the actual value"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario where onchain mustPayToOtherScriptWithDatumHash constraint expects less token than the actual value
successfulUseOfMustPayToOtherScriptWithDatumInTxWhenOnchainExpectsLowerTokenValue
    :: SubmitTx
    -> Ledger.Language
    -> TestTree
successfulUseOfMustPayToOtherScriptWithDatumInTxWhenOnchainExpectsLowerTokenValue
        submitTxFromConstraints lc =
    let onChainConstraint =
            asRedeemer
            $ MustPayToOtherScriptWithDatumInTx
                someValidatorHash
                someDatum
                (Value.fromCardanoValue $ tknValueOf (tknAmount - 1) lc)
        contract =
            mustPayToOtherScriptWithDatumInTxContract
                submitTxFromConstraints
                lc
                (adaAndTokenValue lc)
                onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Successful use of mustPayToOtherScriptWithDatumInTx onchain constraint when it expects less ada than the actual value"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Invalid contract that tries to use inline datum in a V1 script
mustPayToOtherScriptWithInlineDatumContract
    :: SubmitTx
    -> Ledger.Language
    -> C.Value
    -> Redeemer
    -> Contract () Empty ContractError ()
mustPayToOtherScriptWithInlineDatumContract submitTxFromConstraints lc offChainValue onChainConstraint = do
    let lookups1 = Constraints.mintingPolicy $ mustPayToOtherScriptPolicy lc
        tx1 =
            Constraints.mustPayToOtherScriptWithInlineDatum
                someValidatorHash
                someDatum
                (Value.fromCardanoValue offChainValue)
           <> Constraints.mustMintValueWithRedeemer onChainConstraint (Value.fromCardanoValue $ tknValue lc)
    ledgerTx1 <- submitTxFromConstraints lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Contract error when ada amount to send to other script is greater than wallet balance
contractErrorWhenAttemptingToSpendMoreThanAdaBalance :: SubmitTx -> Ledger.Language -> TestTree
contractErrorWhenAttemptingToSpendMoreThanAdaBalance submitTxFromConstraints lc =
    let onChainConstraint = asRedeemer $ MustPayToOtherScriptWithDatumInTx someValidatorHash someDatum (Value.fromCardanoValue adaValue)
        walletAdaBalance = Value.lovelaceValueOf 500_000_000 -- with fees this exceeds wallet balance
        contract =
            mustPayToOtherScriptWithDatumInTxContract
                submitTxFromConstraints
                lc
                walletAdaBalance
                onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Contract error when ada amount to send to other script is greater than wallet balance"
    (assertContractError contract (Trace.walletInstanceTag w1) insufficientFundsError "failed to throw error")
    (void $ trace contract)

-- | Contract error when token amount to send to other script is greater than wallet balance
contractErrorWhenAttemptingToSpendMoreThanTokenBalance :: SubmitTx -> Ledger.Language -> TestTree
contractErrorWhenAttemptingToSpendMoreThanTokenBalance submitTxFromConstraints lc =
    let onChainConstraint =
            asRedeemer
            $ MustPayToOtherScriptWithDatumInTx someValidatorHash someDatum (Value.fromCardanoValue otherTokenValue)
        contract =
            mustPayToOtherScriptWithDatumInTxContract
                submitTxFromConstraints
                lc
                otherTokenValue
                onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Contract error when token amount to send to other script is greater than wallet balance"
    (assertContractError contract (Trace.walletInstanceTag w1) insufficientFundsError "failed to throw error")
    (void $ trace contract)

-- | Phase-1 failure when mustPayToOtherScriptWithDatumHash in a V1 script use inline datum
phase1FailureWhenPayToOtherScriptV1ScriptUseInlineDatum :: SubmitTx -> Ledger.Language -> TestTree
phase1FailureWhenPayToOtherScriptV1ScriptUseInlineDatum submitTxFromConstraints lc =
    let onChainConstraint = asRedeemer $ MustPayToOtherScriptWithInlineDatum someValidatorHash someDatum (Value.fromCardanoValue $ adaAndTokenValue lc)
        contract = mustPayToOtherScriptWithInlineDatumContract submitTxFromConstraints lc (adaAndTokenValue lc) onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Phase-1 failure when mustPayToOtherScriptWithDatumHash in a V1 script use inline datum"
    (assertFailedTransaction (const $ cardanoLedgerErrorContaining ""))
    (void $ trace contract)



-- | Phase-2 validation failure when onchain mustSpendScriptOutput constraint expects more than actual ada value
phase2ErrorWhenExpectingMoreThanValue :: SubmitTx -> Ledger.Language -> TestTree
phase2ErrorWhenExpectingMoreThanValue submitTxFromConstraints lc =
    let onChainConstraint =
            asRedeemer
            $ MustPayToOtherScriptWithDatumInTx someValidatorHash someDatum (Value.fromCardanoValue otherTokenValue)
        contract =
            mustPayToOtherScriptWithDatumInTxContract
                submitTxFromConstraints
                lc
                adaValue
                onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Phase-2 validation failure when when token amount sent to other script is lower than actual value"
    (assertEvaluationError "La")
    (void $ trace contract)


data UnitTest
instance Scripts.ValidatorTypes UnitTest

mkMustPayToOtherScriptPolicy :: (Constraints.TxConstraints () () -> sc -> Bool) -> ConstraintParams -> sc -> Bool
mkMustPayToOtherScriptPolicy checkScriptContext t = case t of
    MustPayToOtherScriptWithDatumInTx vh d v ->
        checkScriptContext (Constraints.mustPayToOtherScriptWithDatumInTx vh d v)
    MustPayToOtherScriptAddressWithDatumInTx vh svh d v ->
        checkScriptContext (Constraints.mustPayToOtherScriptAddressWithDatumInTx vh svh d v)
    MustPayToOtherScriptWithInlineDatum vh d v ->
        checkScriptContext (Constraints.mustPayToOtherScriptWithInlineDatum vh d v)
    MustPayToOtherScriptAddressWithInlineDatum vh svh d v ->
        checkScriptContext (Constraints.mustPayToOtherScriptAddressWithInlineDatum vh svh d v)

mustPayToOtherScriptPolicyV1 :: Ledger.MintingPolicy
mustPayToOtherScriptPolicyV1 = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        checkedMkMustPayToOtherScriptPolicy = mkMustPayToOtherScriptPolicy Constraints.checkScriptContext
        wrap = Scripts.mkUntypedMintingPolicy checkedMkMustPayToOtherScriptPolicy

mustPayToOtherScriptPolicyV2 :: Ledger.MintingPolicy
mustPayToOtherScriptPolicyV2 = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        checkedMkMustPayToOtherScriptPolicy = mkMustPayToOtherScriptPolicy V2.Constraints.checkScriptContext
        wrap = Scripts.mkUntypedMintingPolicy checkedMkMustPayToOtherScriptPolicy

mustPayToOtherScriptPolicy :: Ledger.Language -> Scripts.Versioned Ledger.MintingPolicy
mustPayToOtherScriptPolicy = \case
  Ledger.PlutusV1 -> Scripts.Versioned mustPayToOtherScriptPolicyV1 Scripts.PlutusV1
  Ledger.PlutusV2 -> Scripts.Versioned mustPayToOtherScriptPolicyV2 Scripts.PlutusV2

mustPayToOtherScriptPolicyId :: Ledger.Language -> C.PolicyId
mustPayToOtherScriptPolicyId = Value.policyId . mustPayToOtherScriptPolicy

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

data ConstraintParams =
    MustPayToOtherScriptWithDatumInTx PSU.V1.ValidatorHash Ledger.Datum Ledger.Value
  | MustPayToOtherScriptAddressWithDatumInTx
        PSU.V1.ValidatorHash
        Ledger.StakingCredential
        Ledger.Datum
        Ledger.Value
  | MustPayToOtherScriptWithInlineDatum
        PSU.V1.ValidatorHash
        Ledger.Datum
        Ledger.Value
  | MustPayToOtherScriptAddressWithInlineDatum
        PSU.V1.ValidatorHash
        Ledger.StakingCredential
        Ledger.Datum
        Ledger.Value
    deriving (Show)

PlutusTx.unstableMakeIsData ''ConstraintParams
