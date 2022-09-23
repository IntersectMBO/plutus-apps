{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.TxConstraints.MustPayToPubKeyAddress(tests) where

import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints (OutDatum (Hashed, Inline), mustMintValueWithRedeemer,
                                                    mustPayToPubKey, mustPayToPubKeyAddress, mustPayWithDatumToPubKey,
                                                    mustPayWithDatumToPubKeyAddress, plutusV1MintingPolicy,
                                                    plutusV2MintingPolicy)
import Ledger.Constraints.OnChain.V1 qualified as Constraints (checkScriptContext)
import Ledger.Constraints.OnChain.V2 qualified as V2.Constraints
import Ledger.Scripts (ScriptError (EvaluationError))
import Ledger.Test (asDatum, asRedeemer)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertFailedTransaction, assertValidatedTransactionCount, checkPredicate,
                             mockWalletPaymentPubKeyHash, w1, w2)
import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Script.Utils.V2.Scripts qualified as PSU.V2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2.Scripts
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Contexts qualified as V2.Scripts
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

-- Constraint's functions should soon be changed to use Address instead of PaymentPubKeyHash and StakeKeyHash
tests :: TestTree
tests =
    testGroup "MustPayToPubKeyAddress"
        [ successfulUseOfMustPayToPubKeyWithMintedTokenValue
        , successfulUseOfMustPayToPubKeyWhenOffchainIncludesTokenAndOnchainChecksOnlyToken
        , successfulUseOfMustPayToPubKeyWhenOffchainIncludesTokenAndOnchainChecksOnlyAda
        , successfulUseOfMustPayToPubKeyExpectingALowerAdaValue
        , successfulUseOfMustPayToPubKeyAddress
        , successfulUseOfMustPayWithDatumToPubKey
        , successfulUseOfMustPayWithInlineDatumToPubKeyV2
        , successfulUseOfMustPayWithDatumToPubKeyAddress
        , phase1FailureWhenUsingInlineDatumWithV1
        , phase2FailureWhenUsingUnexpectedPaymentPubKeyHash
        --, phase2FailureWhenUsingUnexpectedStakePubKeyHash -- onchain check not implemented
        , phase2FailureWhenUsingUnexpectedDatum
        , phase2FailureWhenUsingUnexpectedValue
         ]

someDatum :: Ledger.Datum
someDatum = asDatum @P.BuiltinByteString "datum"

otherDatum :: Ledger.Datum
otherDatum = asDatum @P.BuiltinByteString "other datum"

adaAmount :: Integer
adaAmount = 5_000_000

adaValue :: Value.Value
adaValue = Ada.lovelaceValueOf adaAmount

tknValue :: Value.Value
tknValue = Value.singleton mustPayToPubKeyAddressPolicyCurrencySymbol "mint-me" 1

tknValueV2 :: Value.Value
tknValueV2 = Value.singleton mustPayToPubKeyAddressPolicyCurrencySymbolV2 "mint-me" 1

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
successfulUseOfMustPayToPubKeyWithMintedTokenValue :: TestTree
successfulUseOfMustPayToPubKeyWithMintedTokenValue =
    let adaAndTokenValue = adaValue <> tknValue
        onChainConstraint = asRedeemer $ MustPayToPubKey w2PaymentPubKeyHash adaAndTokenValue
        contract = do
            let lookups1 = Constraints.plutusV1MintingPolicy mustPayToPubKeyAddressPolicy
                tx1 = Constraints.mustPayToPubKey w2PaymentPubKeyHash adaAndTokenValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint tknValue
            ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Successful use of offchain and onchain mustPayToPubKey constraint for native token value"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using mustPayToPubKey offchain constraint to include ada and token whilst onchain constraint checks for token value only
successfulUseOfMustPayToPubKeyWhenOffchainIncludesTokenAndOnchainChecksOnlyToken :: TestTree
successfulUseOfMustPayToPubKeyWhenOffchainIncludesTokenAndOnchainChecksOnlyToken =
    let adaAndTokenValue = adaValue <> tknValue
        onChainConstraint = asRedeemer $ MustPayToPubKey w2PaymentPubKeyHash tknValue
        contract = do
            let lookups1 = Constraints.plutusV1MintingPolicy mustPayToPubKeyAddressPolicy
                tx1 = Constraints.mustPayToPubKey w2PaymentPubKeyHash adaAndTokenValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint tknValue
            ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Successful use of mustPayToPubKey offchain constraint to include ada and token whilst onchain constraint checks for token value only"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using mustPayToPubKey offchain constraint to include ada and token whilst onchain constraint checks for ada value only
successfulUseOfMustPayToPubKeyWhenOffchainIncludesTokenAndOnchainChecksOnlyAda :: TestTree
successfulUseOfMustPayToPubKeyWhenOffchainIncludesTokenAndOnchainChecksOnlyAda =
    let adaAndTokenValue = adaValue <> tknValue
        onChainConstraint = asRedeemer $ MustPayToPubKey w2PaymentPubKeyHash adaValue
        contract = do
            let lookups1 = Constraints.plutusV1MintingPolicy mustPayToPubKeyAddressPolicy
                tx1 = Constraints.mustPayToPubKey w2PaymentPubKeyHash adaAndTokenValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint tknValue
            ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Successful use of mustPayToPubKey offchain constraint to include ada and token whilst onchain constraint checks for ada value only"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario where the onchain mustPayToPubKey constraint expects less ada than the actual value
successfulUseOfMustPayToPubKeyExpectingALowerAdaValue :: TestTree
successfulUseOfMustPayToPubKeyExpectingALowerAdaValue =
    let onChainConstraint = asRedeemer $ MustPayToPubKey w2PaymentPubKeyHash (Ada.lovelaceValueOf $ adaAmount - 1)
        contract = do
            let lookups1 = Constraints.plutusV1MintingPolicy mustPayToPubKeyAddressPolicy
                tx1 = Constraints.mustPayToPubKey w2PaymentPubKeyHash adaValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint tknValue
            ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Successful use of onchain mustPayToPubKey constraint when it expects less ada than the actual value"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using offchain and onchain constraint mustPayToPubKeyAddress with ada-only value
successfulUseOfMustPayToPubKeyAddress :: TestTree
successfulUseOfMustPayToPubKeyAddress =
    let onChainConstraint = asRedeemer $ MustPayToPubKeyAddress w2PaymentPubKeyHash w2StakePubKeyHash adaValue
        contract = do
            let lookups1 = Constraints.plutusV1MintingPolicy mustPayToPubKeyAddressPolicy
                tx1 = Constraints.mustPayToPubKeyAddress w2PaymentPubKeyHash w2StakePubKeyHash adaValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint tknValue
            ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Successful use of offchain and onchain mustPayToPubKeyAddress constraint for ada-only value"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using offchain and onchain constraint mustPayWithDatumToPubKey with bytestring datum and ada value
successfulUseOfMustPayWithDatumToPubKey :: TestTree
successfulUseOfMustPayWithDatumToPubKey =
    let onChainConstraint = asRedeemer $ MustPayWithDatumToPubKey w2PaymentPubKeyHash someDatum adaValue
        contract = do
            let lookups1 = Constraints.plutusV1MintingPolicy mustPayToPubKeyAddressPolicy
                tx1 = Constraints.mustPayWithDatumToPubKey w2PaymentPubKeyHash (Constraints.Hashed someDatum) adaValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint tknValue
            ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Successful use of offchain and onchain mustPayWithDatumToPubKey constraint with bytestring datum and ada value"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using offchain and onchain constraint mustPayWithDatumToPubKey with inline bytestring datum and ada value
successfulUseOfMustPayWithInlineDatumToPubKeyV2 :: TestTree
successfulUseOfMustPayWithInlineDatumToPubKeyV2 =
    let onChainConstraint = asRedeemer $ MustPayWithDatumToPubKey w2PaymentPubKeyHash someDatum adaValue
        contract = do
            let lookups1 = Constraints.plutusV2MintingPolicy mustPayToPubKeyAddressPolicyV2
                tx1 = Constraints.mustPayWithDatumToPubKey w2PaymentPubKeyHash (Constraints.Inline someDatum) adaValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint tknValueV2
            ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Successful use of offchain and onchain mustPayWithDatumToPubKey constraint with inline bytestring datum and ada value"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using offchain and onchain constraint mustPayWithDatumToPubKeyAddress with bytestring datum and ada value
successfulUseOfMustPayWithDatumToPubKeyAddress :: TestTree
successfulUseOfMustPayWithDatumToPubKeyAddress =
    let onChainConstraint = asRedeemer $ MustPayWithDatumToPubKeyAddress w2PaymentPubKeyHash w2StakePubKeyHash someDatum adaValue
        contract = do
            let lookups1 = Constraints.plutusV1MintingPolicy mustPayToPubKeyAddressPolicy
                tx1 = Constraints.mustPayWithDatumToPubKeyAddress w2PaymentPubKeyHash w2StakePubKeyHash (Constraints.Hashed someDatum) adaValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint tknValue
            ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Successful use of offchain and onchain mustPayWithDatumToPubKeyAddress constraint with bytestring datum and ada value"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Phase-1 failure when mustPayToPubKeyAddress in a V1 script use inline datum
phase1FailureWhenUsingInlineDatumWithV1 :: TestTree
phase1FailureWhenUsingInlineDatumWithV1 =
    let onChainConstraint = asRedeemer $ MustPayWithDatumToPubKey w2PaymentPubKeyHash someDatum adaValue
        contract = do
            let lookups1 = Constraints.plutusV1MintingPolicy mustPayToPubKeyAddressPolicy
                tx1 = Constraints.mustPayWithDatumToPubKey w2PaymentPubKeyHash (Constraints.Inline someDatum) adaValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint tknValue
            ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Phase-1 failure when mustPayToPubKeyAddress in a V1 script use inline datum"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.CardanoLedgerValidationError _ -> True; _ -> False }))
    (void $ trace contract)

-- | Phase-2 failure when onchain mustPayWithDatumToPubKeyAddress constraint cannot verify the PaymentPubkeyHash"
phase2FailureWhenUsingUnexpectedPaymentPubKeyHash :: TestTree
phase2FailureWhenUsingUnexpectedPaymentPubKeyHash =
    let onChainConstraint = asRedeemer $ MustPayWithDatumToPubKeyAddress w2PaymentPubKeyHash w2StakePubKeyHash someDatum adaValue
        contract = do
            let lookups1 = Constraints.plutusV1MintingPolicy mustPayToPubKeyAddressPolicy
                tx1 = Constraints.mustPayWithDatumToPubKeyAddress w1PaymentPubKeyHash w2StakePubKeyHash (Constraints.Hashed someDatum) adaValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint tknValue
            ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Phase-2 validation failure occurs when onchain mustPayWithDatumToPubKeyAddress constraint sees an unexpected PaymentPubkeyHash"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("La":_) _) -> True; _ -> False }))
    (void $ trace contract)

-- | Phase-2 failure when onchain mustPayWithDatumToPubKeyAddress constraint cannot verify the Datum"
phase2FailureWhenUsingUnexpectedDatum :: TestTree
phase2FailureWhenUsingUnexpectedDatum =
    let onChainConstraint = asRedeemer $ MustPayWithDatumToPubKeyAddress w2PaymentPubKeyHash w2StakePubKeyHash otherDatum adaValue
        contract = do
            let lookups1 = Constraints.plutusV1MintingPolicy mustPayToPubKeyAddressPolicy
                tx1 = Constraints.mustPayWithDatumToPubKeyAddress w2PaymentPubKeyHash w2StakePubKeyHash (Constraints.Hashed someDatum) adaValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint tknValue
            ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Phase-2 validation failure occurs when onchain mustPayWithDatumToPubKeyAddress constraint sees an unexpected Datum"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("La":_) _) -> True; _ -> False }))
    (void $ trace contract)

-- | Phase-2 failure when onchain mustPayWithDatumToPubKeyAddress constraint cannot verify the Value"
phase2FailureWhenUsingUnexpectedValue :: TestTree
phase2FailureWhenUsingUnexpectedValue =
    let onChainConstraint = asRedeemer $ MustPayWithDatumToPubKeyAddress w2PaymentPubKeyHash w2StakePubKeyHash someDatum (Ada.lovelaceValueOf $ adaAmount + 1)
        contract = do
            let lookups1 = Constraints.plutusV1MintingPolicy mustPayToPubKeyAddressPolicy
                tx1 = Constraints.mustPayWithDatumToPubKeyAddress w2PaymentPubKeyHash w2StakePubKeyHash (Constraints.Hashed someDatum) adaValue
                   <> Constraints.mustMintValueWithRedeemer onChainConstraint tknValue
            ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Phase-2 validation failure occurs when onchain mustPayWithDatumToPubKeyAddress constraint sees an unexpected Value"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("La":_) _) -> True; _ -> False }))
    (void $ trace contract)

data UnitTest
instance Scripts.ValidatorTypes UnitTest

{-# INLINEABLE mkMustPayToPubKeyAddressPolicy #-}
mkMustPayToPubKeyAddressPolicy :: ConstraintParams -> Ledger.ScriptContext -> Bool
mkMustPayToPubKeyAddressPolicy t = case t of
    MustPayToPubKey ppkh v                        -> Constraints.checkScriptContext @() @() (Constraints.mustPayToPubKey ppkh v)
    MustPayToPubKeyAddress ppkh spkh v            -> Constraints.checkScriptContext @() @() (Constraints.mustPayToPubKeyAddress ppkh spkh v)
    MustPayWithDatumToPubKey ppkh d v             -> Constraints.checkScriptContext @() @() (Constraints.mustPayWithDatumToPubKey ppkh (Constraints.Hashed d) v)
    MustPayWithDatumToPubKeyAddress ppkh spkh d v -> Constraints.checkScriptContext @() @() (Constraints.mustPayWithDatumToPubKeyAddress ppkh spkh (Constraints.Hashed d) v)

{-# INLINEABLE mkMustPayToPubKeyAddressPolicyV2 #-}
mkMustPayToPubKeyAddressPolicyV2 :: ConstraintParams -> V2.Scripts.ScriptContext -> Bool
mkMustPayToPubKeyAddressPolicyV2 t = case t of
    MustPayToPubKey ppkh v                        -> V2.Constraints.checkScriptContext @() @() (Constraints.mustPayToPubKey ppkh v)
    MustPayToPubKeyAddress ppkh spkh v            -> V2.Constraints.checkScriptContext @() @() (Constraints.mustPayToPubKeyAddress ppkh spkh v)
    MustPayWithDatumToPubKey ppkh d v             -> V2.Constraints.checkScriptContext @() @() (Constraints.mustPayWithDatumToPubKey ppkh (Constraints.Inline d) v)
    MustPayWithDatumToPubKeyAddress ppkh spkh d v -> V2.Constraints.checkScriptContext @() @() (Constraints.mustPayWithDatumToPubKeyAddress ppkh spkh (Constraints.Inline d) v)

mustPayToPubKeyAddressPolicy :: Scripts.MintingPolicy
mustPayToPubKeyAddressPolicy = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        wrap = Scripts.mkUntypedMintingPolicy mkMustPayToPubKeyAddressPolicy

mustPayToPubKeyAddressPolicyV2 :: V2.Scripts.MintingPolicy
mustPayToPubKeyAddressPolicyV2 = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        wrap = V2.Scripts.mkUntypedMintingPolicy mkMustPayToPubKeyAddressPolicyV2

mustPayToPubKeyAddressPolicyHash :: Ledger.MintingPolicyHash
mustPayToPubKeyAddressPolicyHash = PSU.V1.mintingPolicyHash mustPayToPubKeyAddressPolicy

mustPayToPubKeyAddressPolicyHashV2 :: Ledger.MintingPolicyHash
mustPayToPubKeyAddressPolicyHashV2 = PSU.V2.mintingPolicyHash mustPayToPubKeyAddressPolicyV2

mustPayToPubKeyAddressPolicyCurrencySymbol :: CurrencySymbol
mustPayToPubKeyAddressPolicyCurrencySymbol = CurrencySymbol $ unsafeFromBuiltinData $ toBuiltinData mustPayToPubKeyAddressPolicyHash

mustPayToPubKeyAddressPolicyCurrencySymbolV2 :: CurrencySymbol
mustPayToPubKeyAddressPolicyCurrencySymbolV2 = CurrencySymbol $ unsafeFromBuiltinData $ toBuiltinData mustPayToPubKeyAddressPolicyHashV2

data ConstraintParams = MustPayToPubKey Ledger.PaymentPubKeyHash Value.Value
                      | MustPayToPubKeyAddress Ledger.PaymentPubKeyHash Ledger.StakePubKeyHash Value.Value
                      | MustPayWithDatumToPubKey Ledger.PaymentPubKeyHash Ledger.Datum Value.Value
                      | MustPayWithDatumToPubKeyAddress Ledger.PaymentPubKeyHash Ledger.StakePubKeyHash Ledger.Datum Value.Value
    deriving (Show)

PlutusTx.unstableMakeIsData ''ConstraintParams
