{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.TxConstraints.MustPayToOtherScript(tests) where

import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Control.Lens ((&))
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints.OffChain qualified as Constraints (plutusV1MintingPolicy, plutusV1OtherScript, unspentOutputs)
import Ledger.Constraints.OnChain.V1 qualified as Constraints (checkScriptContext)
import Ledger.Constraints.TxConstraints qualified as Constraints (mustMintValueWithRedeemer, mustPayToOtherScript,
                                                                  mustPayToOtherScriptAddress)
import Ledger.Generators (someTokenValue)
import Ledger.Scripts (ScriptError (EvaluationError))
import Ledger.Test (someValidator, someValidatorHash)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertContractError, assertFailedTransaction, assertValidatedTransactionCount,
                             changeInitialWalletValue, checkPredicateOptions, defaultCheckOptions, w1)
import Plutus.Script.Utils.V1.Generators (alwaysSucceedValidatorHash)
import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (CurrencySymbol (CurrencySymbol), Datum (Datum), Redeemer (Redeemer),
                             ToData (toBuiltinData), UnsafeFromData (unsafeFromBuiltinData))
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Wallet (WalletAPIError (InsufficientFunds))

-- Constraint's functions should soon be changed to use Address instead of PaymentPubKeyHash and StakeKeyHash
tests :: TestTree
tests =
    testGroup "MustPayToOtherScript"
        [ successfulUseOfMustPayToOtherScriptWithMintedToken
        , successfulUseOfMustPayToOtherScriptWhenOffchainIncludesTokenAndOnchainChecksOnlyToken
        --, successfulUseOfMustPayToOtherScriptWhenOffchainIncludesTokenAndOnchainChecksOnlyAda -- FAILING when onchain checks for only ada value and token is present
        --, successfulUseOfMustPayToOtherScriptWithScriptsExactTokenBalance                     -- FAILING with WalletContractError InsufficientFunds, it seems to only be considering wallet for token, not the someValidatorHash script
        , successfulUseOfMustPayToOtherScriptWhenOnchainExpectsLowerAdaValue
        , contractErrorWhenAttemptingToSpendMoreThanAdaBalance
        , contractErrorWhenAttemptingToSpendMoreThanTokenBalance
        , phase2ErrorWhenExpectingMoreThanValue
         ]

someDatum :: Datum
someDatum = Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData ("datum" :: P.BuiltinByteString)

otherDatum :: Datum
otherDatum = Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData ("other datum" :: P.BuiltinByteString)

utxoValue :: Value.Value
utxoValue = Ada.lovelaceValueOf 10_000_000

adaAmount :: Integer
adaAmount = 5_000_000

adaValue :: Value.Value
adaValue = Ada.lovelaceValueOf adaAmount

tknValue :: Value.Value
tknValue = Value.singleton mustPayToOtherScriptPolicyCurrencySymbol "mint-me" 1

adaAndTokenValue :: Value.Value
adaAndTokenValue = adaValue <> tknValue

otherTokenValue :: Value.Value
otherTokenValue = someTokenValue "someToken" 1

asRedeemer :: PlutusTx.ToData a => a -> Redeemer
asRedeemer a = Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a

trace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void $ Trace.waitNSlots 1

-- | Contract to a single transaction with mustSpendScriptOutputs offchain constraint and mint with policy using matching onchain constraint
mustPayToOtherScriptContract :: Value.Value -> Redeemer -> Contract () Empty ContractError ()
mustPayToOtherScriptContract offChainValue onChainConstraint = do
    let lookups1 = Constraints.plutusV1MintingPolicy mustPayToOtherScriptPolicy
        tx1 = Constraints.mustPayToOtherScript someValidatorHash someDatum offChainValue
           <> Constraints.mustMintValueWithRedeemer onChainConstraint tknValue
    ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Valid scenario using offchain and onchain constraint mustPayToOtherScript with exact token value being minted
successfulUseOfMustPayToOtherScriptWithMintedToken :: TestTree
successfulUseOfMustPayToOtherScriptWithMintedToken =
    let onChainConstraint = asRedeemer $ MustPayToOtherScript someValidatorHash someDatum adaAndTokenValue
        contract = mustPayToOtherScriptContract adaAndTokenValue onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Successful use of offchain and onchain mustPayToOtherScript constraint with wallet's exact ada balance"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using mustPayToOtherScript offchain constraint to include ada and token whilst onchain constraint checks for token value only
successfulUseOfMustPayToOtherScriptWhenOffchainIncludesTokenAndOnchainChecksOnlyToken :: TestTree
successfulUseOfMustPayToOtherScriptWhenOffchainIncludesTokenAndOnchainChecksOnlyToken =
    let onChainConstraint = asRedeemer $ MustPayToOtherScript someValidatorHash someDatum tknValue
        contract = mustPayToOtherScriptContract adaAndTokenValue onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Successful use of mustPayToOtherScript offchain constraint to include ada and token whilst onchain constraint checks for token value only"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using mustPayToOtherScript offchain constraint to include ada and token whilst onchain constraint checks for ada value only
-- FAILING when onchain checks for only ada value and token is present
successfulUseOfMustPayToOtherScriptWhenOffchainIncludesTokenAndOnchainChecksOnlyAda :: TestTree
successfulUseOfMustPayToOtherScriptWhenOffchainIncludesTokenAndOnchainChecksOnlyAda =
    let onChainConstraint = asRedeemer $ MustPayToOtherScript someValidatorHash someDatum adaValue
        contract = mustPayToOtherScriptContract adaAndTokenValue onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Successful use of mustPayToOtherScript offchain constraint to include ada and token whilst onchain constraint checks for ada value only"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Valid scenario using offchain and onchain constraint mustPayToOtherScript with script's exact token balance
-- FAILING with WalletContractError InsufficientFunds, it seems to only be considering wallet for token, not the someValidatorHash script
successfulUseOfMustPayToOtherScriptWithScriptsExactTokenBalance :: TestTree
successfulUseOfMustPayToOtherScriptWithScriptsExactTokenBalance =
        let otherValidatorHash = alwaysSucceedValidatorHash
            adaAndOtherTokenValue = adaValue <> otherTokenValue
            onChainConstraint = asRedeemer $ MustPayToOtherScript someValidatorHash someDatum otherTokenValue
            options = defaultCheckOptions & changeInitialWalletValue w1 (otherTokenValue <>)
            contract = do
                let lookups1 = Constraints.plutusV1OtherScript someValidator
                    tx1 = Constraints.mustPayToOtherScript someValidatorHash someDatum adaAndOtherTokenValue
                ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
                awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

                scriptUtxos <- utxosAt $ Ledger.scriptHashAddress someValidatorHash
                let lookups2 = Constraints.plutusV1OtherScript someValidator
                             <> Constraints.unspentOutputs scriptUtxos
                            <> Constraints.plutusV1MintingPolicy mustPayToOtherScriptPolicy
                    tx2 = Constraints.mustPayToOtherScript otherValidatorHash someDatum adaAndOtherTokenValue
                       <> Constraints.mustMintValueWithRedeemer onChainConstraint tknValue
                ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
                awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

    in checkPredicateOptions options
    "Successful use of offchain and onchain mustPayToOtherScript constraint with script's exact token balance"
    (assertValidatedTransactionCount 2)
    (void $ trace contract)

-- | Valid scenario where onchain mustPayToOtherScript constraint expects less ada than the actual value
successfulUseOfMustPayToOtherScriptWhenOnchainExpectsLowerAdaValue :: TestTree
successfulUseOfMustPayToOtherScriptWhenOnchainExpectsLowerAdaValue =
    let onChainConstraint = asRedeemer $ MustPayToOtherScript someValidatorHash someDatum (Ada.lovelaceValueOf $ adaAmount - 1)
        contract = mustPayToOtherScriptContract adaValue onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Successful use of mustPayToOtherScript onchain constraint when it expects less ada than the actual value"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Contract error when ada amount to send to other script is greater than wallet balance
contractErrorWhenAttemptingToSpendMoreThanAdaBalance :: TestTree
contractErrorWhenAttemptingToSpendMoreThanAdaBalance =
    let onChainConstraint = asRedeemer $ MustPayToOtherScript someValidatorHash someDatum adaValue
        walletAdaBalance = Value.scale 10 utxoValue -- with fees this exceeds wallet balance
        contract = mustPayToOtherScriptContract walletAdaBalance onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Contract error when ada amount to send to other script is greater than wallet balance"
    (assertContractError contract (Trace.walletInstanceTag w1) (\case WalletContractError (InsufficientFunds _) -> True; _ -> False) "failed to throw error")
    (void $ trace contract)

-- | Contract error when token amount to send to other script is greater than wallet balance
contractErrorWhenAttemptingToSpendMoreThanTokenBalance :: TestTree
contractErrorWhenAttemptingToSpendMoreThanTokenBalance =
    let onChainConstraint = asRedeemer $ MustPayToOtherScript someValidatorHash someDatum otherTokenValue
        contract = mustPayToOtherScriptContract otherTokenValue onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Contract error when token amount to send to other script is greater than wallet balance"
    (assertContractError contract (Trace.walletInstanceTag w1) (\case WalletContractError (InsufficientFunds _) -> True; _ -> False) "failed to throw error")
    (void $ trace contract)

-- | Phase-2 validation failure when onchain mustSpendScriptOutput constraint expects more than actual ada value
phase2ErrorWhenExpectingMoreThanValue :: TestTree
phase2ErrorWhenExpectingMoreThanValue =
    let onChainConstraint = asRedeemer $ MustPayToOtherScript someValidatorHash someDatum otherTokenValue
        contract = mustPayToOtherScriptContract adaValue onChainConstraint

    in checkPredicateOptions defaultCheckOptions
    "Phase-2 validation failure when when token amount sent to other script is lower than actual value"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("Lb":_) _) -> True; _ -> False }))
    (void $ trace contract)

data UnitTest
instance Scripts.ValidatorTypes UnitTest

{-# INLINEABLE mkMustPayToOtherScriptPolicy #-}
mkMustPayToOtherScriptPolicy :: ConstraintParams -> Ledger.ScriptContext -> Bool
mkMustPayToOtherScriptPolicy t = case t of
    MustPayToOtherScript vh d v            -> Constraints.checkScriptContext @() @() (Constraints.mustPayToOtherScript vh d v)
    MustPayToOtherScriptAddress vh svh d v -> Constraints.checkScriptContext @() @() (Constraints.mustPayToOtherScriptAddress vh svh d v)

mustPayToOtherScriptPolicy :: Scripts.MintingPolicy
mustPayToOtherScriptPolicy = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        wrap = Scripts.mkUntypedMintingPolicy mkMustPayToOtherScriptPolicy

mustPayToOtherScriptPolicyHash :: Ledger.MintingPolicyHash
mustPayToOtherScriptPolicyHash = PSU.V1.mintingPolicyHash mustPayToOtherScriptPolicy

mustPayToOtherScriptPolicyCurrencySymbol :: CurrencySymbol
mustPayToOtherScriptPolicyCurrencySymbol = CurrencySymbol $ unsafeFromBuiltinData $ toBuiltinData mustPayToOtherScriptPolicyHash

data ConstraintParams = MustPayToOtherScript PSU.V1.ValidatorHash Datum Value.Value
                      | MustPayToOtherScriptAddress PSU.V1.ValidatorHash PSU.V1.StakeValidatorHash Datum Value.Value
    deriving (Show)

PlutusTx.unstableMakeIsData ''ConstraintParams
