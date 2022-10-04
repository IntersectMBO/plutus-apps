{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.TxConstraints.MustProduceAtLeast(tests) where

import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Data.Function ((&))
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints.OffChain qualified as Constraints (MkTxError (OwnPubKeyMissing), ownPaymentPubKeyHash,
                                                             typedValidatorLookups, unspentOutputs)
import Ledger.Constraints.OnChain.V1 qualified as Constraints (checkScriptContext)
import Ledger.Constraints.TxConstraints qualified as Constraints (collectFromTheScript, mustIncludeDatumInTx,
                                                                  mustPayToTheScript, mustProduceAtLeast)
import Ledger.Generators (someTokenValue)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertContractError, assertFailedTransaction, assertValidatedTransactionCount,
                             changeInitialWalletValue, checkPredicateOptions, defaultCheckOptions,
                             mockWalletPaymentPubKeyHash, w1, (.&&.))
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (Datum (Datum), ScriptContext, ValidatorHash)
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError))
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Prelude hiding (not)
import Wallet.API (WalletAPIError (InsufficientFunds))

tests :: TestTree
tests =
    testGroup "MustProduceAtLeast"
        [ spendAtLeastTheScriptBalance
        , spendLessThanScriptBalance
        , spendTokenBalanceFromScript
        , spendMoreThanScriptBalanceWithOwnWalletAsOwnPubkeyLookup
        --, spendMoreThanScriptBalanceWithOtherWalletAsOwnPubkeyLookup -- FAILING
        , contractErrorWhenUnableToSpendMoreThanScriptTokenBalance
        , contractErrorWhenOwnPaymentPubKeyHashLookupIsMissing
        , phase2FailureWhenProducedAdaAmountIsNotSatisfied
        , phase2FailureWhenProducedTokenAmountIsNotSatisfied
        ]

someTokens :: Integer -> Value.Value
someTokens = someTokenValue "someToken"

baseLovelaceLockedByScript :: Integer
baseLovelaceLockedByScript = 25_000_000

baseAdaValueLockedByScript :: Value.Value
baseAdaValueLockedByScript = Ada.lovelaceValueOf baseLovelaceLockedByScript

baseAdaAndTokenValueLockedByScript :: Value.Value
baseAdaAndTokenValueLockedByScript = baseAdaValueLockedByScript <> someTokens 1

-- | Valid contract containing all required lookups. Uses mustProduceAtLeast constraint with provided on-chain and off-chain values.
mustProduceAtLeastContract :: Value.Value -> Value.Value -> Value.Value -> Ledger.PaymentPubKeyHash -> Contract () Empty ContractError ()
mustProduceAtLeastContract offAmt onAmt baseScriptValue pkh = do
    let lookups1 = Constraints.typedValidatorLookups typedValidator
        tx1 = Constraints.mustPayToTheScript onAmt baseScriptValue
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    pubKeyUtxos <- utxosAt $ Ledger.pubKeyHashAddress pkh Nothing
    scriptUtxos <- utxosAt scrAddress
    let lookups2 = Constraints.typedValidatorLookups typedValidator
            <> Constraints.unspentOutputs pubKeyUtxos
            <> Constraints.unspentOutputs scriptUtxos
            <> Constraints.ownPaymentPubKeyHash pkh
        tx2 =
            Constraints.collectFromTheScript scriptUtxos ()
            <> Constraints.mustIncludeDatumInTx (Datum $ PlutusTx.toBuiltinData onAmt)
            <> Constraints.mustProduceAtLeast offAmt
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

trace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void $ Trace.waitNSlots 1

-- | Uses onchain and offchain constraint mustProduceAtLeast to spend entire ada balance locked by the script
spendAtLeastTheScriptBalance :: TestTree
spendAtLeastTheScriptBalance =
    let contract = mustProduceAtLeastContract baseAdaValueLockedByScript baseAdaValueLockedByScript baseAdaValueLockedByScript $ mockWalletPaymentPubKeyHash w1
    in  checkPredicateOptions
            defaultCheckOptions
            "Successful use of mustProduceAtLeast at script's exact balance"
            (assertValidatedTransactionCount 2)
            (void $ trace contract)

-- | Uses onchain and offchain constraint mustProduceAtLeast to spend less ada than is locked by the script
spendLessThanScriptBalance :: TestTree
spendLessThanScriptBalance =
    let amt = Ada.lovelaceValueOf (baseLovelaceLockedByScript - 500)
        contract = mustProduceAtLeastContract amt amt baseAdaValueLockedByScript $ mockWalletPaymentPubKeyHash w1
    in  checkPredicateOptions
            defaultCheckOptions
            "Successful use of mustProduceAtLeast below script's balance"
            (assertValidatedTransactionCount 2)
            (void $ trace contract )

-- | Uses onchain and offchain constraint mustProduceAtLeast to spend entire token balance locked by the script
spendTokenBalanceFromScript :: TestTree
spendTokenBalanceFromScript =
    let token = someTokens 1
        contract = mustProduceAtLeastContract (baseAdaValueLockedByScript <> token) (baseAdaValueLockedByScript <> token) baseAdaAndTokenValueLockedByScript $ mockWalletPaymentPubKeyHash w1
        options = defaultCheckOptions
            & changeInitialWalletValue w1 (token <>)
    in  checkPredicateOptions
            options
            "Successful use of mustProduceAtLeast sending tokens"
            (assertValidatedTransactionCount 2)
            (void $ trace contract)

-- | Uses onchain and offchain constraint mustProduceAtLeast to spend more than the ada balance locked by the script, excess is paid by own wallet.
spendMoreThanScriptBalanceWithOwnWalletAsOwnPubkeyLookup :: TestTree
spendMoreThanScriptBalanceWithOwnWalletAsOwnPubkeyLookup =
    let amt = Ada.lovelaceValueOf (baseLovelaceLockedByScript + 5_000_000)
        contract = mustProduceAtLeastContract amt amt baseAdaValueLockedByScript $ mockWalletPaymentPubKeyHash w1
    in  checkPredicateOptions
            defaultCheckOptions
            "Validation pass when mustProduceAtLeast is greater than script's balance"
            (assertValidatedTransactionCount 2)
            (void $ trace contract)

{- Failing due to https://github.com/input-output-hk/plutus-apps/issues/697: Errors with InsufficientFunds because it doesn't attempt to pay from w6
-- | Uses onchain and offchain constraint mustProduceAtLeast to spend more than the ada balance locked by the script and own wallet, excess is paid by other wallet.
spendMoreThanScriptBalanceWithOtherWalletAsOwnPubkeyLookup :: TestTree
spendMoreThanScriptBalanceWithOtherWalletAsOwnPubkeyLookup =
    let amt = Ada.lovelaceValueOf (baseLovelaceLockedByScript + 5_000_000)
        contract = mustProduceAtLeastContract amt amt baseAdaValueLockedByScript $ mockWalletPaymentPubKeyHash w6
        options = defaultCheckOptions
            & changeInitialWalletValue w1 (const amt) -- not enough funds remain for w1 to satisfy constraint
        traceWithW6Signing = do
            Trace.setSigningProcess w1 (Just $ signPrivateKeys [paymentPrivateKey $ walletToMockWallet' w1, paymentPrivateKey $ walletToMockWallet' w6])
            void $ Trace.activateContractWallet w1 contract
            void $ Trace.waitNSlots 1
    in  checkPredicateOptions
            options
            "Validation pass when mustProduceAtLeast is greater than script's balance and other wallet's paymentPubKeyHash is used"
            (assertValidatedTransactionCount 2)
            (void traceWithW6Signing)
-}

-- Contract error is thrown when there are not enough tokens at the script and own wallet to satisfy mostProduceAtLeast constraint
contractErrorWhenUnableToSpendMoreThanScriptTokenBalance :: TestTree
contractErrorWhenUnableToSpendMoreThanScriptTokenBalance =
    let offAmt = baseAdaValueLockedByScript <> someTokens 2
        onAmt  = baseAdaValueLockedByScript <> someTokens 2
        contract = mustProduceAtLeastContract offAmt onAmt baseAdaAndTokenValueLockedByScript $ mockWalletPaymentPubKeyHash w1
        options = defaultCheckOptions
            & changeInitialWalletValue w1 (someTokens 1 <>)
    in  checkPredicateOptions
        options
        "Fail validation when there are not enough tokens to satisfy mustProduceAtLeast constraint"
        (assertContractError contract (Trace.walletInstanceTag w1) (\case { WalletContractError (InsufficientFunds _)-> True; _ -> False }) "failed to throw error"
        .&&. assertValidatedTransactionCount 1)
        (void $ trace contract)

-- Contract error is thrown when using mostProduceAtLeast constraint and the ownPaymentPubKeyHash lookup is missing from the contract
contractErrorWhenOwnPaymentPubKeyHashLookupIsMissing :: TestTree
contractErrorWhenOwnPaymentPubKeyHashLookupIsMissing =
    let withoutOwnPubkeyHashLookupContract:: Value.Value -> Value.Value -> Contract () Empty ContractError ()
        withoutOwnPubkeyHashLookupContract offAmt onAmt = do
            let lookups1 = Constraints.typedValidatorLookups typedValidator
                tx1 = Constraints.mustPayToTheScript onAmt baseAdaValueLockedByScript
            ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

            utxos <- utxosAt scrAddress
            let lookups2 = Constraints.typedValidatorLookups typedValidator
                    <> Constraints.unspentOutputs utxos
                tx2 =
                    Constraints.collectFromTheScript utxos ()
                    <> Constraints.mustIncludeDatumInTx (Datum $ PlutusTx.toBuiltinData onAmt)
                    <> Constraints.mustProduceAtLeast offAmt
            ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

        amt = baseAdaValueLockedByScript
        contract = withoutOwnPubkeyHashLookupContract amt amt
    in  checkPredicateOptions
            defaultCheckOptions
            "Fail validation when mustProduceAtLeast is greater than script's balance and wallet's pubkey is not in the lookup"
            (assertContractError contract (Trace.walletInstanceTag w1) (\case { ConstraintResolutionContractError Constraints.OwnPubKeyMissing -> True; _ -> False }) "failed to throw error"
            .&&. assertValidatedTransactionCount 1)
            (void $ trace contract)

-- Uses onchain and offchain constraint mustProduceAtLeast with a higher expected ada value onchain, asserts script evaluation error.
phase2FailureWhenProducedAdaAmountIsNotSatisfied :: TestTree
phase2FailureWhenProducedAdaAmountIsNotSatisfied =
    let w1StartingBalance = 100_000_000
        offAmt = baseAdaValueLockedByScript
        onAmt  = Ada.lovelaceValueOf (baseLovelaceLockedByScript + w1StartingBalance) -- fees make this impossible to satisfy onchain
        contract = mustProduceAtLeastContract offAmt onAmt baseAdaValueLockedByScript $ mockWalletPaymentPubKeyHash w1
        options = defaultCheckOptions
            & changeInitialWalletValue w1 (const $ Ada.lovelaceValueOf w1StartingBalance)
    in  checkPredicateOptions
        options
        "Fail phase-2 validation when on-chain mustProduceAtLeast is greater than script's ada balance"
        (assertFailedTransaction (\_ err -> case err of {Ledger.ScriptFailure (EvaluationError ("L6":_) _) -> True; _ -> False }))
        (void $ trace contract)

-- Uses onchain and offchain constraint mustProduceAtLeast with a higher expected token value onchain, asserts script evaluation error.
phase2FailureWhenProducedTokenAmountIsNotSatisfied :: TestTree
phase2FailureWhenProducedTokenAmountIsNotSatisfied =
    let offAmt = baseAdaValueLockedByScript <> someTokens 1
        onAmt  = baseAdaValueLockedByScript <> someTokens 2
        contract = mustProduceAtLeastContract offAmt onAmt baseAdaAndTokenValueLockedByScript $ mockWalletPaymentPubKeyHash w1
        options = defaultCheckOptions
            & changeInitialWalletValue w1 (someTokens 1 <>)
    in  checkPredicateOptions
        options
        "Fail phase-2 validation when on-chain mustProduceAtLeast is greater than script's token balance"
        (assertFailedTransaction (\_ err -> case err of {Ledger.ScriptFailure (EvaluationError ("L6":_) _) -> True; _ -> False }))
        (void $ trace contract)

{-# INLINEABLE mkValidator #-}
mkValidator :: Value.Value -> () -> ScriptContext -> Bool
mkValidator v _ ctx = P.traceIfFalse "mustProduceAtLeast not satisfied" (Constraints.checkScriptContext @() @() (Constraints.mustProduceAtLeast v) ctx)

data UnitTest
instance Scripts.ValidatorTypes UnitTest where
    type instance DatumType UnitTest = Value.Value
    type instance RedeemerType UnitTest = ()

typedValidator :: Scripts.TypedValidator UnitTest
typedValidator = Scripts.mkTypedValidator @UnitTest
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

valHash :: ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = Ledger.scriptHashAddress valHash
