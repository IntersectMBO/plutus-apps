{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.Contract.Tx.Constraints.MustProduceAtLeast(tests) where

import Control.Lens (has, makeClassyPrisms, (&))
import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Cardano.Api qualified as C
import Cardano.Node.Emulator.Generators (someTokenValue)
import Cardano.Node.Emulator.Internal.Node.Params qualified as Params
import Ledger qualified
import Ledger.CardanoWallet (paymentPrivateKey)
import Ledger.Tx qualified as Tx
import Ledger.Tx.Constraints.OffChain qualified as Constraints
import Ledger.Tx.Constraints.OnChain.V1 qualified as Constraints
import Ledger.Tx.Constraints.TxConstraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value.CardanoAPI qualified as Value
import Plutus.Contract as Con (Contract, ContractError, Empty, _WalletContractError, awaitTxConfirmed,
                               submitTxConstraintsWith, utxosAt)
import Plutus.Contract.Test (assertContractError, assertEvaluationError, assertValidatedTransactionCount,
                             changeInitialWalletValue, checkPredicateOptions, defaultCheckOptions, mockWalletAddress,
                             w1, w6, (.&&.))
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (Datum (Datum), ScriptContext)
import Plutus.V1.Ledger.Value qualified as Plutus
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Prelude hiding (not)
import Spec.Contract.Error (insufficientFundsError)
import Wallet.Emulator.Error (WalletAPIError)
import Wallet.Emulator.Wallet (signPrivateKeys, walletToMockWallet')

makeClassyPrisms ''WalletAPIError

tests :: TestTree
tests = testGroup "MustProduceAtLeast"
  [ spendAtLeastTheScriptBalance
  , spendLessThanScriptBalance
  , spendTokenBalanceFromScript
  , spendMoreThanScriptBalanceWithOwnWalletAsOwnPubkeyLookup
  , contractErrorWhenSpendMoreThanScriptBalanceWithOtherWalletAsOwnPubkeyLookup
  , contractErrorWhenUnableToSpendMoreThanScriptTokenBalance
  , phase2FailureWhenProducedAdaAmountIsNotSatisfied
  , phase2FailureWhenProducedTokenAmountIsNotSatisfied
  ]

someTokens :: Integer -> C.Value
someTokens = someTokenValue "someToken"

baseLovelaceLockedByScript :: Integer
baseLovelaceLockedByScript = 25_000_000

baseAdaValueLockedByScript :: C.Value
baseAdaValueLockedByScript = Value.lovelaceValueOf baseLovelaceLockedByScript

baseAdaAndTokenValueLockedByScript :: C.Value
baseAdaAndTokenValueLockedByScript = baseAdaValueLockedByScript <> someTokens 1

w1Address :: Ledger.CardanoAddress
w1Address = mockWalletAddress w1

-- | Valid contract containing all required lookups. Uses mustProduceAtLeast constraint with provided on-chain and off-chain values.
mustProduceAtLeastContract :: C.Value -> C.Value -> C.Value -> Ledger.CardanoAddress -> Contract () Empty ContractError ()
mustProduceAtLeastContract offAmt onAmt baseScriptValue addr = do
    let lookups1 = Constraints.typedValidatorLookups typedValidator
        tx1 = Constraints.mustPayToTheScriptWithDatumInTx (Value.fromCardanoValue onAmt) (Value.fromCardanoValue baseScriptValue)
    ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    pubKeyUtxos <- utxosAt addr
    scriptUtxos <- utxosAt scrAddress
    let lookups2 = Constraints.typedValidatorLookups typedValidator
            <> Constraints.unspentOutputs pubKeyUtxos
            <> Constraints.unspentOutputs scriptUtxos
        tx2 =
            Constraints.spendUtxosFromTheScript scriptUtxos ()
            <> Constraints.mustPayToAddressWithDatumInTx
                 (Ledger.toPlutusAddress w1Address)
                 (Datum $ PlutusTx.toBuiltinData $ Value.fromCardanoValue onAmt)
                 (Value.fromCardanoValue offAmt)
            <> Constraints.mustProduceAtLeast (Value.fromCardanoValue offAmt)
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

trace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void Trace.nextSlot

-- | Uses onchain and offchain constraint mustProduceAtLeast to spend entire ada balance locked by the script
spendAtLeastTheScriptBalance :: TestTree
spendAtLeastTheScriptBalance =
    let contract = mustProduceAtLeastContract
                       baseAdaValueLockedByScript
                       baseAdaValueLockedByScript
                       baseAdaValueLockedByScript
                       w1Address
    in checkPredicateOptions
        defaultCheckOptions
        "Successful use of mustProduceAtLeast at script's exact balance"
        (assertValidatedTransactionCount 2)
        (void $ trace contract)

-- | Uses onchain and offchain constraint mustProduceAtLeast to spend less ada than is locked by the script
spendLessThanScriptBalance :: TestTree
spendLessThanScriptBalance =
    let amt = Value.lovelaceValueOf (baseLovelaceLockedByScript - 500)
        contract = mustProduceAtLeastContract amt amt baseAdaValueLockedByScript w1Address
    in checkPredicateOptions
        defaultCheckOptions
        "Successful use of mustProduceAtLeast below script's balance"
        (assertValidatedTransactionCount 2)
        (void $ trace contract )

-- | Uses onchain and offchain constraint mustProduceAtLeast to spend entire token balance locked by the script
spendTokenBalanceFromScript :: TestTree
spendTokenBalanceFromScript =
    let token = someTokens 1
        contract = mustProduceAtLeastContract
                       (baseAdaValueLockedByScript <> token)
                       (baseAdaValueLockedByScript <> token)
                       baseAdaAndTokenValueLockedByScript
                       w1Address
        options = defaultCheckOptions
            & changeInitialWalletValue w1 (token <>)
    in checkPredicateOptions
        options
        "Successful use of mustProduceAtLeast sending tokens"
        (assertValidatedTransactionCount 2)
        (void $ trace contract)

-- | Uses onchain and offchain constraint mustProduceAtLeast to spend more than the ada balance locked by the script, excess is paid by own wallet.
spendMoreThanScriptBalanceWithOwnWalletAsOwnPubkeyLookup :: TestTree
spendMoreThanScriptBalanceWithOwnWalletAsOwnPubkeyLookup =
    let amt = Value.lovelaceValueOf (baseLovelaceLockedByScript + 5_000_000)
        contract = mustProduceAtLeastContract amt amt baseAdaValueLockedByScript w1Address
    in checkPredicateOptions
        defaultCheckOptions
        "Validation pass when mustProduceAtLeast is greater than script's balance"
        (assertValidatedTransactionCount 2)
        (void $ trace contract)

-- | Uses onchain and offchain constraint mustProduceAtLeast to spend more than the ada balance
-- locked by the script and own wallet. Using setSigningProcess doesn't mean that other wallet can
-- be used to pay excess.
contractErrorWhenSpendMoreThanScriptBalanceWithOtherWalletAsOwnPubkeyLookup :: TestTree
contractErrorWhenSpendMoreThanScriptBalanceWithOtherWalletAsOwnPubkeyLookup =
    let amt = Value.lovelaceValueOf (baseLovelaceLockedByScript + 5_000_000)
        contract = mustProduceAtLeastContract amt amt baseAdaValueLockedByScript $ mockWalletAddress w6
        options = defaultCheckOptions
            & changeInitialWalletValue w1 (const amt) -- not enough funds remain for w1 to satisfy constraint
        traceWithW6Signing = do
            Trace.setSigningProcess w1 (Just $ signPrivateKeys [paymentPrivateKey $ walletToMockWallet' w1, paymentPrivateKey $ walletToMockWallet' w6])
            void $ Trace.activateContractWallet w1 contract
            Trace.nextSlot
    in checkPredicateOptions
        options
        "Fail validation when there are not enough ada in own wallet to satisfy mustProduceAtLeast constraint. Other wallet is not used."
        (assertContractError contract (Trace.walletInstanceTag w1) insufficientFundsError "failed to throw error"
        .&&. assertValidatedTransactionCount 1)
        (void traceWithW6Signing)

-- Contract error is thrown when there are not enough tokens at the script and own wallet to satisfy mostProduceAtLeast constraint
contractErrorWhenUnableToSpendMoreThanScriptTokenBalance :: TestTree
contractErrorWhenUnableToSpendMoreThanScriptTokenBalance =
    let offAmt = baseAdaValueLockedByScript <> someTokens 2
        onAmt  = baseAdaValueLockedByScript <> someTokens 2
        contract = mustProduceAtLeastContract offAmt onAmt baseAdaAndTokenValueLockedByScript w1Address
        options = defaultCheckOptions
            & changeInitialWalletValue w1 (someTokens 1 <>)
    in checkPredicateOptions
        options
        "Fail validation when there are not enough tokens to satisfy mustProduceAtLeast constraint"
        (assertContractError contract (Trace.walletInstanceTag w1) (has (_WalletContractError . _InsufficientFunds)) "failed to throw error"
        .&&. assertValidatedTransactionCount 1)
        (void $ trace contract)

-- Uses onchain and offchain constraint mustProduceAtLeast with a higher expected ada value onchain, asserts script evaluation error.
phase2FailureWhenProducedAdaAmountIsNotSatisfied :: TestTree
phase2FailureWhenProducedAdaAmountIsNotSatisfied =
    let w1StartingBalance = 100_000_000
        offAmt = baseAdaValueLockedByScript
        onAmt  = Value.lovelaceValueOf (baseLovelaceLockedByScript + w1StartingBalance) -- fees make this impossible to satisfy onchain
        contract = mustProduceAtLeastContract offAmt onAmt baseAdaValueLockedByScript w1Address
        options = defaultCheckOptions
            & changeInitialWalletValue w1 (const $ Value.lovelaceValueOf w1StartingBalance)
    in checkPredicateOptions
        options
        "Fail phase-2 validation when on-chain mustProduceAtLeast is greater than script's ada balance"
        (assertEvaluationError "L6")
        (void $ trace contract)

-- Uses onchain and offchain constraint mustProduceAtLeast with a higher expected token value onchain, asserts script evaluation error.
phase2FailureWhenProducedTokenAmountIsNotSatisfied :: TestTree
phase2FailureWhenProducedTokenAmountIsNotSatisfied =
    let offAmt = baseAdaValueLockedByScript <> someTokens 1
        onAmt  = baseAdaValueLockedByScript <> someTokens 2
        contract = mustProduceAtLeastContract offAmt onAmt baseAdaAndTokenValueLockedByScript w1Address
        options = defaultCheckOptions
            & changeInitialWalletValue w1 (someTokens 1 <>)
    in checkPredicateOptions
        options
        "Fail phase-2 validation when on-chain mustProduceAtLeast is greater than script's token balance"
        (assertEvaluationError "L6")
        (void $ trace contract)

{-# INLINEABLE mkValidator #-}
mkValidator :: Plutus.Value -> () -> ScriptContext -> Bool
mkValidator v _ ctx = P.traceIfFalse "mustProduceAtLeast not satisfied" (Constraints.checkScriptContext @() @() (Constraints.mustProduceAtLeast v) ctx)

data UnitTest
instance Scripts.ValidatorTypes UnitTest where
    type instance DatumType UnitTest = Plutus.Value
    type instance RedeemerType UnitTest = ()

typedValidator :: Scripts.TypedValidator UnitTest
typedValidator = Scripts.mkTypedValidator @UnitTest
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

scrAddress :: Ledger.CardanoAddress
scrAddress = Scripts.validatorCardanoAddress Params.testnet typedValidator
