{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.Contract.Tx.Constraints.MustProduceAtLeast(tests) where

import Control.Lens (review, (&), (??), (^.))
import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Cardano.Api qualified as C
import Cardano.Node.Emulator.Generators (someTokenValue)
import Cardano.Node.Emulator.Params qualified as Params
import Ledger qualified
import Ledger.CardanoWallet (paymentPrivateKey)
import Ledger.Constraints.OffChain qualified as Constraints
import Ledger.Constraints.OnChain.V1 qualified as Constraints
import Ledger.Constraints.TxConstraints qualified as Constraints
import Ledger.Tx qualified as Tx
import Ledger.Tx.Constraints qualified as Tx.Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value.CardanoAPI qualified as Value
import Plutus.Contract (mapError, throwError)
import Plutus.Contract as Con (Contract, ContractError (WalletContractError), Empty, awaitTxConfirmed,
                               submitTxConstraintsWith, submitUnbalancedTx, utxosAt)
import Plutus.Contract.Error (AsContractError (_TxConstraintResolutionContractError))
import Plutus.Contract.Test (assertContractError, assertEvaluationError, assertValidatedTransactionCount,
                             changeInitialWalletValue, checkPredicateOptions, defaultCheckOptions, emulatorConfig,
                             mockWalletAddress, w1, w6, (.&&.))
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (Datum (Datum), ScriptContext)
import Plutus.V1.Ledger.Value qualified as Plutus
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Prelude hiding (not)
import Spec.Contract.Error (insufficientFundsError)
import Wallet.Emulator.Error (WalletAPIError (InsufficientFunds))
import Wallet.Emulator.Wallet (signPrivateKeys, walletToMockWallet')

--
-- TODO include these tests to the main suite in the test-suite when we'll be able to validate the contract
tests :: TestTree
tests =
    testGroup "MustProduceAtLeast"
    [ testGroup "ledger constraints" $ tests' ledgerSubmitTx
    , testGroup "cardano constraints" $ tests' cardanoSubmitTx
    ]

tests' :: SubmitTx -> [TestTree]
tests' sub =
  [ spendAtLeastTheScriptBalance
  , spendLessThanScriptBalance
  , spendTokenBalanceFromScript
  , spendMoreThanScriptBalanceWithOwnWalletAsOwnPubkeyLookup
  , contractErrorWhenSpendMoreThanScriptBalanceWithOtherWalletAsOwnPubkeyLookup
  , contractErrorWhenUnableToSpendMoreThanScriptTokenBalance
  , phase2FailureWhenProducedAdaAmountIsNotSatisfied
  , phase2FailureWhenProducedTokenAmountIsNotSatisfied
  ] ?? sub

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
mustProduceAtLeastContract :: SubmitTx -> C.Value -> C.Value -> C.Value -> Ledger.CardanoAddress -> Contract () Empty ContractError ()
mustProduceAtLeastContract submitTxFromConstraints offAmt onAmt baseScriptValue addr = do
    let lookups1 = Constraints.typedValidatorLookups typedValidator
        tx1 = Constraints.mustPayToTheScriptWithDatumInTx (Value.fromCardanoValue onAmt) (Value.fromCardanoValue baseScriptValue)
    ledgerTx1 <- submitTxFromConstraints lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    pubKeyUtxos <- utxosAt addr
    scriptUtxos <- utxosAt scrAddress
    let lookups2 = Constraints.typedValidatorLookups typedValidator
            <> Constraints.unspentOutputs pubKeyUtxos
            <> Constraints.unspentOutputs scriptUtxos
        tx2 =
            Constraints.collectFromTheScript scriptUtxos ()
            <> Constraints.mustPayToAddressWithDatumInTx
                 (Ledger.toPlutusAddress w1Address)
                 (Datum $ PlutusTx.toBuiltinData $ Value.fromCardanoValue onAmt)
                 (Value.fromCardanoValue offAmt)
            <> Constraints.mustProduceAtLeast (Value.fromCardanoValue offAmt)
    ledgerTx2 <- submitTxFromConstraints lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

trace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void Trace.nextSlot

-- | Uses onchain and offchain constraint mustProduceAtLeast to spend entire ada balance locked by the script
spendAtLeastTheScriptBalance :: SubmitTx -> TestTree
spendAtLeastTheScriptBalance sub =
    let contract = mustProduceAtLeastContract sub
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
spendLessThanScriptBalance :: SubmitTx -> TestTree
spendLessThanScriptBalance sub =
    let amt = Value.lovelaceValueOf (baseLovelaceLockedByScript - 500)
        contract = mustProduceAtLeastContract sub amt amt baseAdaValueLockedByScript w1Address
    in checkPredicateOptions
        defaultCheckOptions
        "Successful use of mustProduceAtLeast below script's balance"
        (assertValidatedTransactionCount 2)
        (void $ trace contract )

-- | Uses onchain and offchain constraint mustProduceAtLeast to spend entire token balance locked by the script
spendTokenBalanceFromScript :: SubmitTx -> TestTree
spendTokenBalanceFromScript sub =
    let token = someTokens 1
        contract = mustProduceAtLeastContract sub
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
spendMoreThanScriptBalanceWithOwnWalletAsOwnPubkeyLookup :: SubmitTx -> TestTree
spendMoreThanScriptBalanceWithOwnWalletAsOwnPubkeyLookup sub =
    let amt = Value.lovelaceValueOf (baseLovelaceLockedByScript + 5_000_000)
        contract = mustProduceAtLeastContract sub amt amt baseAdaValueLockedByScript w1Address
    in checkPredicateOptions
        defaultCheckOptions
        "Validation pass when mustProduceAtLeast is greater than script's balance"
        (assertValidatedTransactionCount 2)
        (void $ trace contract)

-- | Uses onchain and offchain constraint mustProduceAtLeast to spend more than the ada balance
-- locked by the script and own wallet. Using setSigningProcess doesn't mean that other wallet can
-- be used to pay excess.
contractErrorWhenSpendMoreThanScriptBalanceWithOtherWalletAsOwnPubkeyLookup :: SubmitTx -> TestTree
contractErrorWhenSpendMoreThanScriptBalanceWithOtherWalletAsOwnPubkeyLookup sub =
    let amt = Value.lovelaceValueOf (baseLovelaceLockedByScript + 5_000_000)
        contract = mustProduceAtLeastContract sub amt amt baseAdaValueLockedByScript $ mockWalletAddress w6
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
contractErrorWhenUnableToSpendMoreThanScriptTokenBalance :: SubmitTx -> TestTree
contractErrorWhenUnableToSpendMoreThanScriptTokenBalance sub =
    let offAmt = baseAdaValueLockedByScript <> someTokens 2
        onAmt  = baseAdaValueLockedByScript <> someTokens 2
        contract = mustProduceAtLeastContract sub offAmt onAmt baseAdaAndTokenValueLockedByScript w1Address
        options = defaultCheckOptions
            & changeInitialWalletValue w1 (someTokens 1 <>)
    in checkPredicateOptions
        options
        "Fail validation when there are not enough tokens to satisfy mustProduceAtLeast constraint"
        (assertContractError contract (Trace.walletInstanceTag w1) (\case { WalletContractError (InsufficientFunds _)-> True; _ -> False }) "failed to throw error"
        .&&. assertValidatedTransactionCount 1)
        (void $ trace contract)

-- Uses onchain and offchain constraint mustProduceAtLeast with a higher expected ada value onchain, asserts script evaluation error.
phase2FailureWhenProducedAdaAmountIsNotSatisfied :: SubmitTx -> TestTree
phase2FailureWhenProducedAdaAmountIsNotSatisfied sub =
    let w1StartingBalance = 100_000_000
        offAmt = baseAdaValueLockedByScript
        onAmt  = Value.lovelaceValueOf (baseLovelaceLockedByScript + w1StartingBalance) -- fees make this impossible to satisfy onchain
        contract = mustProduceAtLeastContract sub offAmt onAmt baseAdaValueLockedByScript w1Address
        options = defaultCheckOptions
            & changeInitialWalletValue w1 (const $ Value.lovelaceValueOf w1StartingBalance)
    in checkPredicateOptions
        options
        "Fail phase-2 validation when on-chain mustProduceAtLeast is greater than script's ada balance"
        (assertEvaluationError "L6")
        (void $ trace contract)

-- Uses onchain and offchain constraint mustProduceAtLeast with a higher expected token value onchain, asserts script evaluation error.
phase2FailureWhenProducedTokenAmountIsNotSatisfied :: SubmitTx -> TestTree
phase2FailureWhenProducedTokenAmountIsNotSatisfied sub =
    let offAmt = baseAdaValueLockedByScript <> someTokens 1
        onAmt  = baseAdaValueLockedByScript <> someTokens 2
        contract = mustProduceAtLeastContract sub offAmt onAmt baseAdaAndTokenValueLockedByScript w1Address
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

type SubmitTx
  =  Constraints.ScriptLookups UnitTest
  -> Constraints.TxConstraints (Scripts.RedeemerType UnitTest) (Scripts.DatumType UnitTest)
  -> Contract () Empty ContractError Tx.CardanoTx

cardanoSubmitTx :: SubmitTx
cardanoSubmitTx lookups tx = let
  p = defaultCheckOptions ^. emulatorConfig . Trace.params
  tx' = Tx.Constraints.mkTx @UnitTest p lookups tx
  in submitUnbalancedTx =<<
    (mapError (review _TxConstraintResolutionContractError) $ either throwError pure tx')

ledgerSubmitTx :: SubmitTx
ledgerSubmitTx = submitTxConstraintsWith
