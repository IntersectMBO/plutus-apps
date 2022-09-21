{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.TxConstraints.MustSpendPubKeyOutput(tests) where

import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Control.Lens ((^.))
import Data.Map qualified as M (elems)
import Data.Set (Set)
import Data.Set qualified as S (elemAt, elems)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.CardanoWallet (paymentPrivateKey)
import Ledger.Constraints.OffChain qualified as Constraints (MkTxError (TxOutRefNotFound), ownPaymentPubKeyHash,
                                                             plutusV1TypedValidatorLookups, unspentOutputs)
import Ledger.Constraints.OnChain.V1 qualified as Constraints (checkScriptContext)
import Ledger.Constraints.TxConstraints qualified as Constraints (collectFromTheScript, mustIncludeDatum,
                                                                  mustPayToTheScript, mustSpendPubKeyOutput)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.ChainIndex.Emulator (addressMap, diskState, unCredentialMap)
import Plutus.Contract as Con
import Plutus.Contract.Test (assertContractError, assertFailedTransaction, assertValidatedTransactionCount,
                             checkPredicate, mockWalletPaymentPubKeyHash, w1, w2, walletFundsChange, (.&&.))
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (Datum (Datum), ScriptContext, TxOutRef (TxOutRef, txOutRefIdx), Validator, ValidatorHash)
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError))
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Wallet.Emulator.Wallet (WalletState, chainIndexEmulatorState, signPrivateKeys, walletToMockWallet')

tests :: TestTree
tests =
    testGroup "MustSpendPubKeyOutput"
        [ mustSpendSingleUtxoFromOwnWallet
        , mustSpendRemainingInitialUtxosFromOwnWallet
        , mustSpendSingleUtxoFromOtherWallet
        , mustSpendAllUtxosFromOtherWallet
        , contractErrorWhenAttemptingToSpendNonExistentOutput

        -- TODO: uncomment after enabling 2nd phase validation
        -- See note [Second phase validation]
        -- , phase2FailureWhenTxoIsNotSpent
        ]

nonExistentTxoRef :: TxOutRef
nonExistentTxoRef = TxOutRef "abcd" 123

w1PaymentPubKeyHash :: Ledger.PaymentPubKeyHash
w1PaymentPubKeyHash = mockWalletPaymentPubKeyHash w1

w2PaymentPubKeyHash :: Ledger.PaymentPubKeyHash
w2PaymentPubKeyHash = mockWalletPaymentPubKeyHash w2

initialLovelacePerWallet :: Integer
initialLovelacePerWallet = 100_000_000

lovelacePerInitialUtxo :: Integer
lovelacePerInitialUtxo = initialLovelacePerWallet `div` 10

-- wallet starts with 10 utxos of 10 ada by default, this amount paid to script spends 1 utxo.
baseLovelaceLockedByScript :: Integer
baseLovelaceLockedByScript = lovelacePerInitialUtxo `div` 2

mustSpendPubKeyOutputContract :: [TxOutRef] -> [TxOutRef] -> Ledger.PaymentPubKeyHash -> Contract () Empty ContractError ()
mustSpendPubKeyOutputContract offChainTxOutRefs onChainTxOutRefs pkh = do
    let lookups1 = Constraints.plutusV1TypedValidatorLookups typedValidator
        tx1 = Constraints.mustPayToTheScript onChainTxOutRefs $ Ada.lovelaceValueOf baseLovelaceLockedByScript
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    pubKeyUtxos <- utxosAt $ Ledger.pubKeyHashAddress pkh Nothing
    logInfo @String $ "pubKeyUtxos:: " ++ show pubKeyUtxos -- remove
    scriptUtxos <- utxosAt scrAddress
    let lookups2 = Constraints.plutusV1TypedValidatorLookups typedValidator
            <> Constraints.unspentOutputs pubKeyUtxos
            <> Constraints.unspentOutputs scriptUtxos
            <> Constraints.ownPaymentPubKeyHash pkh
        tx2 =
            Constraints.collectFromTheScript scriptUtxos ()
            <> Constraints.mustIncludeDatum (Datum $ PlutusTx.toBuiltinData onChainTxOutRefs)
            <> mconcat mustSpendPubKeyOutputs
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

    where
        mustSpendPubKeyOutputs = fmap Constraints.mustSpendPubKeyOutput offChainTxOutRefs

txoRefsFromWalletState :: WalletState -> Set TxOutRef
txoRefsFromWalletState ws = head $ M.elems $ ws ^. chainIndexEmulatorState . diskState . addressMap . unCredentialMap

-- needed to workaround bug 695
overrideW1TxOutRefs :: [TxOutRef] -> [TxOutRef]
overrideW1TxOutRefs = overrideTxOutRefIdxes 50

overrideW2TxOutRefs :: [TxOutRef] -> [TxOutRef]
overrideW2TxOutRefs = overrideTxOutRefIdxes 20

overrideTxOutRefIdxes :: Integer -> [TxOutRef] -> [TxOutRef]
overrideTxOutRefIdxes i = fmap (\r@TxOutRef{txOutRefIdx=idx} -> r{txOutRefIdx= idx + i})
--

{-
-- Example of bug https://github.com/input-output-hk/plutus-apps/issues/695: fails with TxOutRefNotFound because w1 does not have utxo with index of 5 from WalletState
bug695 :: TestTree
bug695 =
    let trace = do
            w1State <- Trace.agentState w1
            let w1TxoRefs = txoRefsFromWalletState w1State
                w1MiddleTxoRef = [S.elemAt (length w1TxoRefs `div` 2) w1TxoRefs]
            void $ Trace.activateContractWallet w1 $ mustSpendPubKeyOutputContract w1MiddleTxoRef w1MiddleTxoRef w1PaymentPubKeyHash
            void $ Trace.waitNSlots 1

    in checkPredicate "Example of bug 695"
        (assertValidatedTransactionCount 2 .&&. walletFundsChange w1 mempty)
        (void trace)
-}

{-
-- Example of bug https://github.com/input-output-hk/plutus-apps/issues/696
bug696 :: TestTree
bug696 =
    let trace = do
            thisChainState <- Trace.chainState
            let traceBlockchain = thisChainState ^. chainNewestFirst
                traceEmulatorState = emulatorState traceBlockchain
                walletStateMap = traceEmulatorState ^. walletStates
                w1State = fromJust $ M.lookup w1 walletStateMap -- Fails here: Maybe.fromJust: Nothing

                w1TxoRefs = txoRefsFromWalletState w1State
                w1MiddleTxoRef = [S.elemAt (length w1TxoRefs `div` 2) w1TxoRefs]
            void $ Trace.activateContractWallet w1 $ mustSpendPubKeyOutputContract w1MiddleTxoRef w1MiddleTxoRef w1PaymentPubKeyHash
            void $ Trace.waitNSlots 1

    in checkPredicate "Example of bug 696"
        (assertValidatedTransactionCount 2 .&&. walletFundsChange w1 mempty)
        (void trace)
-}

-- | Uses onchain and offchain constraint mustSpendPubKeyOutput to spend a single utxo from own wallet
mustSpendSingleUtxoFromOwnWallet :: TestTree
mustSpendSingleUtxoFromOwnWallet =
    let trace = do
            w1State <- Trace.agentState w1
            let w1TxoRefs = txoRefsFromWalletState w1State
                w1MiddleTxoRef = [S.elemAt (length w1TxoRefs `div` 2) w1TxoRefs]
                overridedW1TxoRefs = overrideW1TxOutRefs w1MiddleTxoRef -- need to override index due to bug 695
            void $ Trace.activateContractWallet w1 $ mustSpendPubKeyOutputContract overridedW1TxoRefs overridedW1TxoRefs w1PaymentPubKeyHash
            void $ Trace.waitNSlots 1

    in checkPredicate "Successful use of mustSpendPubKeyOutput with a single txOutRef from own wallet"
        (assertValidatedTransactionCount 2 .&&. walletFundsChange w1 mempty)
        (void trace)

-- | Uses onchain and offchain constraint mustSpendPubKeyOutput to spend the remaining utxos that were initially distributed to own wallet
mustSpendRemainingInitialUtxosFromOwnWallet :: TestTree
mustSpendRemainingInitialUtxosFromOwnWallet =
    let trace = do
            w1State <- Trace.agentState w1
            let w1TxoRefs = txoRefsFromWalletState w1State
                w1RemainingTxoRefs = tail $ S.elems w1TxoRefs
                overridedW1TxoRefs = overrideW1TxOutRefs w1RemainingTxoRefs -- need to override index due to bug 695
            void $ Trace.activateContractWallet w1 $ mustSpendPubKeyOutputContract overridedW1TxoRefs overridedW1TxoRefs w1PaymentPubKeyHash
            void $ Trace.waitNSlots 1

    in checkPredicate "Successful use of mustSpendPubKeyOutput with all remaining initial txOutRefs from own wallet"
        (assertValidatedTransactionCount 2 .&&. walletFundsChange w1 mempty)
        (void trace)

-- | Uses onchain and offchain constraint mustSpendPubKeyOutput to spend a single utxo from other wallet
mustSpendSingleUtxoFromOtherWallet :: TestTree
mustSpendSingleUtxoFromOtherWallet =
    let trace = do
            w2State <- Trace.agentState w2
            let w2TxoRefs = txoRefsFromWalletState w2State
                w2MiddleTxoRef = [S.elemAt (length w2TxoRefs `div` 2) w2TxoRefs]
                overridedW2TxoRefs = overrideW2TxOutRefs w2MiddleTxoRef -- need to override index due to bug 695
            Trace.setSigningProcess w1 (Just $ signPrivateKeys [paymentPrivateKey $ walletToMockWallet' w1, paymentPrivateKey $ walletToMockWallet' w2])
            void $ Trace.activateContractWallet w1 $ mustSpendPubKeyOutputContract overridedW2TxoRefs overridedW2TxoRefs w2PaymentPubKeyHash
            void $ Trace.waitNSlots 1

    in checkPredicate "Successful use of mustSpendPubKeyOutput with a single txOutRef from other wallet"
        (assertValidatedTransactionCount 2 .&&. walletFundsChange w2 (Ada.lovelaceValueOf $ negate lovelacePerInitialUtxo))
        (void trace)

-- | Uses onchain and offchain constraint mustSpendPubKeyOutput to spend all utxos from other wallet
mustSpendAllUtxosFromOtherWallet :: TestTree
mustSpendAllUtxosFromOtherWallet =
    let trace = do
            w2State <- Trace.agentState w2
            let w2TxoRefs = txoRefsFromWalletState w2State
                allW2TxoRefs = S.elems w2TxoRefs
                overridedW2TxoRefs = overrideW2TxOutRefs allW2TxoRefs -- need to override index due to bug 695
            Trace.setSigningProcess w1 (Just $ signPrivateKeys [paymentPrivateKey $ walletToMockWallet' w1, paymentPrivateKey $ walletToMockWallet' w2])
            void $ Trace.activateContractWallet w1 $ mustSpendPubKeyOutputContract overridedW2TxoRefs overridedW2TxoRefs w2PaymentPubKeyHash
            void $ Trace.waitNSlots 1

    in checkPredicate "Successful use of mustSpendPubKeyOutput with all initial txOutRefs from other wallet"
    (assertValidatedTransactionCount 2 .&&. walletFundsChange w2 (Ada.lovelaceValueOf $ negate initialLovelacePerWallet))
    (void trace)

-- Contract error is thrown when mustSpendPubKeyOutput is expecting a txo that does not exist
contractErrorWhenAttemptingToSpendNonExistentOutput :: TestTree
contractErrorWhenAttemptingToSpendNonExistentOutput =
    let contract = mustSpendPubKeyOutputContract [nonExistentTxoRef] [nonExistentTxoRef] w1PaymentPubKeyHash
        trace = do
            void $ Trace.activateContractWallet w1 contract
            void $ Trace.waitNSlots 1

    in checkPredicate "Fail validation when mustSpendPubKeyOutput constraint expects a non-existing txo"
        (assertContractError contract (Trace.walletInstanceTag w1) (\case { ConstraintResolutionContractError ( Constraints.TxOutRefNotFound txoRefInError) -> txoRefInError == nonExistentTxoRef; _ -> False }) "failed to throw error"
        .&&. assertValidatedTransactionCount 1)
        (void trace)

-- Uses onchain and offchain constraint mustSpendPubKeyOutput with a different expected txo onchain, asserts script evaluation error.
phase2FailureWhenTxoIsNotSpent :: TestTree
phase2FailureWhenTxoIsNotSpent =
    let trace = do
            w1State <- Trace.agentState w1
            let w1TxoRefs = txoRefsFromWalletState w1State
                w1MiddleTxoRef = [S.elemAt (length w1TxoRefs `div` 2) w1TxoRefs]
                overridedW1TxoRefs = overrideW1TxOutRefs w1MiddleTxoRef -- need to override index due to bug 695
            void $ Trace.activateContractWallet w1 $ mustSpendPubKeyOutputContract overridedW1TxoRefs [nonExistentTxoRef] w1PaymentPubKeyHash
            void $ Trace.waitNSlots 1

    in checkPredicate "Fail phase-2 validation when txo expected by on-chain mustSpendPubKeyOutput does not exist"
        (assertFailedTransaction (\_ err -> case err of {Ledger.ScriptFailure (EvaluationError ("L7":_) _) -> True; _ -> False }))
        (void trace)

{-# INLINEABLE mkValidator #-}
mkValidator :: [TxOutRef] -> () -> ScriptContext -> Bool
mkValidator txOutRefs _ ctx = P.traceIfFalse "mustSpendPubKeyOutput not satisfied" (Constraints.checkScriptContext @() @() (P.mconcat mustSpendPubKeyOutputs) ctx)
    where
        mustSpendPubKeyOutputs = P.fmap Constraints.mustSpendPubKeyOutput txOutRefs

data UnitTest
instance Scripts.ValidatorTypes UnitTest where
    type instance DatumType UnitTest = [TxOutRef]
    type instance RedeemerType UnitTest = ()

typedValidator :: Scripts.TypedValidator UnitTest
typedValidator = Scripts.mkTypedValidator @UnitTest
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

validatorScript :: Validator
validatorScript = Scripts.validatorScript typedValidator

valHash :: ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = Ledger.scriptHashAddress valHash
