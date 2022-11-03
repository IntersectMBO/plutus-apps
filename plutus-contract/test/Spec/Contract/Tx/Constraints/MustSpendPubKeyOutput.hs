{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.Contract.Tx.Constraints.MustSpendPubKeyOutput(tests) where

import Control.Lens (at, non, (^.))
import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Data.Set (Set)
import Data.Set qualified as S (elemAt, elems)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.CardanoWallet (paymentPrivateKey)
import Ledger.Constraints.OffChain qualified as Constraints (MkTxError (TxOutRefNotFound), ownPaymentPubKeyHash,
                                                             typedValidatorLookups, unspentOutputs)
import Ledger.Constraints.OnChain.V1 qualified as Constraints (checkScriptContext)
import Ledger.Constraints.TxConstraints qualified as Constraints (collectFromTheScript, mustBeSignedBy,
                                                                  mustIncludeDatumInTx, mustPayToTheScriptWithDatumInTx,
                                                                  mustSpendPubKeyOutput)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.ChainIndex.Emulator (addressMap, diskState, unCredentialMap)
import Plutus.Contract as Con
import Plutus.Contract.Test (assertContractError, assertFailedTransaction, assertValidatedTransactionCount,
                             checkPredicate, mockWalletPaymentPubKeyHash, w1, w2, walletFundsChange, (.&&.))
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (Address (addressCredential), Datum (Datum), ScriptContext, TxOutRef (TxOutRef), Validator,
                             ValidatorHash)
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError))
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Wallet.Emulator.Wallet as Wallet (WalletState, chainIndexEmulatorState, ownAddress, signPrivateKeys,
                                         walletToMockWallet')

tests :: TestTree
tests =
    testGroup "MustSpendPubKeyOutput"
        [ mustSpendSingleUtxoFromOwnWallet
        , mustSpendRemainingInitialUtxosFromOwnWallet
        , mustSpendSingleUtxoFromOtherWallet
        , mustSpendAllUtxosFromOtherWallet
        , contractErrorWhenAttemptingToSpendNonExistentOutput
        , phase2FailureWhenTxoIsNotSpent
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
mustSpendPubKeyOutputContract = mustSpendPubKeyOutputContract' []

mustSpendPubKeyOutputContract' :: [Ledger.PaymentPubKeyHash] -> [TxOutRef] -> [TxOutRef] -> Ledger.PaymentPubKeyHash -> Contract () Empty ContractError ()
mustSpendPubKeyOutputContract' keys offChainTxOutRefs onChainTxOutRefs pkh = do
    let lookups1 = Constraints.typedValidatorLookups typedValidator
        tx1 = Constraints.mustPayToTheScriptWithDatumInTx onChainTxOutRefs (Ada.lovelaceValueOf baseLovelaceLockedByScript)
            <> foldMap Constraints.mustBeSignedBy keys
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    pubKeyUtxos <- utxosAt $ Ledger.pubKeyHashAddress pkh Nothing
    logInfo @String $ "pubKeyUtxos:: " ++ show pubKeyUtxos -- remove
    scriptUtxos <- utxosAt scrAddress
    let lookups2 = Constraints.typedValidatorLookups typedValidator
            <> Constraints.unspentOutputs pubKeyUtxos
            <> Constraints.unspentOutputs scriptUtxos
            <> Constraints.ownPaymentPubKeyHash pkh
        tx2 =
            Constraints.collectFromTheScript scriptUtxos ()
            <> Constraints.mustIncludeDatumInTx (Datum $ PlutusTx.toBuiltinData onChainTxOutRefs)
            <> mconcat mustSpendPubKeyOutputs
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

    where
        mustSpendPubKeyOutputs = Constraints.mustSpendPubKeyOutput <$> offChainTxOutRefs

txoRefsFromWalletState :: WalletState -> Set TxOutRef
txoRefsFromWalletState w = let
  pkCred = addressCredential $ Wallet.ownAddress w
  in w ^. chainIndexEmulatorState . diskState . addressMap . unCredentialMap . at pkCred . non mempty


-- | Uses onchain and offchain constraint mustSpendPubKeyOutput to spend a single utxo from own wallet
mustSpendSingleUtxoFromOwnWallet :: TestTree
mustSpendSingleUtxoFromOwnWallet =
    let trace = do
            w1State <- Trace.agentState w1
            let w1TxoRefs = txoRefsFromWalletState w1State
                w1MiddleTxoRef = [S.elemAt (length w1TxoRefs `div` 2) w1TxoRefs]
            void $ Trace.activateContractWallet w1 $ mustSpendPubKeyOutputContract w1MiddleTxoRef w1MiddleTxoRef w1PaymentPubKeyHash
            void Trace.nextSlot

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
            void $ Trace.activateContractWallet w1 $ mustSpendPubKeyOutputContract w1RemainingTxoRefs w1RemainingTxoRefs w1PaymentPubKeyHash
            void Trace.nextSlot

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
            Trace.setSigningProcess w1 (Just $ signPrivateKeys [paymentPrivateKey $ walletToMockWallet' w1, paymentPrivateKey $ walletToMockWallet' w2])
            void $ Trace.activateContractWallet w1 $ mustSpendPubKeyOutputContract' [mockWalletPaymentPubKeyHash w2] w2MiddleTxoRef w2MiddleTxoRef w2PaymentPubKeyHash
            void Trace.nextSlot

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
            Trace.setSigningProcess w1 (Just $ signPrivateKeys [paymentPrivateKey $ walletToMockWallet' w1, paymentPrivateKey $ walletToMockWallet' w2])
            void $ Trace.activateContractWallet w1 $ mustSpendPubKeyOutputContract' [mockWalletPaymentPubKeyHash w2] allW2TxoRefs allW2TxoRefs w2PaymentPubKeyHash
            void Trace.nextSlot

    in checkPredicate "Successful use of mustSpendPubKeyOutput with all initial txOutRefs from other wallet"
    (assertValidatedTransactionCount 2 .&&. walletFundsChange w2 (Ada.lovelaceValueOf $ negate initialLovelacePerWallet))
    (void trace)

-- Contract error is thrown when mustSpendPubKeyOutput is expecting a txo that does not exist
contractErrorWhenAttemptingToSpendNonExistentOutput :: TestTree
contractErrorWhenAttemptingToSpendNonExistentOutput =
    let contract = mustSpendPubKeyOutputContract [nonExistentTxoRef] [nonExistentTxoRef] w1PaymentPubKeyHash
        trace = do
            void $ Trace.activateContractWallet w1 contract
            void Trace.nextSlot

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
            void $ Trace.activateContractWallet w1 $ mustSpendPubKeyOutputContract w1MiddleTxoRef [nonExistentTxoRef] w1PaymentPubKeyHash
            void Trace.nextSlot

    in checkPredicate "Fail phase-2 validation when txo expected by on-chain mustSpendPubKeyOutput does not exist"
        (assertFailedTransaction (\_ err -> case err of {Ledger.ScriptFailure (EvaluationError ("L7":_) _) -> True; _ -> False }))
        (void trace)

{-# INLINEABLE mkValidator #-}
mkValidator :: [TxOutRef] -> () -> ScriptContext -> Bool
mkValidator txOutRefs _ ctx = P.traceIfFalse "mustSpendPubKeyOutput not satisfied" (Constraints.checkScriptContext @() @() (P.mconcat mustSpendPubKeyOutputs) ctx)
    where
        mustSpendPubKeyOutputs = Constraints.mustSpendPubKeyOutput P.<$> txOutRefs

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
