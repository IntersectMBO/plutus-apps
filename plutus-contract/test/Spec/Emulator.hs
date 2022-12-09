{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Spec.Emulator(tests) where

import Cardano.Api.Shelley qualified as C
import Cardano.Node.Emulator.Chain qualified as Chain
import Cardano.Node.Emulator.Fee (selectCoin)
import Cardano.Node.Emulator.Generators (Mockchain (Mockchain))
import Cardano.Node.Emulator.Generators qualified as Gen
import Cardano.Node.Emulator.Params (Params (Params, pNetworkId))
import Cardano.Node.Emulator.Validation qualified as Validation
import Control.Lens ((&), (.~), (^.))
import Control.Monad (void)
import Control.Monad.Freer qualified as Eff
import Control.Monad.Freer.Extras.Log (LogLevel (Debug))
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 (pack)
import Data.Default (Default (def))
import Data.Foldable (fold)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Hedgehog (Property, forAll, property)
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Ledger (CardanoTx (..), Language (PlutusV1), OnChainTx (Valid), PaymentPubKeyHash, ScriptError (EvaluationError),
               Tx (txMint), TxInType (ScriptAddress), TxOut (TxOut), Validator, Value, Versioned (Versioned),
               cardanoTxMap, getCardanoTxOutRefs, getCardanoTxOutputs, mkValidatorCardanoAddress, mkValidatorScript,
               onCardanoTx, outputs, txOutValue, unitDatum, unitRedeemer, unspentOutputs)
import Ledger.Index qualified as Index
import Ledger.Tx.CardanoAPI (fromPlutusIndex, toCardanoTxOutDatumInTx, toCardanoTxOutValue)
import Ledger.Value.CardanoAPI qualified as Value
import Plutus.Contract.Test hiding (not)
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.V1.Typed.Scripts (mkUntypedValidator)
import Plutus.Trace (EmulatorTrace, PrintEffect (PrintLn))
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Contexts (ScriptContext)
import PlutusTx qualified
import PlutusTx.Numeric qualified as P
import PlutusTx.Prelude qualified as PlutusTx
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.Hedgehog (testPropertyNamed)
import Wallet (payToPaymentPublicKeyHash_, submitTxn)
import Wallet.API qualified as W
import Wallet.Graph qualified

tests :: TestTree
tests = testGroup "all tests" [
    testGroup "UTXO model" [
        testPropertyNamed "compute UTxO of trivial blockchain" "utxo" utxo,
        testPropertyNamed "validate transaction" "txnValid" txnValid,
        testPropertyNamed "update UTXO set after each transaction" "txnUpdateUtxo" txnUpdateUtxo
        ],
    testGroup "traces" [
        testPropertyNamed "accept valid txn" "validTrace" validTrace,
        testPropertyNamed "accept valid txn 2" "validTrace2" validTrace2,
        testPropertyNamed "reject invalid txn" "invalidTrace" invalidTrace,
        testPropertyNamed "notify wallet" "notifyWallet" notifyWallet,
        testPropertyNamed "log script validation failures" "invalidScript" invalidScript,
        testPropertyNamed "payToPaymentPubkey" "payToPaymentPubKeyScript" payToPaymentPubKeyScript,
        testPropertyNamed "payToPaymentPubkey-2" "payToPaymentPubKeyScript2" payToPaymentPubKeyScript2
        ],
    testGroup "trace output" [
        goldenVsString
          "captures a trace of a wait"
          "test/Spec/golden/traceOutput - wait1.txt"
          (pure $ captureTrace (void $ Trace.waitNSlots 1)),
        goldenVsString
          "captures a trace of pubKeytransactions"
          "test/Spec/golden/traceOutput - pubKeyTransactions.txt"
          (pure $ captureTrace pubKeyTransactions),
        goldenVsString
          "captures a trace of pubKeytransactions2"
          "test/Spec/golden/traceOutput - pubKeyTransactions2.txt"
          (pure $ captureTrace pubKeyTransactions2)
    ],
    testGroup "Etc." [
        testPropertyNamed "selectCoin" "selectCoinProp" selectCoinProp,
        testPropertyNamed "txnFlows" "txnFlowsTest" txnFlowsTest,
        testPropertyNamed "evalEmulatorTrace test" "evalEmulatorTraceTest" evalEmulatorTraceTest
        ]
    ]

captureTrace
    :: EmulatorTrace ()
    -> BSL.ByteString
captureTrace trace
  = pack $ unlines output
  where
    output = capturePrintEffect
           $ Trace.runEmulatorTraceEff (def { Trace.traceConfigMinLogLevel = Debug }) def trace

capturePrintEffect
         :: Eff.Eff '[PrintEffect] r
         -> [String]
capturePrintEffect effs = snd $ Eff.run (runWriter (Eff.reinterpret f effs))
  where
    f :: PrintEffect r -> Eff.Eff '[Writer [String]] r
    f = \case
      PrintLn s -> tell [s]


wallet1, wallet2, wallet3 :: Wallet
wallet1 = knownWallet 1
wallet2 = knownWallet 2
wallet3 = knownWallet 3

pubKey1, pubKey2, pubKey3 :: PaymentPubKeyHash
pubKey1 = mockWalletPaymentPubKeyHash wallet1
pubKey2 = mockWalletPaymentPubKeyHash wallet2
pubKey3 = mockWalletPaymentPubKeyHash wallet3

utxo :: Property
utxo = property $ do
    Mockchain txPool o params <- forAll Gen.genMockchain
    Hedgehog.assert (unspentOutputs [map (Valid . Gen.signTx params o) txPool] == o)

txnValid :: Property
txnValid = property $ do
    (m, txn) <- forAll genChainTxn
    Gen.assertValid txn m

selectCoinProp :: Property
selectCoinProp = property $ do
    inputs <- forAll $ zip [(1 :: Integer) ..] <$> Gen.list (Range.linear 1 100) Gen.genValueNonNegative
    target <- forAll Gen.genValueNonNegative
    let result = selectCoin inputs target
    case result of
        Left _ ->
            Hedgehog.assert $ not $ foldMap snd inputs `Value.valueGeq` target
        Right (ins, change) ->
            Hedgehog.assert $ foldMap (fromMaybe mempty . (`lookup` inputs)) ins == (target <> change)

txnUpdateUtxo :: Property
txnUpdateUtxo = property $ do
    (Mockchain m utxos params, txn) <- forAll genChainTxn
    let options = defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Right m
        signedTx = Gen.signTx params utxos txn

        -- submit the same txn twice, so it should be accepted the first time
        -- and rejected the second time.
        trace = do
            Trace.liftWallet wallet1 (submitTxn signedTx)
            Trace.liftWallet wallet1 (submitTxn signedTx)
        pred = \case
            [ Chain.TxnValidate{}
                , Chain.SlotAdd _
                , Chain.TxnValidate _ _ _
                , Chain.TxnValidationFail _ _ _ (Index.CardanoLedgerValidationError msg) _ _
                , Chain.SlotAdd _
                ] -> "ApplyTxError [UtxowFailure (UtxoFailure (FromAlonzoUtxoFail (ValueNotConserved" `Text.isInfixOf` msg
                     || "[CollectErrors [BadTranslation (TranslationLogicMissingInput" `Text.isInfixOf` msg
            _ -> False
    void $  checkPredicateInner options (assertChainEvents pred) trace Hedgehog.annotate Hedgehog.assert (const $ pure ())

validTrace :: Property
validTrace = property $ do
    (Mockchain m utxo params, txn) <- forAll genChainTxn
    let options = defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Right m
        signedTx = Gen.signTx params utxo txn
        trace = Trace.liftWallet wallet1 (submitTxn signedTx)
    void $  checkPredicateInner options assertNoFailedTransactions trace Hedgehog.annotate Hedgehog.assert (const $ pure ())

validTrace2 :: Property
validTrace2 = property $ do
    (Mockchain m utxo params, txn) <- forAll genChainTxn
    let options = defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Right m
        signedTx = Gen.signTx params utxo txn
        trace = do
            Trace.liftWallet wallet1 (submitTxn signedTx)
            Trace.liftWallet wallet1 (submitTxn signedTx)
        predicate = assertFailedTransaction (\_ _ -> True)
    void $  checkPredicateInner options predicate trace Hedgehog.annotate Hedgehog.assert (const $ pure ())

invalidTrace :: Property
invalidTrace = property $ do
    (Mockchain m utxo params, txn) <- forAll genChainTxn
    let invalidTxn = cardanoTxMap (\tx -> tx { txMint = Value.adaValueOf 1 }) (\_ -> error "Unexpected Cardano.Api.Tx") txn
        signedTxn = Gen.signTx params utxo invalidTxn
        options = defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Right m
        trace = Trace.liftWallet wallet1 (submitTxn signedTxn)
        pred = \case
            [ Chain.TxnValidate{}
                , Chain.SlotAdd _
                , Chain.TxnValidationFail _ _ _ (Index.CardanoLedgerValidationError msg) _ _
                , Chain.SlotAdd _
                ] -> "ValueNotConservedUTxO" `Text.isInfixOf` msg
            _ -> False
    void $ checkPredicateInner options (assertChainEvents pred) trace Hedgehog.annotate Hedgehog.assert (const $ pure ())

invalidScript :: Property
invalidScript = property $ do
    (Mockchain _ _ params@Params{pNetworkId}, txn1) <- forAll genChainTxn

    -- modify one of the outputs to be a script output
    index <- forAll $ Gen.int (Range.linear 0 ((length $ getCardanoTxOutputs txn1) - 1))
    let emulatorTx = onCardanoTx id (\_ -> error "Unexpected Cardano.Api.Tx") txn1
        setOutputs o = TxOut
          $ C.TxOut
                (mkValidatorCardanoAddress pNetworkId failValidator)
                (toCardanoTxOutValue $ txOutValue o)
                (toCardanoTxOutDatumInTx unitDatum)
                C.ReferenceScriptNone
        outs = setOutputs <$> emulatorTx ^. outputs
        scriptTxn = EmulatorTx $
            emulatorTx
          & outputs .~ outs
    Hedgehog.annotateShow scriptTxn
    let outToSpend = getCardanoTxOutRefs scriptTxn !! index
    let totalVal = txOutValue (fst outToSpend)

    -- try and spend the script output
    let invalidTxnUtxo = [(snd outToSpend, fst outToSpend)]
    invalidTxn <- forAll
        $ Gen.genValidTransactionSpending
            [Gen.TxInputWitnessed (snd outToSpend) (ScriptAddress (Left failValidator) unitRedeemer (Just unitDatum))]
            totalVal
    Hedgehog.annotateShow invalidTxn

    let cUtxoIndex = either (error . show) id $ fromPlutusIndex $ Index.UtxoIndex $ Map.fromList invalidTxnUtxo
        signedInvalidTxn = onCardanoTx
          (\t -> Validation.fromPlutusTxSigned' params cUtxoIndex t Gen.knownPaymentKeys)
          (const $ error "unexpected CardanoTx")
          invalidTxn

    Hedgehog.annotateShow signedInvalidTxn
    Hedgehog.assert (case signedInvalidTxn of
      Left (Left (Index.Phase2, Index.ScriptFailure (EvaluationError msgs _))) -> elem "I always fail everything" msgs
      _                                                                        -> False
      )
    where
        failValidator :: Versioned Validator
        failValidator = Versioned (mkValidatorScript $$(PlutusTx.compile [|| mkUntypedValidator validator ||])) PlutusV1
        validator :: () -> () -> ScriptContext -> Bool
        validator _ _ _ = PlutusTx.traceError "I always fail everything"

txnFlowsTest :: Property
txnFlowsTest =
    checkPredicateGen Gen.generatorModel
        (assertBlockchain $ \chain ->
           let numTx = length $ fold chain
               flows = Wallet.Graph.txnFlows [] chain
            in
                length flows > numTx)
        pubKeyTransactions

notifyWallet :: Property
notifyWallet =
    checkPredicateGen Gen.generatorModel
    (walletFundsChange wallet1 mempty)
    (pure ())

payToPaymentPubKeyScript :: Property
payToPaymentPubKeyScript =
    let hasInitialBalance w = walletFundsChange w mempty
    in checkPredicateGen Gen.generatorModel
        (hasInitialBalance wallet1
            .&&. hasInitialBalance wallet2
            .&&. hasInitialBalance wallet3)
        pubKeyTransactions

payToPaymentPubKeyScript2 :: Property
payToPaymentPubKeyScript2 =
    let hasInitialBalance w = walletFundsChange w mempty
    in checkPredicateGen Gen.generatorModel
        (hasInitialBalance wallet1
            .&&. hasInitialBalance wallet2
            .&&. hasInitialBalance wallet3)
        pubKeyTransactions2

pubKeyTransactions :: EmulatorTrace ()
pubKeyTransactions = do
    let five = Ada.adaValueOf 5
    Trace.liftWallet wallet1 $ payToPaymentPublicKeyHash_ def W.always five pubKey2
    _ <- Trace.nextSlot
    Trace.liftWallet wallet2 $ payToPaymentPublicKeyHash_ def W.always five pubKey3
    _ <- Trace.nextSlot
    Trace.liftWallet wallet3 $ payToPaymentPublicKeyHash_ def W.always five pubKey1
    void Trace.nextSlot

pubKeyTransactions2 :: EmulatorTrace ()
pubKeyTransactions2 = do
    let payment1 = initialBalance P.- Ada.adaValueOf 10
        payment2 = initialBalance P.+ Ada.adaValueOf 10
    Trace.liftWallet wallet1 $ payToPaymentPublicKeyHash_ def W.always payment1 pubKey2
    _ <- Trace.nextSlot
    Trace.liftWallet wallet2 $ payToPaymentPublicKeyHash_ def W.always payment2 pubKey3
    _ <- Trace.nextSlot
    Trace.liftWallet wallet3 $ payToPaymentPublicKeyHash_ def W.always payment2 pubKey1
    _ <- Trace.nextSlot
    Trace.liftWallet wallet1 $ payToPaymentPublicKeyHash_ def W.always (Ada.adaValueOf 20) pubKey2
    void Trace.nextSlot

evalEmulatorTraceTest :: Property
evalEmulatorTraceTest = property $ do
    let trace = Trace.payToWallet wallet1 wallet2 (Ada.adaValueOf 10)
        res = Trace.evalEmulatorTrace def def trace
    Hedgehog.annotateShow res
    Hedgehog.assert (either (const False) (const True) res)

genChainTxn :: Hedgehog.Gen (Mockchain, CardanoTx)
genChainTxn = do
    m <- Gen.genMockchain
    txn <- Gen.genValidTransaction m
    pure (m, txn)

initialBalance :: Value
initialBalance = Ada.adaValueOf 100
