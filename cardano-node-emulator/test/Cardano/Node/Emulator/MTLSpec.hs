{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.Node.Emulator.MTLSpec (tests) where

import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.RWS.Strict (evalRWS)
import Data.Default (def)
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Text.Lazy.Encoding qualified as Text
import Ledger.Address (CardanoAddress, PaymentPrivateKey)
import Ledger.Tx.CardanoAPI (CardanoBuildTx (CardanoBuildTx))
import Ledger.Value.CardanoAPI qualified as Value
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertFailure, testCase)

import Cardano.Node.Emulator.API (EmulatorError, EmulatorLogs, EmulatorM, emptyEmulatorStateWithInitialDist, nextSlot,
                                  payToAddress, submitUnbalancedTx)
import Cardano.Node.Emulator.Generators qualified as E
import Cardano.Node.Emulator.Test (hasValidatedTransactionCountOfTotal, renderLogs)

tests :: TestTree
tests = testGroup "Cardano.Node.Emulator.MTL"
  [ checkPredicate "submit empty tx" (\lg _ -> hasValidatedTransactionCountOfTotal 1 1 lg) $ do
      void $ submitUnbalancedTx mempty w1 [pk1] (CardanoBuildTx E.emptyTxBodyContent)
      nextSlot

  , checkPredicate "payToAddress" (\lg _ -> hasValidatedTransactionCountOfTotal 1 1 lg) $ do
      void $ payToAddress (w1, pk1) w2 (Value.adaValueOf 1)
      nextSlot

  , checkPredicate "payToAddress twice in one slot" (\lg _ -> hasValidatedTransactionCountOfTotal 2 2 lg) $ do
      void $ payToAddress (w1, pk1) w2 (Value.adaValueOf 1)
      void $ payToAddress (w2, pk2) w1 (Value.adaValueOf 1)
      nextSlot

  , checkPredicate "payToAddress in two slots" (\lg _ -> hasValidatedTransactionCountOfTotal 2 2 lg) $ do
      void $ payToAddress (w1, pk1) w2 (Value.adaValueOf 1)
      nextSlot
      void $ payToAddress (w1, pk1) w2 (Value.adaValueOf 1)
      nextSlot

  , goldenVsString "captures the log of payToAddress"
      "test/Cardano/Node/Emulator/golden/logs - payToAddress.txt"
      (pure . Text.encodeUtf8 . LText.fromStrict . renderLogs . snd . run $ do
        void $ payToAddress (w1, pk1) w2 (Value.adaValueOf 1)
        nextSlot
        void $ payToAddress (w1, pk1) w2 (Value.adaValueOf 1)
        nextSlot)
  ]

w1, w2 :: CardanoAddress
w1 : w2 : _ = E.knownAddresses

pk1, pk2 :: PaymentPrivateKey
pk1 : pk2 : _ = E.knownPaymentPrivateKeys

run :: EmulatorM a -> (Either EmulatorError a, EmulatorLogs)
run contract = evalRWS (runExceptT contract) params (emptyEmulatorStateWithInitialDist initialDist)
  where
    params = def
    initialDist = Map.fromList [(w1, Value.adaValueOf 10), (w2, Value.adaValueOf 10)]

checkPredicate
  :: TestName
  -> (EmulatorLogs -> Either EmulatorError a -> Maybe String)
  -> EmulatorM a
  -> TestTree
checkPredicate testName test contract =
  testCase testName $ do
    let (res, lg) = run contract
    for_ (test lg res) $ \msg ->
      assertFailure $ Text.unpack (renderLogs lg) ++ "\n" ++ msg
