{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Cardano.Node.Emulator.MTLSpec (tests) where

import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.RWS.Strict (evalRWS)
import Data.Default (def)
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

import Cardano.Node.Emulator qualified as E
import Cardano.Node.Emulator.MTL
import Cardano.Node.Emulator.MTL.Test
import Ledger.Address (CardanoAddress, PaymentPrivateKey)
import Ledger.Tx.CardanoAPI (CardanoBuildTx (..))
import Ledger.Value.CardanoAPI qualified as Value


tests :: TestTree
tests = testGroup "Cardano.Node.Emulator.MTL"
  [ checkPredicate "submit empty tx" (\lg _ -> hasValidatedTransactionCountOfTotal 1 1 lg) $ do
      void $ submitUnbalancedTx mempty w1 (CardanoBuildTx E.emptyTxBodyContent) [pk1]
      nextSlot

  ,  checkPredicate "payToAddress" (\lg _ -> hasValidatedTransactionCountOfTotal 1 1 lg) $ do
      void $ payToAddress (w1, pk1) w2 (Value.adaValueOf 1)
      nextSlot

  ,  checkPredicate "payToAddress twice in one slot" (\lg _ -> hasValidatedTransactionCountOfTotal 2 2 lg) $ do
      void $ payToAddress (w1, pk1) w2 (Value.adaValueOf 1)
      void $ payToAddress (w2, pk2) w1 (Value.adaValueOf 1)
      nextSlot

  ,  checkPredicate "payToAddress in two slots" (\lg _ -> hasValidatedTransactionCountOfTotal 2 2 lg) $ do
      void $ payToAddress (w1, pk1) w2 (Value.adaValueOf 1)
      nextSlot
      void $ payToAddress (w1, pk1) w2 (Value.adaValueOf 1)
      nextSlot
  ]

w1, w2 :: CardanoAddress
w1 : w2 : _ = E.knownAddresses

pk1, pk2 :: PaymentPrivateKey
pk1 : pk2 : _ = E.knownPaymentPrivateKeys

checkPredicate
  :: TestName
  -> (Seq E.ChainEvent -> Either EmulatorError a -> Maybe String)
  -> EmulatorM a
  -> TestTree
checkPredicate testName test contract =
  testCase testName $ do
    let params = def
        initialDist = Map.fromList [(w1, Value.adaValueOf 10), (w2, Value.adaValueOf 10)]
        (res, lg) = evalRWS (runExceptT contract) params (emptyEmulatorStateWithInitialDist initialDist)
    for_ (test lg res) $ \msg ->
      assertFailure $ renderLogs lg ++ "\n" ++ msg
