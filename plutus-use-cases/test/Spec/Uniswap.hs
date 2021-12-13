{-# LANGUAGE OverloadedStrings #-}
module Spec.Uniswap(
    tests
    ) where

import Plutus.Contract.Test
import Plutus.Contracts.Uniswap.Trace qualified as Uniswap
import Plutus.Trace.Emulator qualified as Trace

import Test.Tasty

tests :: TestTree
tests = testGroup "uniswap" [
    checkPredicate "can create a liquidity pool and add liquidity"
        (assertNotDone Uniswap.setupTokens
                       (Trace.walletInstanceTag w1)
                       "setupTokens contract should be still running"
        .&&. assertNoFailedTransactions)
        Uniswap.uniswapTrace
    ]
