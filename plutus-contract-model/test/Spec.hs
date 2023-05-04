{-# LANGUAGE OverloadedStrings #-}

{- | Tests covering the integration of local modules and external packages. To name a few:
    Trace Emulator, plutus-tx-constraints, Contract.Test library,
    plutus-tx and plutus-ledger-api.
    Scenarios aim to use a variety of functions and assert relevant properties.
    Can also be considered living documentation for Contract and Tx Constraint use.
-}
module Main(main) where

import Spec.ErrorChecking qualified
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "plutus-contract-model"
    [ Spec.ErrorChecking.tests
    ]
