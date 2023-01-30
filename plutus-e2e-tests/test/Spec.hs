{-# LANGUAGE OverloadedStrings #-}

{- | Tests covering the integration of local modules and external packages. To name a few:
    Trace Emulator, plutus-ledger-constraints, plutus-tx-constarints, Contract.Test library,
    plutus-tx and plutus-ledger-api.
    Scenarios aim to use a variety of functions and assert relevant properties.
    Can also be considered living documentation for Contract and Tx Constraint use.
-}
module Main(main) where

import Spec.Builtins.SECP256k1 qualified
import Spec.ReferenceScript qualified
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Plutus E2E" [
    Spec.Builtins.SECP256k1.tests
  , Spec.ReferenceScript.tests
    ]
