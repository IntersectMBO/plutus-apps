{-# LANGUAGE OverloadedStrings #-}

{- | Tests covering the integration of local modules and external packages. To name a few:
    Trace Emulator, plutus-tx-constraints, Contract.Test library,
    plutus-tx and plutus-ledger-api.
    Scenarios aim to use a variety of functions and assert relevant properties.
    Can also be considered living documentation for Contract and Tx Constraint use.
-}
module Main(main) where

import Spec.Contract qualified
import Spec.Contract.Tx.Constraints.Spec qualified
import Spec.Emulator qualified
import Spec.InlineDatum qualified
import Spec.Plutus.Contract.Oracle qualified
import Spec.Rows qualified
import Spec.Secrets qualified
import Spec.State qualified
import Spec.ThreadToken qualified
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "plutus-contract"
    [ Spec.Contract.tests
    , Spec.Emulator.tests
    , Spec.State.tests
    , Spec.Rows.tests
    , Spec.ThreadToken.tests
    , Spec.Contract.Tx.Constraints.Spec.tests
    , Spec.Secrets.tests
    , Spec.Plutus.Contract.Oracle.tests
    , Spec.InlineDatum.tests
    ]
