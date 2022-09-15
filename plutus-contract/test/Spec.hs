{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Spec.Balancing qualified
import Spec.Contract qualified
import Spec.Contract.TxConstraints qualified
import Spec.Emulator qualified
import Spec.ErrorChecking qualified
import Spec.Plutus.Contract.Oracle qualified
import Spec.Plutus.Contract.Wallet qualified
import Spec.Rows qualified
import Spec.Secrets qualified
import Spec.State qualified
import Spec.ThreadToken qualified
import Spec.TxConstraints.MustIncludeDatum qualified
import Spec.TxConstraints.MustMint qualified
import Spec.TxConstraints.MustPayToPubKeyAddress qualified
import Spec.TxConstraints.MustSpendAtLeast qualified
import Spec.TxConstraints.MustSpendScriptOutput qualified
import Spec.TxConstraints.RequiredSigner qualified
import Spec.TxConstraints.TimeValidity qualified
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "plutus-contract" [
    Spec.Contract.tests,
    Spec.Contract.TxConstraints.tests,
    Spec.Emulator.tests,
    Spec.State.tests,
    Spec.Rows.tests,
    Spec.ThreadToken.tests,
    Spec.TxConstraints.MustIncludeDatum.tests,
    Spec.TxConstraints.MustMint.tests,
    Spec.TxConstraints.MustPayToPubKeyAddress.tests,
    Spec.TxConstraints.MustSpendAtLeast.tests,
    Spec.TxConstraints.MustSpendScriptOutput.tests,
    Spec.TxConstraints.RequiredSigner.tests,
    Spec.TxConstraints.TimeValidity.tests,
    Spec.Secrets.tests,
    Spec.ErrorChecking.tests,
    Spec.Plutus.Contract.Wallet.tests,
    Spec.Plutus.Contract.Oracle.tests,
    Spec.Balancing.tests
    ]
