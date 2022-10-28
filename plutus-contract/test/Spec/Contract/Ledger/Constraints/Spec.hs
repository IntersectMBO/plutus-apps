{-# LANGUAGE OverloadedStrings #-}

{- | Integration tests covering each of the transaction constraints used by plutus-contract.
    Currently covering both the plutus-ledger-constraints and plutus-tx-constraints. Soon only
    plutus-tx-constraints will be supported.
    These tests cover both the off chain and on chain constraints:
      - Off chain constraints are used to build the transaction correctly to pass all phase-1
        and phase-2 checks performed by the ledger.
      - On chain constraints use plutus-tx to produce a script for validating all conditions.
    The Trace Emulator is used to ensure the transaction constraints are being used similarly to
    how developers building their applications would. This improves coverage of real-world scenarios
    whilst also serving as a good reference for example usage.
-}
module Spec.Contract.Ledger.Constraints.Spec(tests) where

import Spec.Contract.Ledger.Constraints.MustIncludeDatum qualified
import Spec.Contract.Ledger.Constraints.MustMint qualified
import Spec.Contract.Ledger.Constraints.MustPayToOtherScript qualified
import Spec.Contract.Ledger.Constraints.MustPayToPubKeyAddress qualified
import Spec.Contract.Ledger.Constraints.MustProduceAtLeast qualified
import Spec.Contract.Ledger.Constraints.MustReferenceOutput qualified
import Spec.Contract.Ledger.Constraints.MustSatisfyAnyOf qualified
import Spec.Contract.Ledger.Constraints.MustSpendAtLeast qualified
import Spec.Contract.Ledger.Constraints.MustSpendPubKeyOutput qualified
import Spec.Contract.Ledger.Constraints.MustSpendScriptOutput qualified
import Spec.Contract.Ledger.Constraints.RequiredSigner qualified
import Spec.Contract.Ledger.Constraints.TimeValidity qualified
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "constraints" [
    Spec.Contract.Ledger.Constraints.MustIncludeDatum.tests,
    Spec.Contract.Ledger.Constraints.MustMint.tests,
    Spec.Contract.Ledger.Constraints.MustPayToOtherScript.tests,
    Spec.Contract.Ledger.Constraints.MustProduceAtLeast.tests,
    Spec.Contract.Ledger.Constraints.MustPayToPubKeyAddress.tests,
    Spec.Contract.Ledger.Constraints.MustReferenceOutput.tests,
    Spec.Contract.Ledger.Constraints.MustSatisfyAnyOf.tests,
    Spec.Contract.Ledger.Constraints.MustSpendAtLeast.tests,
    Spec.Contract.Ledger.Constraints.MustSpendPubKeyOutput.tests,
    Spec.Contract.Ledger.Constraints.MustSpendScriptOutput.tests,
    Spec.Contract.Ledger.Constraints.RequiredSigner.tests,
    Spec.Contract.Ledger.Constraints.TimeValidity.tests
    ]
