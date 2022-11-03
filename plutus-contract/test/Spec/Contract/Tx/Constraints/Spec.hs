{-# LANGUAGE OverloadedStrings #-}

{- | Integration tests covering each of the transaction constraints used by plutus-contract.
    Currently covering both the plutus-ledger-constraints and plutus-tx-constraints. Soon only
    plutus-tx-constraints will be supported.

    These tests cover both the off chain and on chain constraints:
      - Off chain constraints are used to build the transaction correctly to pass all phase-1
        and phase-2 checks performed by the ledger.
      - On chain constraints use plutus-tx to produce a script for validating all conditions.

    The general pattern is to build and submit a single transaction using the off chain constraint
    under test to execute a minting policy using the same constraint on chain. This helps to
    minimise complexity, the alternative approach of using a spending script requires at least
    two transactions.

    The Trace Emulator is used to ensure the transaction constraints are being used similarly to
    how developers building their applications would. This improves coverage of real-world scenarios
    whilst also serving as a good reference for example usage.
-}
module Spec.Contract.Tx.Constraints.Spec(tests) where

import Spec.Contract.Tx.Constraints.MustIncludeDatum qualified
import Spec.Contract.Tx.Constraints.MustMint qualified
import Spec.Contract.Tx.Constraints.MustPayToOtherScript qualified
import Spec.Contract.Tx.Constraints.MustPayToPubKeyAddress qualified
import Spec.Contract.Tx.Constraints.MustProduceAtLeast qualified
import Spec.Contract.Tx.Constraints.MustReferenceOutput qualified
import Spec.Contract.Tx.Constraints.MustSatisfyAnyOf qualified
import Spec.Contract.Tx.Constraints.MustSpendAtLeast qualified
import Spec.Contract.Tx.Constraints.MustSpendPubKeyOutput qualified
import Spec.Contract.Tx.Constraints.MustSpendScriptOutput qualified
import Spec.Contract.Tx.Constraints.RequiredSigner qualified
import Spec.Contract.Tx.Constraints.TimeValidity qualified
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "constraints" [
    Spec.Contract.Tx.Constraints.MustIncludeDatum.tests,
    Spec.Contract.Tx.Constraints.MustMint.tests,
    Spec.Contract.Tx.Constraints.MustPayToOtherScript.tests,
    Spec.Contract.Tx.Constraints.MustProduceAtLeast.tests,
    Spec.Contract.Tx.Constraints.MustPayToPubKeyAddress.tests,
    Spec.Contract.Tx.Constraints.MustReferenceOutput.tests,
    Spec.Contract.Tx.Constraints.MustSatisfyAnyOf.tests,
    Spec.Contract.Tx.Constraints.MustSpendAtLeast.tests,
    Spec.Contract.Tx.Constraints.MustSpendPubKeyOutput.tests,
    Spec.Contract.Tx.Constraints.MustSpendScriptOutput.tests,
    Spec.Contract.Tx.Constraints.RequiredSigner.tests,
    Spec.Contract.Tx.Constraints.TimeValidity.tests
    ]
