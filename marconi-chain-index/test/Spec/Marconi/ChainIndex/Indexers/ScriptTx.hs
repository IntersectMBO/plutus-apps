{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.ChainIndex.Indexers.ScriptTx ( tests) where

import Control.Monad (replicateM)
import Data.Coerce (coerce)
import Data.Set qualified as S

import Hedgehog (Property, assert, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Api qualified as C
import Gen.Cardano.Api.Typed qualified as CGen
import Gen.Marconi.ChainIndex.Types (genTxBodyWithTxIns, genWitnessAndHashInEra)
import Marconi.ChainIndex.Indexers.ScriptTx qualified as ScriptTx

tests :: TestTree
tests = testGroup "Spec.Marconi.ChainIndex.Indexers.ScriptTx are:"
  [ testPropertyNamed
    "prop_script_hashes_in_tx_match" "getTxBodyScriptsRoundtrip"
    getTxBodyScriptsRoundtrip
  ]

-- | Create @nScripts@ scripts, add them to a transaction body, then
-- generate a transaction with @makeTransactionBody@ and check if the
-- scripts put in are present in the generated transaction.
getTxBodyScriptsRoundtrip :: Property
getTxBodyScriptsRoundtrip = property $ do
  nScripts <- forAll $ Gen.integral (Range.linear 5 500)
  C.AnyCardanoEra (era :: C.CardanoEra era) <- forAll $
    Gen.enum (C.AnyCardanoEra C.ShelleyEra) maxBound

  txIns <- replicateM nScripts $ forAll CGen.genTxIn
  witnessesHashes <- replicateM nScripts $ forAll $ genWitnessAndHashInEra era

  let (witnesses, scriptHashes) = unzip witnessesHashes
      collateral = case C.collateralSupportedInEra era of
        Just yes -> C.TxInsCollateral yes txIns
        _        -> C.TxInsCollateralNone

  txBody <- forAll $ genTxBodyWithTxIns era (zip txIns $ map C.BuildTxWith witnesses) collateral
  let hashesFound = map coerce $ ScriptTx.getTxBodyScripts txBody :: [C.ScriptHash]
  assert $ S.fromList scriptHashes == S.fromList hashesFound
