{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.Coerce (coerce)
import Data.Set qualified as S

import Hedgehog (Gen, Property, assert, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
import Gen.Cardano.Api.Typed qualified as CGen

import Marconi.Index.ScriptTx qualified as ScriptTx

import Integration qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Marconi"
  [ testPropertyNamed "prop_script_hashes_in_tx_match" "getTxBodyScriptsRoundtrip" getTxBodyScriptsRoundtrip
  , Integration.tests
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

genTxBodyWithTxIns
  :: C.IsCardanoEra era
  => C.CardanoEra era
  -> [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era))]
  -> C.TxInsCollateral era
  -> Gen (C.TxBody era)
genTxBodyWithTxIns era txIns txInsCollateral = do
  txBodyContent <- genTxBodyContentWithTxInsCollateral era txIns txInsCollateral
  case C.makeTransactionBody txBodyContent of
    Left err     -> fail $ C.displayError err
    Right txBody -> pure txBody

genTxBodyContentWithTxInsCollateral
  :: C.CardanoEra era
  -> [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era))]
  -> C.TxInsCollateral era
  -> Gen (C.TxBodyContent C.BuildTx era)
genTxBodyContentWithTxInsCollateral era txIns txInsCollateral = do
  txbody <- CGen.genTxBodyContent era
  txProtocolParams <- C.BuildTxWith . Just <$> CGen.genProtocolParameters
  pure $ txbody
    { C.txIns
    , C.txInsCollateral
    , C.txProtocolParams
    }

genWitnessAndHashInEra :: C.CardanoEra era -> Gen (C.Witness C.WitCtxTxIn era, C.ScriptHash)
genWitnessAndHashInEra era = do
  C.ScriptInEra scriptLanguageInEra script <- CGen.genScriptInEra era
  witness :: C.Witness C.WitCtxTxIn era1 <- C.ScriptWitness C.ScriptWitnessForSpending <$> case script of
    C.PlutusScript version plutusScript -> do
      scriptData <- CGen.genScriptData
      executionUnits <- genExecutionUnits
      pure $ C.PlutusScriptWitness
        scriptLanguageInEra
        version
        (Shelley.PScript plutusScript)
        (C.ScriptDatumForTxIn scriptData)
        scriptData
        executionUnits
    C.SimpleScript version simpleScript ->
      pure $ C.SimpleScriptWitness scriptLanguageInEra version (Shelley.SScript simpleScript)
  pure (witness, C.hashScript script)

-- | TODO Copy-paste from cardano-node: cardano-api/gen/Gen/Cardano/Api/Typed.hs
genExecutionUnits :: Gen C.ExecutionUnits
genExecutionUnits = C.ExecutionUnits <$> Gen.integral (Range.constant 0 1000)
                                   <*> Gen.integral (Range.constant 0 1000)
