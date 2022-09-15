{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.Coerce (coerce)
import Data.Set qualified as S

import Hedgehog (Gen, Property, assert, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Cardano.Api qualified as C
import Gen.Cardano.Api.Typed qualified as CGen

import Marconi.Index.ScriptTx qualified as ScriptTx

import Gen
import Integration qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Marconi"
  [ -- testProperty "prop_script_hashes_in_tx_match" getTxBodyScriptsRoundtrip
    Integration.tests
  ]

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
  txOuts <- Gen.list (Range.constant 1 10) (CGen.genTxOut era)
  txFee <- genTxFee era
  txValidityRange <- genTxValidityRange era
  txMetadata <- genTxMetadataInEra era
  txAuxScripts <- genTxAuxScripts era
  let txExtraKeyWits = C.TxExtraKeyWitnessesNone --TODO: Alonzo era: Generate witness key hashes
  txProtocolParams <- C.BuildTxWith . Just <$> genProtocolParameters
  txWithdrawals <- genTxWithdrawals era
  txCertificates <- genTxCertificates era
  txUpdateProposal <- genTxUpdateProposal era
  txMintValue <- genTxMintValue era
  txScriptValidity <- genTxScriptValidity era

  pure $ C.TxBodyContent
    { C.txIns
    , C.txInsCollateral
    , C.txOuts
    , C.txFee
    , C.txValidityRange
    , C.txMetadata
    , C.txAuxScripts
    , C.txExtraKeyWits
    , C.txProtocolParams
    , C.txWithdrawals
    , C.txCertificates
    , C.txUpdateProposal
    , C.txMintValue
    , C.txScriptValidity
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
        plutusScript
        (C.ScriptDatumForTxIn scriptData)
        scriptData
        executionUnits
    C.SimpleScript version simpleScript ->
      pure $ C.SimpleScriptWitness scriptLanguageInEra version simpleScript
  pure (witness, C.hashScript script)
