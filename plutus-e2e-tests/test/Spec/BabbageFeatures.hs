{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra

module Spec.BabbageFeatures(tests) where

import Cardano.Api qualified as C
import Data.Map qualified as Map
import Test.Tasty (TestTree, testGroup)

import Hedgehog qualified as H
import Hedgehog.Extras.Test qualified as HE
import Test.Base qualified as H
import Test.Tasty.Hedgehog (testProperty)

import Helpers (testnetOptionsBabbage7, testnetOptionsBabbage8)
import Helpers qualified as TN
import PlutusScripts qualified as PS
import Testnet.Plutus qualified as TN

tests :: TestTree
tests = testGroup "reference script"
  [ testProperty "mint a token with a reference script in Babbage PV7" (referenceScriptMint testnetOptionsBabbage7)
  , testProperty "mint a token with a reference script in Babbage PV8" (referenceScriptMint testnetOptionsBabbage8)
  --, testProperty "mint a token with a reference script in Babbage PV8" (referenceScriptMint localNodeOptionsPreview)

  , testProperty "spend locked funds with a reference script using inline datum in Babbage PV7" (referenceScriptInlineDatumSpend testnetOptionsBabbage7)
  , testProperty "spend locked funds with a reference script using inline datum in Babbage PV8" (referenceScriptInlineDatumSpend testnetOptionsBabbage8)
  --, testProperty "spend locked funds with a reference script using inline datum in Babbage PV8" (referenceScriptInlineDatumSpend localNodeOptionsPreview)

  , testProperty "spend locked funds with a reference script providing datum in txbody in Babbage PV7" (referenceScriptDatumHashSpend testnetOptionsBabbage7)
  , testProperty "spend locked funds with a reference script providing datum in txbody in Babbage PV8" (referenceScriptDatumHashSpend testnetOptionsBabbage8)
  --, testProperty "spend locked funds with a reference script providing datum in txbody in Babbage PV8" (referenceScriptDatumHashSpend localNodeOptionsPreview)
  ]

referenceScriptMint :: Either TN.LocalNodeOptions TN.TestnetOptions -> H.Property
referenceScriptMint networkOptions = H.integration . HE.runFinallies . TN.workspace "." $ \tempAbsPath -> do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions

  -- 1: spin up a testnet or use local node connected to public testnet
  (localNodeConnectInfo, pparams, networkId) <- TN.setupTestEnvironment networkOptions tempAbsPath
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId

  -- 2: build a transaction to hold reference script

  txIn <- TN.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    refScriptTxOut = TN.txOutWithRefScript era (C.lovelaceToValue 20_000_000) w1Address
      (PS.unPlutusScriptV2 PS.alwaysSucceedPolicyScriptV2)
    otherTxOut = TN.txOut era (C.lovelaceToValue 5_000_000) w1Address

    txBodyContent = (TN.emptyTxBodyContent era pparams)
      { C.txIns = TN.pubkeyTxIns [txIn]
      , C.txOuts = [refScriptTxOut, otherTxOut]
      }

  signedTx <- TN.buildTx era txBodyContent w1Address w1SKey networkId
  TN.submitTx era localNodeConnectInfo signedTx
  let refScriptTxIn = TN.txIn (TN.txId signedTx) 0
      otherTxIn   = TN.txIn (TN.txId signedTx) 1
  TN.waitForTxInAtAddress era localNodeConnectInfo w1Address refScriptTxIn

  -- 3: build a transaction to mint token using reference script

  let
    tokenValues = C.valueFromList [(PS.alwaysSucceedAssetIdV2, 6)]
    mintWitnesses = Map.fromList [PS.alwaysSucceedMintWitnessV2 era (Just refScriptTxIn)]
    collateral = TN.txInsCollateral era [otherTxIn]
    txOut = TN.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address

    txBodyContent2 = (TN.emptyTxBodyContent era pparams)
      { C.txIns = TN.pubkeyTxIns [otherTxIn]
      , C.txInsCollateral = collateral
      , C.txInsReference = TN.txInsReference era [refScriptTxIn]
      , C.txMintValue = TN.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut]
      }

  signedTx2 <- TN.buildTx era txBodyContent2 w1Address w1SKey networkId
  TN.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn = TN.txIn (TN.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <- TN.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn
  txOutHasTokenValue <- TN.txOutHasValue resultTxOut tokenValues
  H.assert txOutHasTokenValue
  H.success


referenceScriptInlineDatumSpend :: Either TN.LocalNodeOptions TN.TestnetOptions -> H.Property
referenceScriptInlineDatumSpend networkOptions = H.integration . HE.runFinallies . TN.workspace "." $ \tempAbsPath -> do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions

  -- 1: spin up a testnet or use local node connected to public testnet
  (localNodeConnectInfo, pparams, networkId) <- TN.setupTestEnvironment networkOptions tempAbsPath
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId

  -- 2: build a transaction to hold reference script

  txIn <- TN.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    refTxOut      = TN.txOutWithRefScript era (C.lovelaceToValue 20_000_000) w1Address
                     (PS.unPlutusScriptV2 PS.alwaysSucceedSpendScriptV2)
    otherTxOut    = TN.txOut era (C.lovelaceToValue 5_000_000) w1Address
    scriptAddress = TN.makeAddress (Right PS.alwaysSucceedSpendScriptHashV2) networkId
    scriptTxOut   = TN.txOutWithInlineDatum era (C.lovelaceToValue 10_000_000) scriptAddress (PS.toScriptData ())

    txBodyContent = (TN.emptyTxBodyContent era pparams)
      { C.txIns = TN.pubkeyTxIns [txIn]
      , C.txOuts = [refTxOut, otherTxOut, scriptTxOut]
      }

  signedTx <- TN.buildTx era txBodyContent w1Address w1SKey networkId
  TN.submitTx era localNodeConnectInfo signedTx
  let refScriptTxIn = TN.txIn (TN.txId signedTx) 0
      otherTxIn     = TN.txIn (TN.txId signedTx) 1
      txInAtScript  = TN.txIn (TN.txId signedTx) 2
  TN.waitForTxInAtAddress era localNodeConnectInfo w1Address refScriptTxIn

  -- 3: build a transaction to mint token using reference script

  let
    scriptTxIn = TN.txInWitness txInAtScript (PS.alwaysSucceedSpendWitnessV2 era (Just refScriptTxIn) Nothing)
    collateral = TN.txInsCollateral era [otherTxIn]
    adaValue = C.lovelaceToValue 4_200_000
    txOut = TN.txOut era adaValue w1Address

    txBodyContent2 = (TN.emptyTxBodyContent era pparams)
      { C.txIns = [scriptTxIn]
      , C.txInsReference = TN.txInsReference era [refScriptTxIn]
      , C.txInsCollateral = collateral
      , C.txOuts = [txOut]
      }

  signedTx2 <- TN.buildTx era txBodyContent2 w1Address w1SKey networkId
  TN.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn = TN.txIn (TN.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <- TN.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn
  txOutHasAdaValue <- TN.txOutHasValue resultTxOut adaValue
  H.assert txOutHasAdaValue
  H.success


referenceScriptDatumHashSpend :: Either TN.LocalNodeOptions TN.TestnetOptions -> H.Property
referenceScriptDatumHashSpend networkOptions = H.integration . HE.runFinallies . TN.workspace "." $ \tempAbsPath -> do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions

  -- 1: spin up a testnet or use local node connected to public testnet
  (localNodeConnectInfo, pparams, networkId) <- TN.setupTestEnvironment networkOptions tempAbsPath
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId

  -- 2: build a transaction to hold reference script

  txIn <- TN.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    refTxOut      = TN.txOutWithRefScript era (C.lovelaceToValue 20_000_000) w1Address
                     (PS.unPlutusScriptV2 PS.alwaysSucceedSpendScriptV2)
    otherTxOut    = TN.txOut era (C.lovelaceToValue 5_000_000) w1Address
    scriptAddress = TN.makeAddress (Right PS.alwaysSucceedSpendScriptHashV2) networkId
    datum         = PS.toScriptData ()
    scriptTxOut   = TN.txOutWithDatumHash era (C.lovelaceToValue 10_000_000) scriptAddress datum

    txBodyContent = (TN.emptyTxBodyContent era pparams)
      { C.txIns = TN.pubkeyTxIns [txIn]
      , C.txOuts = [refTxOut, otherTxOut, scriptTxOut]
      }

  signedTx <- TN.buildTx era txBodyContent w1Address w1SKey networkId
  TN.submitTx era localNodeConnectInfo signedTx
  let refScriptTxIn = TN.txIn (TN.txId signedTx) 0
      otherTxIn     = TN.txIn (TN.txId signedTx) 1
      txInAtScript  = TN.txIn (TN.txId signedTx) 2
  TN.waitForTxInAtAddress era localNodeConnectInfo w1Address refScriptTxIn

  -- 3: build a transaction to mint token using reference script

  let
    scriptTxIn = TN.txInWitness txInAtScript $ PS.alwaysSucceedSpendWitnessV2 era (Just refScriptTxIn) (Just datum)
    collateral = TN.txInsCollateral era [otherTxIn]
    adaValue = C.lovelaceToValue 4_200_000
    txOut = TN.txOut era adaValue w1Address

    txBodyContent2 = (TN.emptyTxBodyContent era pparams)
      { C.txIns = [scriptTxIn]
      , C.txInsReference = TN.txInsReference era [refScriptTxIn]
      , C.txInsCollateral = collateral
      , C.txOuts = [txOut]
      }

  signedTx2 <- TN.buildTx era txBodyContent2 w1Address w1SKey networkId
  TN.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn = TN.txIn (TN.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <- TN.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn
  txOutHasAdaValue <- TN.txOutHasValue resultTxOut adaValue
  H.assert txOutHasAdaValue
  H.success

  -- TODO: inlineDatumSpendTest (no reference script)
