{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra

module Spec.ReferenceScript(tests) where

import Cardano.Api qualified as C
import Data.Map qualified as Map
import Test.Tasty (TestTree, testGroup)

import Hedgehog qualified as H
import Hedgehog.Extras.Test qualified as HE
import Test.Base qualified as H
import Test.Tasty.Hedgehog (testProperty)

import Helpers qualified as TN
import PlutusScripts qualified as PS
import Testnet.Plutus qualified as TN

testnetOptionsBabbage8 :: TN.TestnetOptions
testnetOptionsBabbage8 = TN.defaultTestnetOptions {TN.era = C.AnyCardanoEra C.BabbageEra, TN.protocolVersion = 8}

tests :: TestTree
tests = testGroup "reference script"
  [ testProperty "mint a token with a reference script" (referenceScriptMint testnetOptionsBabbage8) ]

referenceScriptMint :: TN.TestnetOptions -> H.Property
referenceScriptMint testnetOptions = H.integration . HE.runFinallies . TN.workspace "chairman" $ \tempAbsPath -> do

  C.AnyCardanoEra era <- return $ TN.era testnetOptions

-- 1: spin up a testnet
  base <- TN.getProjectBase
  (localNodeConnectInfo, pparams, networkId) <- TN.startTestnet era testnetOptions base tempAbsPath
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId

-- 2: build a transaction to hold reference script

  txIn <- TN.firstTxIn era localNodeConnectInfo w1Address

  let
    txOut = TN.txOutWithRefScript era (C.lovelaceToValue 20_000_000) w1Address (PS.unPlutusScriptV2 PS.alwaysSucceedPolicyScriptV2)

    txBodyContent = (TN.emptyTxBodyContent era pparams)
      { C.txIns = TN.pubkeyTxIns [txIn]
      , C.txOuts = [txOut]
      }

  signedTx <- TN.buildTx era txBodyContent w1Address w1SKey networkId
  TN.submitTx era localNodeConnectInfo signedTx
  let expectedTxIn = TN.txInFromSignedTx signedTx 0
  TN.waitForTxInAtAddress era localNodeConnectInfo w1Address expectedTxIn

-- 3: build a transaction to mint token using reference script

  let
    tokenValues = C.valueFromList [(PS.alwaysSucceedAssetIdV2, 6)]
    txIn2 = expectedTxIn
    mintWitnesses = Map.fromList [PS.alwaysSucceedMintWitnessV2 era (Just txIn2)]
    collateral = TN.txInsCollateral era [txIn2]
    txOut2 = TN.txOutNoDatumOrRefScript era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address

    txBodyContent2 = (TN.emptyTxBodyContent era pparams)
      { C.txIns = TN.pubkeyTxIns [txIn2]
      , C.txInsCollateral = collateral
      , C.txMintValue = TN.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut2]
      }

  signedTx2 <- TN.buildTx era txBodyContent2 w1Address w1SKey networkId
  TN.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn2 = TN.txInFromSignedTx signedTx2 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <- TN.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn2
  txOutHasTokenValue <- TN.txOutHasValue resultTxOut tokenValues
  H.assert txOutHasTokenValue
  H.success
