{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Builtins.SECP256k1(tests) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as Map
import Plutus.V2.Ledger.Api qualified as PlutusV2
import Test.Tasty (TestTree, testGroup)

import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Extras.Test qualified as HE
import Test.Base qualified as H
import Test.Tasty.Hedgehog (testPropertyNamed)

import Helpers qualified as TN
import PlutusScripts qualified as PS
import Testnet.Babbage qualified as TN

tests :: TestTree
tests = testGroup "SECP256k1"
  [ testPropertyNamed "verify schnorr and ecdsa builtins" "verifySchnorrAndEcdsa" verifySchnorrAndEcdsa
  ]

{- | Test that builtins: verifySchnorrSecp256k1Signature and verifyEcdsaSecp256k1Signature can be
   used successfully to mint in a Babbage era transaction.

   Steps:
    - spin up a testnet
    - build and submit a transaction to mint a token using the builtin
    - query the ledger to see if mint was successful
-}
verifySchnorrAndEcdsa :: H.Property
verifySchnorrAndEcdsa = H.integration . HE.runFinallies . TN.workspace "chairman" $ \tempAbsPath -> do

-- 1: spin up a testnet

  base <- TN.getProjectBase
  (localNodeConnectInfo, pparams, networkId) <- TN.startTestnet TN.defaultTestnetOptions base tempAbsPath
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId

-- 2: build a transaction

  txIn <- TN.firstTxIn localNodeConnectInfo w1Address

  let
    collateral = C.TxInsCollateral C.CollateralInBabbageEra [txIn]
    tokenValues = C.valueFromList [(PS.verifySchnorrAssetId, 4), (PS.verifyEcdsaAssetId, 2)]
    txOut = TN.txOutNoDatumOrRefScript (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address
    mintWitnesses = Map.fromList [PS.verifySchnorrMintWitness, PS.verifyEcdsaMintWitness]

    txBodyContent = (TN.emptyTxBodyContent pparams)
      { C.txIns = TN.pubkeyTxIns [txIn]
      , C.txInsCollateral = collateral
      , C.txOuts = [txOut]
      , C.txMintValue = TN.txMintValue tokenValues mintWitnesses
      }

  signedTx <- TN.buildTx txBodyContent w1Address w1SKey networkId

  TN.submitTx localNodeConnectInfo signedTx

-- 3. query and assert successful mint

  let expectedTxIn = TN.txInFromSignedTx signedTx 0
  resultTxOut <- TN.getTxOutAtAddress localNodeConnectInfo w1Address expectedTxIn
  txOutHasTokenValue <- TN.txOutHasValue resultTxOut tokenValues

  H.assert txOutHasTokenValue

