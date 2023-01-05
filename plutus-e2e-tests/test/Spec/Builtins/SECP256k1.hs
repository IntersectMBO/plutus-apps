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

{- | Test builtin verifySchnorrSecp256k1Signature can be used to verify multiple signatures using a
   minting policy in a Babbage era transaction.

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

-- 2: build a transaction
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
  txIn <- TN.firstTxIn localNodeConnectInfo w1Address
  let collateral = C.TxInsCollateral C.CollateralInBabbageEra [txIn]

      tokenSchnorr = C.AssetId PS.verifySchnorrPolicyId (C.AssetName "Schnorr")
      --TODO: tokenEcdsa = C.AssetId Scripts.verifyEcdsaPolicyId (C.AssetName "ECDSA")
      tokenValue = C.valueFromList [(tokenSchnorr, 666)] --TODO: add tokenEcdsa

      --redeemerSchnorr = C.fromPlutusData $ PlutusV2.toData Scripts.verifySchnorrParams
      --TODO: redeemerEcdsa = C.fromPlutusData $ PlutusV2.toData Scripts.verifyEcdsaParams

      --scriptWitnessSchnorr = TN.mintScriptWitnessV2 Scripts.verifySchnorrPolicyScript redeemerSchnorr
      --TODO: scriptWitnessEcdsa = Scripts.mintScriptWitness redeemerEcdsa

      txOut :: C.TxOut ctx C.BabbageEra
      txOut = TN.txOutNoDatumOrRefScript (C.lovelaceToValue 3_000_000 <> tokenValue) w1Address

      mintWitnesses = Map.fromList [PS.verifySchnorrMintWitness]

      txBodyContent :: C.TxBodyContent C.BuildTx C.BabbageEra
      txBodyContent = (TN.emptyTxBodyContent pparams)
        { C.txIns = TN.pubkeyTxIns [txIn]
        , C.txInsCollateral = collateral
        , C.txOuts = [txOut]
        , C.txMintValue = TN.txMintValue tokenValue mintWitnesses
        }

  signedTx <- TN.buildTx txBodyContent w1Address w1SKey networkId
  H.annotateShow signedTx --remove

-- 3: submit transaction to mint

  TN.submitTx localNodeConnectInfo signedTx

-- 4. query and assert successful mint

  let expectedTxIn = TN.txIn (C.getTxId $ C.getTxBody signedTx) 0
  resultTxOut <- TN.getTxOutAtAddress localNodeConnectInfo w1Address expectedTxIn

  H.annotateShow resultTxOut -- remove
  -- TODO: get asset value from resultTxOut

  --endUtxos <- TN.getAddressTxInsValue localNodeConnectInfo w1Address --remove
  --H.annotateShow endUtxos -- to check if minted - remove
  H.assert False
