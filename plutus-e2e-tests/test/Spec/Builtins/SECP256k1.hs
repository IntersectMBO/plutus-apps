{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra

module Spec.Builtins.SECP256k1(tests) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens ((??))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either qualified as E
import Data.Map qualified as Map
import Data.Maybe qualified as M
import Plutus.V2.Ledger.Api qualified as PlutusV2
import Test.Tasty (TestTree, testGroup)

import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Extras.Test qualified as HE
import Test.Base qualified as H
import Test.Tasty.Hedgehog (fromGroup, testProperty)

import Helpers qualified as TN
import PlutusScripts qualified as PS
import Testnet.Plutus qualified as TN

testnetOptionsAlonzo   = TN.defaultTestnetOptions {TN.era = C.AnyCardanoEra C.AlonzoEra,  TN.protocolVersion = 6}
testnetOptionsBabbage7 = TN.defaultTestnetOptions {TN.era = C.AnyCardanoEra C.BabbageEra, TN.protocolVersion = 7}
testnetOptionsBabbage8 = TN.defaultTestnetOptions {TN.era = C.AnyCardanoEra C.BabbageEra, TN.protocolVersion = 8}

tests :: TestTree
tests = testGroup "SECP256k1"
  [ testProperty "unable to use SECP256k1 builtins in Alonzo PV6" (verifySchnorrAndEcdsa testnetOptionsAlonzo)
  , testProperty "unable to use SECP256k1 builtins in Babbage PV7" (verifySchnorrAndEcdsa testnetOptionsBabbage7)
  , testProperty "can use SECP256k1 builtins in Babbage PV8" (verifySchnorrAndEcdsa testnetOptionsBabbage8)
  ]

{- | Test that builtins: verifySchnorrSecp256k1Signature and verifyEcdsaSecp256k1Signature can only
   be used to mint in Babbage era protocol version 8 and beyond.

   Steps:
    - spin up a testnet
    - build and submit a transaction to mint a token using the two SECP256k1 builtins
    - query the ledger to see if mint was successful
-}
verifySchnorrAndEcdsa :: TN.TestnetOptions -> H.Property
verifySchnorrAndEcdsa testnetOptions = H.integration . HE.runFinallies . TN.workspace "chairman" $ \tempAbsPath -> do

  let pv = TN.protocolVersion testnetOptions
  C.AnyCardanoEra era <- return $ TN.era testnetOptions

-- 1: spin up a testnet
  base <- TN.getProjectBase
  (localNodeConnectInfo, pparams, networkId) <- TN.startTestnet era testnetOptions base tempAbsPath
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId

-- 2: build a transaction

  txIn <- TN.firstTxIn era localNodeConnectInfo w1Address

  let
    tokenValues = C.valueFromList [(PS.verifySchnorrAssetIdV2, 4), (PS.verifyEcdsaAssetIdV2, 2)]
    txOut = TN.txOutNoDatumOrRefScript era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address
    mintWitnesses = Map.fromList [PS.verifySchnorrMintWitnessV2 era, PS.verifyEcdsaMintWitnessV2 era]
    collateral = TN.txInsCollateral era [txIn]

    txBodyContent = (TN.emptyTxBodyContent era pparams)
      { C.txIns = TN.pubkeyTxIns [txIn]
      , C.txInsCollateral = collateral
      , C.txMintValue = TN.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut]
      }

  case pv of
    6 -> do
      let
        tokenValuesPv6 = C.valueFromList [(PS.verifySchnorrAssetIdV1, 4), (PS.verifyEcdsaAssetIdV1, 2)]
        mintWitnessesPv6 = Map.fromList [PS.verifySchnorrMintWitnessV1 era, PS.verifyEcdsaMintWitnessV1 era]
        txBodyContentPv6 = txBodyContent { C.txMintValue = TN.txMintValue era tokenValuesPv6 mintWitnessesPv6}
      eitherTx <- TN.buildTx' era txBodyContentPv6 w1Address w1SKey networkId
      H.assert (E.isLeft eitherTx) -- todo: check for specific plutus error
      H.success

    7 -> do
      eitherTx <- TN.buildTx' era txBodyContent w1Address w1SKey networkId
      H.assert (E.isLeft eitherTx) -- todo: check for prohibited builtin plutus error
      H.success

    8 -> do
      signedTx <- TN.buildTx era txBodyContent w1Address w1SKey networkId
      TN.submitTx era localNodeConnectInfo signedTx
      let expectedTxIn = TN.txInFromSignedTx signedTx 0
      -- 3. query and assert successful mint
      resultTxOut <- TN.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn
      txOutHasTokenValue <- TN.txOutHasValue resultTxOut tokenValues
      H.assert txOutHasTokenValue
      H.success
