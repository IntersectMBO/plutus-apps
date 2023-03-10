{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# LANGUAGE RecordWildCards     #-}

module Spec.Builtins.SECP256k1(verifySchnorrAndEcdsaTest) where

import Cardano.Api qualified as C
import Data.Map qualified as Map

import Hedgehog qualified as H

import CardanoTestnet qualified as TN
import Control.Monad.IO.Class (MonadIO)
import Hedgehog (MonadTest)
import Helpers.Query qualified as Q
import Helpers.Test (TestParams (TestParams, localNodeConnectInfo, networkId, pparams, tempAbsPath))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import PlutusScripts.SECP256k1 qualified as PS

{- | Test that builtins: verifySchnorrSecp256k1Signature and verifyEcdsaSecp256k1Signature can only
   be used to mint in Babbage era protocol version 8 and beyond.

   Steps:
    - spin up a testnet
    - build and submit a transaction to mint a token using the two SECP256k1 builtins
    - if pv8+ then query the ledger to see if mint was successful otherwise expect
        "forbidden builtin" error when building tx
-}
verifySchnorrAndEcdsaTest :: (MonadIO m , MonadTest m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m ()
verifySchnorrAndEcdsaTest networkOptions TestParams{..} = do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  pv <- TN.pvFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 tempAbsPath networkId

-- build a transaction

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    (verifySchnorrAssetId, verifyEcdsaAssetId, verifySchnorrMintWitness, verifyEcdsaMintWitness) =
      case era of
        C.AlonzoEra  ->
          ( PS.verifySchnorrAssetIdV1,
            PS.verifyEcdsaAssetIdV1,
            PS.verifySchnorrMintWitnessV1 era,
            PS.verifyEcdsaMintWitnessV1 era )
        C.BabbageEra ->
          ( PS.verifySchnorrAssetIdV2,
            PS.verifyEcdsaAssetIdV2,
            PS.verifySchnorrMintWitnessV2 era,
            PS.verifyEcdsaMintWitnessV2 era )

    tokenValues = C.valueFromList [(verifySchnorrAssetId, 4), (verifyEcdsaAssetId, 2)]
    txOut = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address
    mintWitnesses = Map.fromList [verifySchnorrMintWitness, verifyEcdsaMintWitness]
    collateral = Tx.txInsCollateral era [txIn]
    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txInsCollateral = collateral
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut]
      }

  case pv < 8 of
    True -> do
      -- Assert that "forbidden" error occurs when attempting to use either SECP256k1 builtin
      eitherTx <- Tx.buildTx' era txBodyContent w1Address w1SKey networkId
      H.assert $ Tx.isTxBodyScriptExecutionError
        "Forbidden builtin function: (builtin verifySchnorrSecp256k1Signature)" eitherTx
      H.assert $ Tx.isTxBodyScriptExecutionError
        "Forbidden builtin function: (builtin verifyEcdsaSecp256k1Signature)" eitherTx
      H.success

    False -> do
      -- Build and submit transaction
      signedTx <- Tx.buildTx era txBodyContent w1Address w1SKey networkId
      Tx.submitTx era localNodeConnectInfo signedTx
      let expectedTxIn = Tx.txIn (Tx.txId signedTx) 0

      -- Query for txo and assert it contains newly minting tokens to prove successful use of SECP256k1 builtins
      resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "TN.getTxOutAtAddress"
      txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
      H.assert txOutHasTokenValue

      H.success
