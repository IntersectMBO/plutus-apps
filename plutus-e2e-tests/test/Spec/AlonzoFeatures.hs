{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra
{-# LANGUAGE RecordWildCards     #-}

module Spec.AlonzoFeatures (
    checkTxInfoV1Test,
    datumHashSpendTest,
    mintBurnTest
    ) where

import Cardano.Api qualified as C
import CardanoTestnet qualified as TN
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map qualified as Map
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock.POSIX qualified as Time
import Hedgehog (MonadTest)
import Hedgehog qualified as H
import Helpers.Common (makeAddress)
import Helpers.Query qualified as Q
import Helpers.Test (TestParams (TestParams, localNodeConnectInfo, networkId, pparams, tempAbsPath))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.Utils qualified as U
import Plutus.V1.Ledger.Api qualified as PlutusV1
import Plutus.V1.Ledger.Interval qualified as PlutusV1
import Plutus.V1.Ledger.Time qualified as PlutusV1
import PlutusScripts.AlwaysSucceeds qualified as PS
import PlutusScripts.Helpers (toScriptData)
import PlutusScripts.Helpers qualified as PS
import PlutusScripts.V1TxInfo (checkV1TxInfoAssetIdV1, checkV1TxInfoMintWitnessV1, checkV1TxInfoRedeemer, txInfoData,
                               txInfoFee, txInfoInputs, txInfoMint, txInfoOutputs, txInfoSigs)

checkTxInfoV1Test :: (MonadIO m , MonadTest m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  POSIXTime ->
  m ()
checkTxInfoV1Test networkOptions TestParams{..} preTestnetTime = do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  startTime <- liftIO Time.getPOSIXTime
  (w1SKey, w1VKey, w1Address) <- TN.w1 tempAbsPath networkId

  -- build a transaction

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
  txInAsTxOut@(C.TxOut _ txInValue _ _) <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address txIn "txInAsTxOut <- getTxOutAtAddress"

  let
    tokenValues = C.valueFromList [(checkV1TxInfoAssetIdV1, 1)]
    executionUnits = C.ExecutionUnits {C.executionSteps = 1_000_000_000, C.executionMemory = 10_000_000 }
    collateral = Tx.txInsCollateral era [txIn]
    totalLovelace = C.txOutValueToLovelace txInValue
    fee = 2_000_000 :: C.Lovelace
    amountPaid = 10_000_000
    amountReturned = totalLovelace - amountPaid - fee
    datum = toScriptData (42 ::Integer)

    txOut1 = Tx.txOutWithDatumInTx era (C.lovelaceToValue amountPaid <> tokenValues) w1Address datum
    txOut2 = Tx.txOut era (C.lovelaceToValue amountReturned) w1Address

    lowerBound = PlutusV1.fromMilliSeconds
      $ PlutusV1.DiffMilliSeconds $ U.posixToMilliseconds preTestnetTime -- before slot 1
    upperBound = PlutusV1.fromMilliSeconds
      $ PlutusV1.DiffMilliSeconds $ U.posixToMilliseconds startTime + 600_000 -- ~10mins after slot 1 (to account for testnet init time)
    timeRange = PlutusV1.interval lowerBound upperBound :: PlutusV1.POSIXTimeRange

    expTxInfoInputs     = txInfoInputs (txIn, txInAsTxOut)
    expTxInfoOutputs    = txInfoOutputs [ txOut1, txOut2 ]
    expTxInfoFee        = txInfoFee fee
    expTxInfoMint       = txInfoMint tokenValues
    expDCert            = [] -- not testing any staking registration certificate
    expWdrl             = [] -- not testing any staking reward withdrawal
    expTxInfoSigs       = txInfoSigs [w1VKey]
    expTxInfoData       = txInfoData [datum]
    expTxInfoValidRange = timeRange

    redeemer = checkV1TxInfoRedeemer [expTxInfoInputs] expTxInfoOutputs expTxInfoFee expTxInfoMint
               expDCert expWdrl expTxInfoValidRange expTxInfoSigs expTxInfoData
    mintWitnesses = Map.fromList [checkV1TxInfoMintWitnessV1 era redeemer executionUnits]

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txInsCollateral = collateral
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut1, txOut2]
      , C.txFee = Tx.txFee era fee
      , C.txValidityRange = Tx.txValidityRange era 1 2700 -- ~9min range (200ms slots). Babbage era onwards cannot have upper slot beyond epoch boundary (10_000 slot epoch).
      , C.txExtraKeyWits = Tx.txExtraKeyWits era [w1VKey]
      }
  txbody <- Tx.buildRawTx era txBodyContent
  kw <- Tx.signTx era txbody w1SKey
  let signedTx = C.makeSignedTransaction [kw] txbody

  Tx.submitTx era localNodeConnectInfo signedTx

  let expectedTxIn = Tx.txIn (Tx.txId signedTx) 0
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "resultTxOut <- getTxOutAtAddress "
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
  H.assert txOutHasTokenValue

  H.success

-- tests spending outputs with datum hash both with and without datum value embedded in tx body
datumHashSpendTest :: (MonadIO m , MonadTest m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m ()
datumHashSpendTest networkOptions TestParams{..} = do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 tempAbsPath networkId

  -- build a transaction with two script outputs to be spent
  -- only one has its datum value embedded in the tx body

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    scriptAddress = makeAddress (Right PS.alwaysSucceedSpendScriptHashV1) networkId
    datum1         = PS.toScriptData (1 :: Integer)
    datum2         = PS.toScriptData (2 :: Integer)
    scriptTxOut1   = Tx.txOutWithDatumHash era (C.lovelaceToValue 5_000_000) scriptAddress datum1
    scriptTxOut2   = Tx.txOutWithDatumInTx era (C.lovelaceToValue 5_000_000) scriptAddress datum2
    otherTxOut    = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txOuts = [scriptTxOut1, scriptTxOut2, otherTxOut]
      }

  signedTx <- Tx.buildTx era txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let txInAtScript1  = Tx.txIn (Tx.txId signedTx) 0
      txInAtScript2  = Tx.txIn (Tx.txId signedTx) 1
      otherTxIn      = Tx.txIn (Tx.txId signedTx) 2
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript1 "waitForTxInAtAddress"

  -- build a transaction to mint token using reference script

  let
    scriptTxIn1 = Tx.txInWitness txInAtScript1 $ PS.alwaysSucceedSpendWitnessV1 era Nothing (Just datum1)
    scriptTxIn2 = Tx.txInWitness txInAtScript2 $ PS.alwaysSucceedSpendWitnessV1 era Nothing (Just datum2)
    collateral = Tx.txInsCollateral era [otherTxIn]
    adaValue = C.lovelaceToValue 4_200_000
    txOut = Tx.txOut era adaValue w1Address

    txBodyContent2 = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = [scriptTxIn1, scriptTxIn2]
      , C.txInsCollateral = collateral
      , C.txOuts = [txOut]
      }

  signedTx2 <- Tx.buildTx era txBodyContent2 w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx2
  let
    expectedTxIn1 = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains expected ada value
  resultTxOut1 <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn1 "resultTxOut1 <- getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut1 adaValue
  H.assert txOutHasAdaValue

  H.success

mintBurnTest :: (MonadTest m, MonadIO m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m ()
mintBurnTest networkOptions TestParams{..} = do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 tempAbsPath networkId

  -- build a transaction to mint tokens

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    tokenValues = C.valueFromList [(PS.alwaysSucceedAssetIdV1, 10)]
    mintWitnesses = Map.fromList [PS.alwaysSucceedMintWitnessV1 era Nothing]
    collateral = Tx.txInsCollateral era [txIn]
    txOut = Tx.txOut era (C.lovelaceToValue 10_000_000 <> tokenValues) w1Address
    otherTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txInsCollateral = collateral
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut, otherTxOut]
      }

  signedTx <- Tx.buildTx era txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let expectedTxIn = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn    = Tx.txIn (Tx.txId signedTx) 1
  -- Query for txo and assert it contains newly minted token
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "resultTxOut <- getTxOutAtAddress"
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
  H.assert txOutHasTokenValue

  -- build a transaction to burn tokens

  let
    txIn2 = expectedTxIn
    burnValue = C.valueFromList [(PS.alwaysSucceedAssetIdV1, -5)]
    tokenValues2 = C.valueFromList [(PS.alwaysSucceedAssetIdV1, 5)]
    collateral2 = Tx.txInsCollateral era [otherTxIn]
    txOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000 <> tokenValues2) w1Address

    txBodyContent2 = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn2]
      , C.txInsCollateral = collateral2
      , C.txMintValue = Tx.txMintValue era burnValue mintWitnesses
      , C.txOuts = [txOut2]
      }

  signedTx2 <- Tx.buildTx era txBodyContent2 w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn2 = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains tokens remaining after burn
  resultTxOut2 <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn2 "resultTxOut2 <- getTxOutAtAddress"
  txOutHasTokenValue2 <- Q.txOutHasValue resultTxOut2 tokenValues2
  H.assert txOutHasTokenValue2

  H.success

-- TODO: collateral contains non ada test
