{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra


module Spec.AlonzoFeatures (tests) where

import Cardano.Api qualified as C
import CardanoTestnet qualified as TN
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map qualified as Map
import Data.Time.Clock.POSIX qualified as Time
import Hedgehog qualified as H
import Hedgehog.Extras.Test qualified as HE
import Helpers.Query qualified as Q
import Helpers.Testnet (testnetOptionsAlonzo6, testnetOptionsBabbage7, testnetOptionsBabbage8)
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.Utils qualified as U (anyLeftFail_, posixToMilliseconds, workspace)
import Plutus.V1.Ledger.Api qualified as PlutusV1
import Plutus.V1.Ledger.Interval qualified as PlutusV1
import Plutus.V1.Ledger.Time qualified as PlutusV1
import PlutusScripts.Helpers (toScriptData)
import PlutusScripts.V1TxInfo (checkV1TxInfoAssetIdV1, checkV1TxInfoMintWitnessV1, checkV1TxInfoRedeemer, txInfoData,
                               txInfoFee, txInfoInputs, txInfoMint, txInfoOutputs, txInfoSigs)
import Test.Base qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Alonzo Features"
    [ testProperty "check each attribute of V1 TxInfo in Alonzo PV6" (checkTxInfoV1Test testnetOptionsAlonzo6)
    , testProperty "check each attribute of V1 TxInfo in Babbage PV7" (checkTxInfoV1Test testnetOptionsBabbage7)
    , testProperty "check each attribute of V1 TxInfo in Babbage PV8" (checkTxInfoV1Test testnetOptionsBabbage8)
    ]

checkTxInfoV1Test :: Either TN.LocalNodeOptions TN.TestnetOptions -> H.Property
checkTxInfoV1Test networkOptions = H.integration . HE.runFinallies . U.workspace "." $ \tempAbsPath -> do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  preTestnetTime <- liftIO Time.getPOSIXTime

  -- 1: spin up a testnet or use local node connected to public testnet
  (localNodeConnectInfo, pparams, networkId, mPoolNodes) <- TN.setupTestEnvironment networkOptions tempAbsPath
  (w1SKey, w1VKey, w1Address) <- TN.w1 tempAbsPath networkId
  startTime <- liftIO Time.getPOSIXTime

  -- 2: build a transaction

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
  txInAsTxOut@(C.TxOut _ txInValue _ _) <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address txIn "txInAsTxOut <- TN.getTxOutAtAddress"

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
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "resultTxOut <- TN.getTxOutAtAddress "
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
  H.assert txOutHasTokenValue

  U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

  H.success

-- TODO: datumHashSpendTest
