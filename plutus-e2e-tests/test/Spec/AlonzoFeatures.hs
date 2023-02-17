{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra


module Spec.AlonzoFeatures (tests) where

import Cardano.Api qualified as C
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map qualified as Map
import Data.Time.Clock.POSIX qualified as Time
import Hedgehog qualified as H
import Hedgehog.Extras.Test qualified as HE
import Helpers (testnetOptionsAlonzo6, testnetOptionsBabbage7, testnetOptionsBabbage8)
import Helpers qualified as TN
import Plutus.V1.Ledger.Api qualified as PlutusV1
import Plutus.V1.Ledger.Interval qualified as PlutusV1
import Plutus.V1.Ledger.Time qualified as PlutusV1
import PlutusScripts.Helpers (toScriptData)
import PlutusScripts.V1TxInfo (checkV1TxInfoAssetIdV1, checkV1TxInfoMintWitnessV1, checkV1TxInfoRedeemer, txInfoData,
                               txInfoFee, txInfoInputs, txInfoMint, txInfoOutputs, txInfoSigs)
import Test.Base qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Testnet.Plutus qualified as TN

-- | Convert a 'POSIXTime' to the number of milliseconds since the Unix epoch.
posixToMilliseconds :: Time.POSIXTime -> Integer
posixToMilliseconds posixTime = round $ 1000 * (realToFrac posixTime :: Double)

tests :: TestTree
tests =
  testGroup
    "Alonzo Features"
    [ testProperty "check each attribute of V1 TxInfo in Alonzo PV6" (checkTxInfoV1 testnetOptionsAlonzo6)
    , testProperty "check each attribute of V1 TxInfo in Babbage PV7" (checkTxInfoV1 testnetOptionsBabbage7)
    , testProperty "check each attribute of V1 TxInfo in Babbage PV8" (checkTxInfoV1 testnetOptionsBabbage8)
    ]

checkTxInfoV1 :: Either TN.LocalNodeOptions TN.TestnetOptions -> H.Property
checkTxInfoV1 networkOptions = H.integration . HE.runFinallies . TN.workspace "." $ \tempAbsPath -> do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  preTestnetTime <- liftIO Time.getPOSIXTime

  -- 1: spin up a testnet or use local node connected to public testnet
  (localNodeConnectInfo, pparams, networkId) <- TN.setupTestEnvironment networkOptions tempAbsPath
  (w1SKey, w1VKey, w1Address) <- TN.w1 tempAbsPath networkId
  startTime <- liftIO Time.getPOSIXTime

  -- 2: build a transaction

  txIn <- TN.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
  txInAsTxOut@(C.TxOut _ txInValue _ _) <- TN.getTxOutAtAddress era localNodeConnectInfo w1Address txIn "txInAsTxOut <- TN.getTxOutAtAddress"

  let
    tokenValues = C.valueFromList [(checkV1TxInfoAssetIdV1, 1)]
    executionUnits = C.ExecutionUnits {C.executionSteps = 1_000_000_000, C.executionMemory = 10_000_000 }
    collateral = TN.txInsCollateral era [txIn]
    totalLovelace = C.txOutValueToLovelace txInValue
    fee = 2_000_000 :: C.Lovelace
    amountPaid = 10_000_000
    amountReturned = totalLovelace - amountPaid - fee
    datum = toScriptData (42 ::Integer)

    txOut1 = TN.txOutWithDatumInTx era (C.lovelaceToValue amountPaid <> tokenValues) w1Address datum
    txOut2 = TN.txOut era (C.lovelaceToValue amountReturned) w1Address

    lowerBound = PlutusV1.fromMilliSeconds
      $ PlutusV1.DiffMilliSeconds $ posixToMilliseconds preTestnetTime -- before slot 1
    upperBound = PlutusV1.fromMilliSeconds
      $ PlutusV1.DiffMilliSeconds $ posixToMilliseconds startTime + 600_000 -- ~10mins after slot 1 (to account for testnet init time)
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

    txBodyContent = (TN.emptyTxBodyContent era pparams)
      { C.txIns = TN.pubkeyTxIns [txIn]
      , C.txInsCollateral = collateral
      , C.txMintValue = TN.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut1, txOut2]
      , C.txFee = TN.txFee era fee
      , C.txValidityRange = TN.txValidityRange era 1 2700 -- ~9min range (200ms slots). Babbage era onwards cannot have upper slot beyond epoch boundary (10_000 slot epoch).
      , C.txExtraKeyWits = TN.txExtraKeyWits era [w1VKey]
      }
  txbody <- TN.buildRawTx era txBodyContent
  kw <- TN.signTx era txbody w1SKey
  let signedTx = C.makeSignedTransaction [kw] txbody

  TN.submitTx era localNodeConnectInfo signedTx

  let expectedTxIn = TN.txIn (TN.txId signedTx) 0
  resultTxOut <- TN.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "resultTxOut <- TN.getTxOutAtAddress "
  txOutHasTokenValue <- TN.txOutHasValue resultTxOut tokenValues
  H.assert txOutHasTokenValue
  H.success

-- TODO: datumHashSpendTest
-- TODO: mintTest
-- TODO: can't use V2 script test
