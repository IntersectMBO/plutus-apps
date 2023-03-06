{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Spec.BabbageFeatures(tests) where

import Cardano.Api qualified as C
import Data.Map qualified as Map
import Test.Tasty (TestTree, testGroup)

import Hedgehog qualified as H
import Hedgehog.Extras.Test qualified as HE
import Test.Base qualified as H
import Test.Tasty.Hedgehog (testProperty)

import CardanoTestnet qualified as TN
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock.POSIX qualified as Time
import Helpers.Common (makeAddress)
import Helpers.Query qualified as Q
import Helpers.Testnet (testnetOptionsBabbage7, testnetOptionsBabbage8)
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.Utils qualified as U (anyLeftFail_, posixToMilliseconds, workspace)
import Plutus.V1.Ledger.Interval qualified as PlutusV1
import Plutus.V1.Ledger.Time qualified as PlutusV1
import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusScripts.AlwaysSucceeds (alwaysSucceedMintWitnessV2', alwaysSucceedPolicyTxInfoRedeemerV2)
import PlutusScripts.AlwaysSucceeds qualified as PS
import PlutusScripts.Helpers (toScriptData)
import PlutusScripts.Helpers qualified as PS
import PlutusScripts.V2TxInfo (checkV2TxInfoAssetIdV2, checkV2TxInfoMintWitnessV2, checkV2TxInfoRedeemer, txInfoData,
                               txInfoFee, txInfoInputs, txInfoMint, txInfoOutputs, txInfoSigs)

tests :: TestTree
tests = testGroup "Babbage Features"
  [ testGroup "reference script"
    [ testProperty "mint a token with a reference script in Babbage PV7" (referenceScriptMintTest testnetOptionsBabbage7)
    , testProperty "mint a token with a reference script in Babbage PV8" (referenceScriptMintTest testnetOptionsBabbage8)
    --, testProperty "mint a token with a reference script in Babbage PV8" (referenceScriptMintTest localNodeOptionsPreview) -- uncomment to use local node on preview testnet

    , testProperty "spend locked funds with a reference script using inline datum in Babbage PV7" (referenceScriptInlineDatumSpendTest testnetOptionsBabbage7)
    , testProperty "spend locked funds with a reference script using inline datum in Babbage PV8" (referenceScriptInlineDatumSpendTest testnetOptionsBabbage8)
    --, testProperty "spend locked funds with a reference script using inline datum in Babbage PV8" (referenceScriptInlineDatumSpendTest localNodeOptionsPreview) -- uncomment to use local node on preview testnet

    , testProperty "spend locked funds with a reference script providing datum in txbody in Babbage PV7" (referenceScriptDatumHashSpendTest testnetOptionsBabbage7)
    , testProperty "spend locked funds with a reference script providing datum in txbody in Babbage PV8" (referenceScriptDatumHashSpendTest testnetOptionsBabbage8)
    --, testProperty "spend locked funds with a reference script providing datum in txbody in Babbage PV8" (referenceScriptDatumHashSpendTest localNodeOptionsPreview) -- uncomment to use local node on preview testnet

    , testProperty "check each attribute of V2 TxInfo in Babbage PV7" (checkTxInfoV2Test testnetOptionsBabbage7)
    , testProperty "check each attribute of V2 TxInfo in Babbage PV8" (checkTxInfoV2Test testnetOptionsBabbage8)
    ]
  ]

referenceScriptMintTest :: Either TN.LocalNodeOptions TN.TestnetOptions -> H.Property
referenceScriptMintTest networkOptions = H.integration . HE.runFinallies . U.workspace "." $ \tempAbsPath -> do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions

  -- 1: spin up a testnet or use local node connected to public testnet
  (localNodeConnectInfo, pparams, networkId, mPoolNodes) <- TN.setupTestEnvironment networkOptions tempAbsPath
  (w1SKey, _, w1Address) <- TN.w1 tempAbsPath networkId

  -- 2: build a transaction to hold reference script

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    refScriptTxOut = Tx.txOutWithRefScript era (C.lovelaceToValue 20_000_000) w1Address
      (PS.unPlutusScriptV2 PS.alwaysSucceedPolicyScriptV2)
    otherTxOut = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txOuts = [refScriptTxOut, otherTxOut]
      }

  signedTx <- Tx.buildTx era txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let refScriptTxIn = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn   = Tx.txIn (Tx.txId signedTx) 1
  Q.waitForTxInAtAddress era localNodeConnectInfo w1Address refScriptTxIn "Tx.waitForTxInAtAddress"

  -- 3: build a transaction to mint token using reference script

  let
    tokenValues = C.valueFromList [(PS.alwaysSucceedAssetIdV2, 6)]
    mintWitnesses = Map.fromList [PS.alwaysSucceedMintWitnessV2 era (Just refScriptTxIn)]
    collateral = Tx.txInsCollateral era [otherTxIn]
    txOut = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address

    txBodyContent2 = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [otherTxIn]
      , C.txInsCollateral = collateral
      , C.txInsReference = Tx.txInsReference era [refScriptTxIn]
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut]
      }

  signedTx2 <- Tx.buildTx era txBodyContent2 w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "Tx.getTxOutAtAddress"
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
  H.assert txOutHasTokenValue

  U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes
  H.success


referenceScriptInlineDatumSpendTest :: Either TN.LocalNodeOptions TN.TestnetOptions -> H.Property
referenceScriptInlineDatumSpendTest networkOptions = H.integration . HE.runFinallies . U.workspace "." $ \tempAbsPath -> do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions

  -- 1: spin up a testnet or use local node connected to public testnet
  (localNodeConnectInfo, pparams, networkId, mPoolNodes) <- TN.setupTestEnvironment networkOptions tempAbsPath
  (w1SKey, _, w1Address) <- TN.w1 tempAbsPath networkId

  -- 2: build a transaction to hold reference script

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    refTxOut      = Tx.txOutWithRefScript era (C.lovelaceToValue 20_000_000) w1Address
                     (PS.unPlutusScriptV2 PS.alwaysSucceedSpendScriptV2)
    otherTxOut    = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address
    scriptAddress = makeAddress (Right PS.alwaysSucceedSpendScriptHashV2) networkId
    scriptTxOut   = Tx.txOutWithInlineDatum era (C.lovelaceToValue 10_000_000) scriptAddress (PS.toScriptData ())

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txOuts = [refTxOut, otherTxOut, scriptTxOut]
      }

  signedTx <- Tx.buildTx era txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let refScriptTxIn = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn     = Tx.txIn (Tx.txId signedTx) 1
      txInAtScript  = Tx.txIn (Tx.txId signedTx) 2
  Q.waitForTxInAtAddress era localNodeConnectInfo w1Address refScriptTxIn "Tx.waitForTxInAtAddress"

  -- 3: build a transaction to mint token using reference script

  let
    scriptTxIn = Tx.txInWitness txInAtScript (PS.alwaysSucceedSpendWitnessV2 era (Just refScriptTxIn) Nothing)
    collateral = Tx.txInsCollateral era [otherTxIn]
    adaValue = C.lovelaceToValue 4_200_000
    txOut = Tx.txOut era adaValue w1Address

    txBodyContent2 = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = [scriptTxIn]
      , C.txInsReference = Tx.txInsReference era [refScriptTxIn]
      , C.txInsCollateral = collateral
      , C.txOuts = [txOut]
      }

  signedTx2 <- Tx.buildTx era txBodyContent2 w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "Tx.getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut adaValue
  H.assert txOutHasAdaValue

  U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes
  H.success


referenceScriptDatumHashSpendTest :: Either TN.LocalNodeOptions TN.TestnetOptions -> H.Property
referenceScriptDatumHashSpendTest networkOptions = H.integration . HE.runFinallies . U.workspace "." $ \tempAbsPath -> do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions

  -- 1: spin up a testnet or use local node connected to public testnet
  (localNodeConnectInfo, pparams, networkId, mPoolNodes) <- TN.setupTestEnvironment networkOptions tempAbsPath
  (w1SKey, _, w1Address) <- TN.w1 tempAbsPath networkId

  -- 2: build a transaction to hold reference script

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    refTxOut      = Tx.txOutWithRefScript era (C.lovelaceToValue 20_000_000) w1Address
                     (PS.unPlutusScriptV2 PS.alwaysSucceedSpendScriptV2)
    otherTxOut    = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address
    scriptAddress = makeAddress (Right PS.alwaysSucceedSpendScriptHashV2) networkId
    datum         = PS.toScriptData ()
    scriptTxOut   = Tx.txOutWithDatumHash era (C.lovelaceToValue 10_000_000) scriptAddress datum

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txOuts = [refTxOut, otherTxOut, scriptTxOut]
      }

  signedTx <- Tx.buildTx era txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let refScriptTxIn = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn     = Tx.txIn (Tx.txId signedTx) 1
      txInAtScript  = Tx.txIn (Tx.txId signedTx) 2
  Q.waitForTxInAtAddress era localNodeConnectInfo w1Address refScriptTxIn "Tx.waitForTxInAtAddress"

  -- 3: build a transaction to mint token using reference script

  let
    scriptTxIn = Tx.txInWitness txInAtScript $ PS.alwaysSucceedSpendWitnessV2 era (Just refScriptTxIn) (Just datum)
    collateral = Tx.txInsCollateral era [otherTxIn]
    adaValue = C.lovelaceToValue 4_200_000
    txOut = Tx.txOut era adaValue w1Address

    txBodyContent2 = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = [scriptTxIn]
      , C.txInsReference = Tx.txInsReference era [refScriptTxIn]
      , C.txInsCollateral = collateral
      , C.txOuts = [txOut]
      }

  signedTx2 <- Tx.buildTx era txBodyContent2 w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "Tx.getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut adaValue
  H.assert txOutHasAdaValue

  U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes
  H.success

checkTxInfoV2Test :: Either TN.LocalNodeOptions TN.TestnetOptions -> H.Property
checkTxInfoV2Test networkOptions = H.integration . HE.runFinallies . U.workspace "." $ \tempAbsPath -> do

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
    tokenValues = C.valueFromList [(checkV2TxInfoAssetIdV2, 1), (PS.alwaysSucceedAssetIdV2, 2)]
    executionUnits1 = C.ExecutionUnits {C.executionSteps = 1_000_000_000, C.executionMemory = 10_000_000 }
    executionUnits2 = C.ExecutionUnits {C.executionSteps = 1_000_000_000, C.executionMemory = 4_000_000 }
    collateral = Tx.txInsCollateral era [txIn]
    totalLovelace = C.txOutValueToLovelace txInValue
    fee = 2_500_000 :: C.Lovelace
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

    expTxInfoInputs          = txInfoInputs (txIn, txInAsTxOut)
    expTxInfoReferenceInputs = txInfoInputs (txIn, txInAsTxOut)
    expTxInfoOutputs         = txInfoOutputs [ txOut1, txOut2 ]
    expTxInfoFee             = txInfoFee fee
    expTxInfoMint            = txInfoMint tokenValues
    expDCert                 = []                   -- not testing any staking registration certificate
    expWdrl                  = PlutusV2.fromList [] -- not testing any staking reward withdrawal
    expTxInfoSigs            = txInfoSigs [w1VKey]
    expTxInfoRedeemers       = alwaysSucceedPolicyTxInfoRedeemerV2
    expTxInfoData            = txInfoData [datum]
    expTxInfoValidRange      = timeRange

    redeemer = checkV2TxInfoRedeemer [expTxInfoInputs] [expTxInfoReferenceInputs] expTxInfoOutputs expTxInfoFee expTxInfoMint
               expDCert expWdrl expTxInfoValidRange expTxInfoSigs expTxInfoRedeemers expTxInfoData
    mintWitnesses = Map.fromList [checkV2TxInfoMintWitnessV2 era redeemer executionUnits1, alwaysSucceedMintWitnessV2' era executionUnits2]

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txInsReference = Tx.txInsReference era [txIn]
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

  -- TODO: inlineDatumSpendTest (no reference script)
