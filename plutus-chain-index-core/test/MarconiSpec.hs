{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module MarconiSpec where

import Control.Monad (replicateM)
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Ratio (Ratio, (%))
import Data.Set qualified as S
import Numeric.Natural (Natural)

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Cardano.Api
import Cardano.Api qualified as Api
import Cardano.Api.Shelley
import Gen.Cardano.Api.Metadata qualified as CGen
import Gen.Cardano.Api.Typed qualified as CGen
import Plutus.V1.Ledger.Api qualified as Plutus

import Marconi.Index.ScriptTx qualified as ScriptTx

tests :: TestTree
tests = testGroup "Marconi"
  [ testProperty "prop_script_hashes_in_tx_match" getTxBodyScriptsRoundtrip ]

getTxBodyScriptsRoundtrip :: Property
getTxBodyScriptsRoundtrip = property $ do
  nScripts <- forAll $ Gen.integral (Range.linear 5 500)
  AnyCardanoEra (era :: CardanoEra era) <- forAll $
    Gen.enum (AnyCardanoEra ShelleyEra) maxBound

  txIns <- replicateM nScripts $ forAll CGen.genTxIn
  witnessesHashes <- replicateM nScripts $ forAll $ genWitnessAndHashInEra era

  let (witnesses, scriptHashes) = unzip witnessesHashes
      collateral = case collateralSupportedInEra era of
        Just yes -> TxInsCollateral yes txIns
        _        -> TxInsCollateralNone

  txBody <- forAll $ genTxBodyWithTxIns era (zip txIns $ map BuildTxWith witnesses) collateral
  let hashesFound = map coerce $ ScriptTx.getTxBodyScripts txBody :: [ScriptHash]
  assert $ S.fromList scriptHashes == S.fromList hashesFound

genTxBodyWithTxIns
  :: IsCardanoEra era
  => CardanoEra era
  -> [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
  -> TxInsCollateral era
  -> Gen (TxBody era)
genTxBodyWithTxIns era txIns txInsCollateral = do
  txBodyContent <- genTxBodyContentWithTxInsCollateral era txIns txInsCollateral
  case makeTransactionBody txBodyContent of
    Left err     -> fail $ displayError err
    Right txBody -> pure txBody

genTxBodyContentWithTxInsCollateral
  :: CardanoEra era
  -> [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
  -> TxInsCollateral era
  -> Gen (TxBodyContent BuildTx era)
genTxBodyContentWithTxInsCollateral era txIns txInsCollateral = do
  txOuts <- Gen.list (Range.constant 1 10) (CGen.genTxOut era)
  txFee <- genTxFee era
  txValidityRange <- genTxValidityRange era
  txMetadata <- genTxMetadataInEra era
  txAuxScripts <- genTxAuxScripts era
  let txExtraKeyWits = TxExtraKeyWitnessesNone --TODO: Alonzo era: Generate witness key hashes
  txProtocolParams <- BuildTxWith . Just <$> genProtocolParameters
  txWithdrawals <- genTxWithdrawals era
  txCertificates <- genTxCertificates era
  txUpdateProposal <- genTxUpdateProposal era
  txMintValue <- genTxMintValue era
  txScriptValidity <- genTxScriptValidity era

  pure $ TxBodyContent
    { Api.txIns
    , Api.txInsCollateral
    , Api.txOuts
    , Api.txFee
    , Api.txValidityRange
    , Api.txMetadata
    , Api.txAuxScripts
    , Api.txExtraKeyWits
    , Api.txProtocolParams
    , Api.txWithdrawals
    , Api.txCertificates
    , Api.txUpdateProposal
    , Api.txMintValue
    , Api.txScriptValidity
    }

genWitnessAndHashInEra :: CardanoEra era -> Gen (Witness WitCtxTxIn era, ScriptHash)
genWitnessAndHashInEra era = do
  ScriptInEra scriptLanguageInEra script <- CGen.genScriptInEra era
  witness :: Witness WitCtxTxIn era1 <- ScriptWitness ScriptWitnessForSpending <$> case script of
    PlutusScript version plutusScript -> do
      scriptData <- CGen.genScriptData
      executionUnits <- genExecutionUnits
      pure $ PlutusScriptWitness
        scriptLanguageInEra
        version
        plutusScript
        (ScriptDatumForTxIn scriptData)
        scriptData
        executionUnits
    SimpleScript version simpleScript ->
      pure $ SimpleScriptWitness scriptLanguageInEra version simpleScript
  pure (witness, hashScript script)

-- * Copy-paste

-- | The following code is copied (including comments) from
-- cardano-node commit 2b1d18c6c7b7142d9eebfec34da48840ed4409b6 (what
-- plutus-apps main depends on)
-- cardano-api/gen/Gen/Cardano/Api/Typed.hs

type TxIns build era = [(TxIn, BuildTxWith build (Witness WitCtxTxIn era))]
-- ^ From /cardano-node/cardano-api/src/Cardano/Api/TxBody.hs

panic :: String -> a
panic = error

genTxScriptValidity :: CardanoEra era -> Gen (TxScriptValidity era)
genTxScriptValidity era = case txScriptValiditySupportedInCardanoEra era of
  Nothing      -> pure TxScriptValidityNone
  Just witness -> TxScriptValidity witness <$> genScriptValidity

genScriptValidity :: Gen ScriptValidity
genScriptValidity = Gen.element [ScriptInvalid, ScriptValid]

genTxMintValue :: CardanoEra era -> Gen (TxMintValue BuildTx era)
genTxMintValue era =
  case multiAssetSupportedInEra era of
    Left _ -> pure TxMintNone
    Right supported ->
      Gen.choice
        [ pure TxMintNone
        , TxMintValue supported <$> CGen.genValueForMinting <*> return (BuildTxWith mempty)
        ]

genTxUpdateProposal :: CardanoEra era -> Gen (TxUpdateProposal era)
genTxUpdateProposal era =
  case updateProposalSupportedInEra era of
    Nothing -> pure TxUpdateProposalNone
    Just supported ->
      Gen.choice
        [ pure TxUpdateProposalNone
        , TxUpdateProposal supported <$> CGen.genUpdateProposal
        ]

genTxCertificates :: CardanoEra era -> Gen (TxCertificates BuildTx era)
genTxCertificates era =
  case certificatesSupportedInEra era of
    Nothing -> pure TxCertificatesNone
    Just supported -> do
      certs <- Gen.list (Range.constant 0 3) CGen.genCertificate
      Gen.choice
        [ pure TxCertificatesNone
        , pure (TxCertificates supported certs $ BuildTxWith mempty)
          -- TODO: Generate certificates
        ]

genTxWithdrawals :: CardanoEra era -> Gen (TxWithdrawals BuildTx era)
genTxWithdrawals era =
  case withdrawalsSupportedInEra era of
    Nothing -> pure TxWithdrawalsNone
    Just supported ->
      Gen.choice
        [ pure TxWithdrawalsNone
        , pure (TxWithdrawals supported mempty)
          -- TODO: Generate withdrawals
        ]

genTxAuxScripts :: CardanoEra era -> Gen (TxAuxScripts era)
genTxAuxScripts era =
  case auxScriptsSupportedInEra era of
    Nothing -> pure TxAuxScriptsNone
    Just supported ->
      TxAuxScripts supported <$>
        Gen.list (Range.linear 0 3)
                 (CGen.genScriptInEra era)

genTxMetadataInEra :: CardanoEra era -> Gen (TxMetadataInEra era)
genTxMetadataInEra era =
  case txMetadataSupportedInEra era of
    Nothing -> pure TxMetadataNone
    Just supported ->
      Gen.choice
        [ pure TxMetadataNone
        , TxMetadataInEra supported <$> CGen.genTxMetadata
        ]

genTxValidityRange
  :: CardanoEra era
  -> Gen (TxValidityLowerBound era, TxValidityUpperBound era)
genTxValidityRange era =
  (,)
    <$> genTxValidityLowerBound era
    <*> genTxValidityUpperBound era

-- TODO: Accept a range for generating ttl.
genTxValidityUpperBound :: CardanoEra era -> Gen (TxValidityUpperBound era)
genTxValidityUpperBound era =
  case (validityUpperBoundSupportedInEra era,
       validityNoUpperBoundSupportedInEra era) of
    (Just supported, _) ->
      TxValidityUpperBound supported <$> genTtl

    (Nothing, Just supported) ->
      pure (TxValidityNoUpperBound supported)

    (Nothing, Nothing) ->
      panic "genTxValidityUpperBound: unexpected era support combination"

genTtl :: Gen SlotNo
genTtl = genSlotNo

genSlotNo :: Gen SlotNo
genSlotNo = SlotNo <$> Gen.word64 Range.constantBounded

-- TODO: Accept a range for generating ttl.
genTxValidityLowerBound :: CardanoEra era -> Gen (TxValidityLowerBound era)
genTxValidityLowerBound era =
  case validityLowerBoundSupportedInEra era of
    Nothing        -> pure TxValidityNoLowerBound
    Just supported -> TxValidityLowerBound supported <$> genTtl

genTxFee :: CardanoEra era -> Gen (TxFee era)
genTxFee era =
  case txFeesExplicitInEra era of
    Left  supported -> pure (TxFeeImplicit supported)
    Right supported -> TxFeeExplicit supported <$> CGen.genLovelace

genTxInsCollateral :: CardanoEra era -> Gen (TxInsCollateral era)
genTxInsCollateral era =
    case collateralSupportedInEra era of
      Nothing        -> pure TxInsCollateralNone
      Just supported -> Gen.choice
                          [ pure TxInsCollateralNone
                          , TxInsCollateral supported <$> Gen.list (Range.linear 0 10) CGen.genTxIn
                          ]

genExecutionUnits :: Gen ExecutionUnits
genExecutionUnits = ExecutionUnits <$> Gen.integral (Range.constant 0 1000)
                                   <*> Gen.integral (Range.constant 0 1000)

genNat :: Gen Natural
genNat = Gen.integral (Range.linear 0 10)

genProtocolParameters :: Gen ProtocolParameters
genProtocolParameters =
  ProtocolParameters
    <$> ((,) <$> genNat <*> genNat)
    <*> CGen.genRational
    <*> CGen.genMaybePraosNonce
    <*> genNat
    <*> genNat
    <*> genNat
    <*> genNat
    <*> genNat
    <*> Gen.maybe CGen.genLovelace
    <*> CGen.genLovelace
    <*> CGen.genLovelace
    <*> CGen.genLovelace
    <*> genEpochNo
    <*> genNat
    <*> genRationalInt64
    <*> CGen.genRational
    <*> CGen.genRational
    <*> (Just <$> CGen.genLovelace)
    <*> genCostModels
    <*> (Just <$> genExecutionUnitPrices)
    <*> (Just <$> genExecutionUnits)
    <*> (Just <$> genExecutionUnits)
    <*> (Just <$> genNat)
    <*> (Just <$> genNat)
    <*> (Just <$> genNat)

genExecutionUnitPrices :: Gen ExecutionUnitPrices
genExecutionUnitPrices = ExecutionUnitPrices <$> CGen.genRational <*> CGen.genRational

-- TODO: consolidate this back to just genRational once this is merged:
-- https://github.com/input-output-hk/cardano-ledger-specs/pull/2330
genRationalInt64 :: Gen Rational
genRationalInt64 =
    (\d -> ratioToRational (1 % d)) <$> genDenominator
  where
    genDenominator :: Gen Int64
    genDenominator = Gen.integral (Range.linear 1 maxBound)

    ratioToRational :: Ratio Int64 -> Rational
    ratioToRational = toRational

genEpochNo :: Gen EpochNo
genEpochNo = EpochNo <$> Gen.word64 (Range.linear 0 10)

genCostModels :: Gen (Map AnyPlutusScriptVersion CostModel)
genCostModels =
    Gen.map (Range.linear 0 (length plutusScriptVersions))
            ((,) <$> Gen.element plutusScriptVersions
                 <*> genCostModel)
  where
    plutusScriptVersions :: [AnyPlutusScriptVersion]
    plutusScriptVersions = [minBound..maxBound]

genCostModel :: Gen CostModel
genCostModel = case Plutus.defaultCostModelParams of
  Nothing -> panic "Plutus defaultCostModelParams is broken."
  Just dcm ->
      CostModel
    -- TODO This needs to be the cost model struct for whichever
    -- Plutus version we're using, once we support multiple Plutus versions.
    <$> mapM (const $ Gen.integral (Range.linear 0 5000)) dcm
