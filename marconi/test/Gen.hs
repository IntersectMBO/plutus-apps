module Gen where

import Data.Int (Int64)
import Data.Map qualified as Map
import Data.Ratio (Ratio, (%))
import Numeric.Natural (Natural)

import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
import Gen.Cardano.Api.Metadata qualified as CGen
import Gen.Cardano.Api.Typed qualified as CGen
import Plutus.V1.Ledger.Api qualified as Plutus

-- * Copy-paste

-- | The following code is copied (including comments) from
-- cardano-node commit 2b1d18c6c7b7142d9eebfec34da48840ed4409b6 (what
-- plutus-apps main depends on)
-- cardano-api/gen/Gen/Cardano/Api/Typed.hs

panic :: String -> a
panic = error

genTxScriptValidity :: C.CardanoEra era -> Gen (C.TxScriptValidity era)
genTxScriptValidity era = case C.txScriptValiditySupportedInCardanoEra era of
  Nothing      -> pure C.TxScriptValidityNone
  Just witness -> C.TxScriptValidity witness <$> genScriptValidity

genScriptValidity :: Gen C.ScriptValidity
genScriptValidity = Gen.element [C.ScriptInvalid, C.ScriptValid]

genTxMintValue :: C.CardanoEra era -> Gen (C.TxMintValue C.BuildTx era)
genTxMintValue era =
  case C.multiAssetSupportedInEra era of
    Left _ -> pure C.TxMintNone
    Right supported ->
      Gen.choice
        [ pure C.TxMintNone
        , C.TxMintValue supported <$> CGen.genValueForMinting <*> return (C.BuildTxWith mempty)
        ]

genTxUpdateProposal :: C.CardanoEra era -> Gen (C.TxUpdateProposal era)
genTxUpdateProposal era =
  case C.updateProposalSupportedInEra era of
    Nothing -> pure C.TxUpdateProposalNone
    Just supported ->
      Gen.choice
        [ pure C.TxUpdateProposalNone
        , C.TxUpdateProposal supported <$> CGen.genUpdateProposal
        ]

genTxCertificates :: C.CardanoEra era -> Gen (C.TxCertificates C.BuildTx era)
genTxCertificates era =
  case C.certificatesSupportedInEra era of
    Nothing -> pure C.TxCertificatesNone
    Just supported -> do
      certs <- Gen.list (Range.constant 0 3) CGen.genCertificate
      Gen.choice
        [ pure C.TxCertificatesNone
        , pure (C.TxCertificates supported certs $ C.BuildTxWith mempty)
          -- TODO: Generate certificates
        ]

genTxWithdrawals :: C.CardanoEra era -> Gen (C.TxWithdrawals C.BuildTx era)
genTxWithdrawals era =
  case C.withdrawalsSupportedInEra era of
    Nothing -> pure C.TxWithdrawalsNone
    Just supported ->
      Gen.choice
        [ pure C.TxWithdrawalsNone
        , pure (C.TxWithdrawals supported mempty)
          -- TODO: Generate withdrawals
        ]

genTxAuxScripts :: C.CardanoEra era -> Gen (C.TxAuxScripts era)
genTxAuxScripts era =
  case C.auxScriptsSupportedInEra era of
    Nothing -> pure C.TxAuxScriptsNone
    Just supported ->
      C.TxAuxScripts supported <$>
        Gen.list (Range.linear 0 3)
                 (CGen.genScriptInEra era)

genTxMetadataInEra :: C.CardanoEra era -> Gen (C.TxMetadataInEra era)
genTxMetadataInEra era =
  case C.txMetadataSupportedInEra era of
    Nothing -> pure C.TxMetadataNone
    Just supported ->
      Gen.choice
        [ pure C.TxMetadataNone
        , C.TxMetadataInEra supported <$> CGen.genTxMetadata
        ]

genTxValidityRange
  :: C.CardanoEra era
  -> Gen (C.TxValidityLowerBound era, C.TxValidityUpperBound era)
genTxValidityRange era =
  (,)
    <$> genTxValidityLowerBound era
    <*> genTxValidityUpperBound era

-- TODO: Accept a range for generating ttl.
genTxValidityUpperBound :: C.CardanoEra era -> Gen (C.TxValidityUpperBound era)
genTxValidityUpperBound era =
  case (C.validityUpperBoundSupportedInEra era,
       C.validityNoUpperBoundSupportedInEra era) of
    (Just supported, _) ->
      C.TxValidityUpperBound supported <$> genTtl

    (Nothing, Just supported) ->
      pure (C.TxValidityNoUpperBound supported)

    (Nothing, Nothing) ->
      panic "genTxValidityUpperBound: unexpected era support combination"

genTtl :: Gen C.SlotNo
genTtl = genSlotNo

genSlotNo :: Gen C.SlotNo
genSlotNo = C.SlotNo <$> Gen.word64 Range.constantBounded

-- TODO: Accept a range for generating ttl.
genTxValidityLowerBound :: C.CardanoEra era -> Gen (C.TxValidityLowerBound era)
genTxValidityLowerBound era =
  case C.validityLowerBoundSupportedInEra era of
    Nothing        -> pure C.TxValidityNoLowerBound
    Just supported -> C.TxValidityLowerBound supported <$> genTtl

genTxFee :: C.CardanoEra era -> Gen (C.TxFee era)
genTxFee era =
  case C.txFeesExplicitInEra era of
    Left  supported -> pure (C.TxFeeImplicit supported)
    Right supported -> C.TxFeeExplicit supported <$> CGen.genLovelace

genExecutionUnits :: Gen C.ExecutionUnits
genExecutionUnits = C.ExecutionUnits <$> Gen.integral (Range.constant 0 1000)
                                   <*> Gen.integral (Range.constant 0 1000)

genNat :: Gen Natural
genNat = Gen.integral (Range.linear 0 10)

genProtocolParameters :: Gen Shelley.ProtocolParameters
genProtocolParameters =
  Shelley.ProtocolParameters
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

genExecutionUnitPrices :: Gen C.ExecutionUnitPrices
genExecutionUnitPrices = C.ExecutionUnitPrices <$> CGen.genRational <*> CGen.genRational

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

genEpochNo :: Gen C.EpochNo
genEpochNo = C.EpochNo <$> Gen.word64 (Range.linear 0 10)

genCostModels :: Gen (Map.Map C.AnyPlutusScriptVersion C.CostModel)
genCostModels =
    Gen.map (Range.linear 0 (length plutusScriptVersions))
            ((,) <$> Gen.element plutusScriptVersions
                 <*> genCostModel)
  where
    plutusScriptVersions :: [C.AnyPlutusScriptVersion]
    plutusScriptVersions = [minBound..maxBound]

genCostModel :: Gen C.CostModel
genCostModel = case Plutus.defaultCostModelParams of
  Nothing -> panic "Plutus defaultCostModelParams is broken."
  Just dcm ->
      C.CostModel
    -- TODO This needs to be the cost model struct for whichever
    -- Plutus version we're using, once we support multiple Plutus versions.
    <$> mapM (const $ Gen.integral (Range.linear 0 5000)) dcm
