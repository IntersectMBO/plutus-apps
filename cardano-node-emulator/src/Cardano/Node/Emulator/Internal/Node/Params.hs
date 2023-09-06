{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TupleSections      #-}
-- | The set of parameters, like protocol parameters and slot configuration.
module Cardano.Node.Emulator.Internal.Node.Params (
  Params(..),
  paramsWithProtocolsParameters,
  slotConfigL,
  emulatorPParamsL,
  pProtocolParams,
  pParamsFromProtocolParams,
  bundledProtocolParameters,
  protocolParamsL,
  networkIdL,
  increaseTransactionLimits,
  increaseTransactionLimits',
  genesisDefaultsFromParams,
  -- * cardano-ledger specific types and conversion functions
  EmulatorEra,
  PParams,
  slotLength,
  testnet,
  emulatorEpochSize,
  emulatorGlobals,
  emulatorEraHistory
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.PParams qualified as C
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.PParams qualified as C
import Cardano.Ledger.BaseTypes (boundRational)
import Cardano.Ledger.Core qualified as C
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API (Coin (Coin), Globals, ShelleyGenesis, mkShelleyGlobals)
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Cardano.Ledger.Slot (EpochSize (EpochSize))
import Cardano.Node.Emulator.Internal.Node.TimeSlot (SlotConfig (scSlotLength, scSlotZeroTime),
                                                     posixTimeToNominalDiffTime, posixTimeToUTCTime)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SlotLength, mkSlotLength)
import Control.Lens (Lens', lens, makeLensesFor, over, (&), (.~))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object), (.:), (.=))
import Data.Aeson qualified as JSON
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Default (Default (def))
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Ratio ((%))
import Data.SOP.Counting qualified as Ouroboros
import Data.SOP.Strict (K (K), NP (Nil, (:*)))
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Ledger.Test (testnet)
import Ouroboros.Consensus.HardFork.History qualified as Ouroboros
import Plutus.Script.Utils.Scripts (Language (PlutusV3))
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCostModelParams)
import PlutusLedgerApi.V1 (POSIXTime (POSIXTime))
import Prettyprinter (Pretty (pretty), viaShow, vsep, (<+>))

-- | The default era for the emulator
type EmulatorEra = BabbageEra StandardCrypto

type PParams = C.PParams EmulatorEra

data Params = Params
  { pSlotConfig     :: !SlotConfig
  -- | Convert `Params` to cardano-ledger `PParams`
  , emulatorPParams :: !PParams
  , pNetworkId      :: !C.NetworkId
  }
  deriving (Eq, Show, Generic)

instance ToJSON C.NetworkId where
  toJSON C.Mainnet                      = JSON.String "Mainnet"
  toJSON (C.Testnet (C.NetworkMagic n)) = JSON.Number $ fromIntegral n
instance FromJSON C.NetworkId where
  parseJSON (JSON.String "Mainnet") = pure C.Mainnet
  parseJSON (JSON.Number n)         = pure $ C.Testnet $ C.NetworkMagic $ truncate n
  parseJSON v                       = prependFailure "parsing NetworkId failed, " (typeMismatch "'Mainnet' or Number" v)

deriving newtype instance ToJSON C.NetworkMagic
deriving newtype instance FromJSON C.NetworkMagic

makeLensesFor
  [ ("pSlotConfig", "slotConfigL")
  , ("emulatorPParams", "emulatorPParamsL")
  , ("pNetworkId", "networkIdL") ]
  ''Params

pProtocolParams :: Params -> C.ProtocolParameters
pProtocolParams p = C.fromLedgerPParams C.ShelleyBasedEraBabbage $ emulatorPParams p

pParamsFromProtocolParams :: C.ProtocolParameters -> PParams
pParamsFromProtocolParams = either (error . show) id . C.toLedgerPParams C.ShelleyBasedEraBabbage

bundledProtocolParameters :: Params -> C.BundledProtocolParameters C.BabbageEra
bundledProtocolParameters params = C.BundleAsShelleyBasedProtocolParameters
  C.ShelleyBasedEraBabbage
  (pProtocolParams params)
  (emulatorPParams params)

paramsWithProtocolsParameters :: SlotConfig -> C.ProtocolParameters -> C.NetworkId -> Params
paramsWithProtocolsParameters sc p = Params sc (pParamsFromProtocolParams p)

protocolParamsL :: Lens' Params C.ProtocolParameters
protocolParamsL = let
  set p pParam = p & emulatorPParamsL .~ pParamsFromProtocolParams pParam
  in lens pProtocolParams set

instance ToJSON Params where
  toJSON p = JSON.object
    [ "pSlotConfig" .= toJSON (pSlotConfig p)
    , "pProtocolParams" .= toJSON (pProtocolParams p)
    , "pNetworkId" .= toJSON (pNetworkId p)
    ]

instance FromJSON Params where
  parseJSON (Object v) = Params
    <$> (v .: "pSlotConfig" >>= parseJSON)
    <*> (pParamsFromProtocolParams <$> (v .: "pProtocolParams" >>= parseJSON))
    <*> (v .: "pNetworkId" >>= parseJSON)
  parseJSON _ = fail "Can't parse a Param"

instance Pretty Params where
  pretty p@Params{..} =
    vsep [ "Slot config:" <+> pretty pSlotConfig
         , "Network ID:" <+> viaShow pNetworkId
         , "Protocol Parameters:" <+> viaShow (pProtocolParams p)
         ]

-- | Set higher limits on transaction size and execution units.
-- This can be used to work around @MaxTxSizeUTxO@ and @ExUnitsTooBigUTxO@ errors.
-- Note that if you need this your Plutus script will probably not validate on Mainnet.
increaseTransactionLimits :: Params -> Params
increaseTransactionLimits = increaseTransactionLimits' 2 10 10

increaseTransactionLimits' :: Natural -> Natural -> Natural -> Params -> Params
increaseTransactionLimits' size steps mem = over protocolParamsL fixParams
  where
    fixParams pp = pp
      { C.protocolParamMaxTxSize = size * C.protocolParamMaxTxSize pp
      , C.protocolParamMaxTxExUnits = C.protocolParamMaxTxExUnits pp >>= (\C.ExecutionUnits {executionSteps, executionMemory} -> pure $ C.ExecutionUnits {executionSteps = steps * executionSteps, executionMemory = mem * executionMemory})
      }

instance Default Params where
  def = Params def (pParamsFromProtocolParams def) testnet

instance Default C.ProtocolParameters where
  -- The protocol parameters as they are in the Alonzo era.
  def = C.ProtocolParameters
    { protocolParamProtocolVersion = (7,0)
    , protocolParamDecentralization = Nothing
    , protocolParamExtraPraosEntropy = Nothing
    , protocolParamMaxBlockHeaderSize = 1100
    , protocolParamMaxBlockBodySize = 90112
    , protocolParamMaxTxSize = 16384
    , protocolParamTxFeeFixed = 155381
    , protocolParamTxFeePerByte = 44
    , protocolParamMinUTxOValue = Nothing
    , protocolParamStakeAddressDeposit = C.Lovelace 2000000
    , protocolParamStakePoolDeposit = C.Lovelace 500000000
    , protocolParamMinPoolCost = C.Lovelace 340000000
    , protocolParamPoolRetireMaxEpoch = C.EpochNo 18
    , protocolParamStakePoolTargetNum = 500
    , protocolParamPoolPledgeInfluence = 3 % 10
    , protocolParamMonetaryExpansion = 3 % 1000
    , protocolParamTreasuryCut = 1 % 5
    , protocolParamUTxOCostPerWord = Nothing -- Obsolete from babbage onwards
    , protocolParamCostModels =
        let costModel = fromJust $ defaultCostModelParams >>= Alonzo.costModelFromMap PlutusV3
            costModels = Map.fromList $ map (, costModel) [minBound .. maxBound]
        in C.fromAlonzoCostModels $ Alonzo.CostModels costModels mempty mempty
    , protocolParamPrices = Just (C.ExecutionUnitPrices {priceExecutionSteps = 721 % 10000000, priceExecutionMemory = 577 % 10000})
    , protocolParamMaxTxExUnits = Just (C.ExecutionUnits {executionSteps = 10000000000, executionMemory = 14000000})
    , protocolParamMaxBlockExUnits = Just (C.ExecutionUnits {executionSteps = 40000000000, executionMemory = 62000000})
    , protocolParamMaxValueSize = Just 5000
    , protocolParamCollateralPercent = Just 150
    , protocolParamMaxCollateralInputs = Just 3
    , protocolParamUTxOCostPerByte =
        let (Coin coinsPerUTxOByte) = Coin 4310
         in Just $ C.Lovelace coinsPerUTxOByte
    }


-- | Calculate the cardano-ledger `SlotLength`
slotLength :: Params -> SlotLength
slotLength Params { pSlotConfig } = mkSlotLength $ posixTimeToNominalDiffTime $ POSIXTime $ scSlotLength pSlotConfig


-- | A sensible default 'EpochSize' value for the emulator
emulatorEpochSize :: EpochSize
emulatorEpochSize = EpochSize 432000

-- | A sensible default 'Globals' value for the emulator
emulatorGlobals :: Params -> Globals
emulatorGlobals params = mkShelleyGlobals
  (genesisDefaultsFromParams params)
  (fixedEpochInfo emulatorEpochSize (slotLength params))
  (toEnum $ fromIntegral $ fst $ C.protocolParamProtocolVersion $ pProtocolParams params)

genesisDefaultsFromParams :: Params -> ShelleyGenesis StandardCrypto
genesisDefaultsFromParams params@Params { pSlotConfig, pNetworkId } = C.shelleyGenesisDefaults
  { C.sgSystemStart = posixTimeToUTCTime $ scSlotZeroTime pSlotConfig
  , C.sgNetworkMagic = case pNetworkId of C.Testnet (C.NetworkMagic nm) -> nm; _ -> 0
  , C.sgNetworkId = case pNetworkId of C.Testnet _ -> C.Ledger.Testnet; C.Mainnet -> C.Ledger.Mainnet
  , C.sgProtocolParams =  emulatorPParams params
      & C.downgradePParams (C.DowngradeBabbagePParams d C.Ledger.NeutralNonce)
      & C.downgradePParams (C.DowngradeAlonzoPParams (Coin 0))
      & C.downgradePParams ()
      & C.downgradePParams ()
  }
  where
    d = fromMaybe (error "3 % 5 should be valid UnitInterval") $ boundRational (3 % 5)

-- | A sensible default 'EraHistory' value for the emulator
emulatorEraHistory :: Params -> C.EraHistory C.CardanoMode
emulatorEraHistory params = C.EraHistory C.CardanoMode (Ouroboros.mkInterpreter $ Ouroboros.summaryWithExactly list)
  where
    one = Ouroboros.nonEmptyHead $ Ouroboros.getSummary $ Ouroboros.neverForksSummary emulatorEpochSize (slotLength params)
    list = Ouroboros.Exactly $ K one :* K one :* K one :* K one :* K one :* K one :* K one :* Nil
