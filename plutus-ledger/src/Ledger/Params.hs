{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | The set of parameters, like protocol parameters and slot configuration.
module Ledger.Params(
  Params(..)
) where

import Cardano.Api.Shelley
import Data.Default (Default (def))
import Data.Map (fromList)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Ledger.TimeSlot (SlotConfig)
import Plutus.V1.Ledger.Api (defaultCostModelParams)

data Params = Params
  { pSlotConfig     :: SlotConfig
  , pProtocolParams :: ProtocolParameters
  , pNetworkId      :: NetworkId
  }
  deriving (Eq, Show)

instance Default Params where
  def = Params def def (Testnet $ NetworkMagic 1)

instance Default ProtocolParameters where
    def = ProtocolParameters
        { protocolParamProtocolVersion = (6,0)
        , protocolParamDecentralization = 3 % 5
        , protocolParamExtraPraosEntropy = Nothing
        , protocolParamMaxBlockHeaderSize = 1100
        , protocolParamMaxBlockBodySize = 65536
        , protocolParamMaxTxSize = 16384
        , protocolParamTxFeeFixed = 155381
        , protocolParamTxFeePerByte = 44
        , protocolParamMinUTxOValue = Nothing
        , protocolParamStakeAddressDeposit = Lovelace 2000000
        , protocolParamStakePoolDeposit = Lovelace 500000000
        , protocolParamMinPoolCost = Lovelace 340000000
        , protocolParamPoolRetireMaxEpoch = EpochNo 18
        , protocolParamStakePoolTargetNum = 150
        , protocolParamPoolPledgeInfluence = 3 % 10
        , protocolParamMonetaryExpansion = 3 % 1000
        , protocolParamTreasuryCut = 1 % 5
        , protocolParamUTxOCostPerWord = Just (Lovelace 34482)
        , protocolParamCostModels = fromList
            [ (AnyPlutusScriptVersion PlutusScriptV1, CostModel $ fromMaybe (error "Ledger.Params: defaultCostModelParams is broken") defaultCostModelParams) ]
        , protocolParamPrices = Just (ExecutionUnitPrices {priceExecutionSteps = 721 % 10000000, priceExecutionMemory = 577 % 10000})
        , protocolParamMaxTxExUnits = Just (ExecutionUnits {executionSteps = 10000000000, executionMemory = 10000000})
        , protocolParamMaxBlockExUnits = Just (ExecutionUnits {executionSteps = 40000000000, executionMemory = 50000000})
        , protocolParamMaxValueSize = Just 5000
        , protocolParamCollateralPercent = Just 150
        , protocolParamMaxCollateralInputs = Just 3
        }
