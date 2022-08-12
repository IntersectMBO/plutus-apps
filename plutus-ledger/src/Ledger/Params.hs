{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | The set of parameters, like protocol parameters and slot configuration.
module Ledger.Params(
  Params(..),
  slotConfigL,
  protocolParamsL,
  networkIdL,
  increaseTransactionLimits,
  -- * cardano-ledger specific types and conversion functions
  EmulatorEra,
  slotLength,
  emulatorEpochSize,
  emulatorGlobals,
  emulatorPParams,
  emulatorEraHistory
) where

import Cardano.Api (CardanoMode, ConsensusMode (..), EraHistory (EraHistory), ShelleyBasedEra (..), toLedgerPParams)
import Cardano.Api.Shelley (AnyPlutusScriptVersion (..), CostModel (..), EpochNo (..), ExecutionUnitPrices (..),
                            ExecutionUnits (..), Lovelace (..), NetworkId (..), NetworkMagic (..),
                            PlutusScriptVersion (..), ProtocolParameters (..), shelleyGenesisDefaults)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.PParams (retractPP)
import Cardano.Ledger.Babbage.Translation (coinsPerUTxOWordToCoinsPerUTxOByte)
import Cardano.Ledger.BaseTypes (boundRational)
import Cardano.Ledger.Core (PParams)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API (Coin (..), Globals, ShelleyGenesis (..), mkShelleyGlobals)
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Cardano.Ledger.Slot (EpochSize (..))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SlotLength, mkSlotLength)
import Control.Lens (makeLensesFor, over)
import Data.Default (Default (def))
import Data.Map (fromList)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.SOP.Strict (K (K), NP (..))
import Ledger.TimeSlot (SlotConfig (..), posixTimeToNominalDiffTime, posixTimeToUTCTime)
import Ouroboros.Consensus.HardFork.History qualified as Ouroboros
import Ouroboros.Consensus.Util.Counting qualified as Ouroboros
import Plutus.V1.Ledger.Api (POSIXTime (..))
import PlutusCore (defaultCostModelParams)

data Params = Params
  { pSlotConfig     :: SlotConfig
  , pProtocolParams :: ProtocolParameters
  , pNetworkId      :: NetworkId
  }
  deriving (Eq, Show)

makeLensesFor
  [ ("pSlotConfig", "slotConfigL")
  , ("pProtocolParams", "protocolParamsL")
  , ("pNetworkId", "networkIdL") ]
  ''Params

-- | Set higher limits on transaction size and execution units.
-- This can be used to work around @MaxTxSizeUTxO@ and @ExUnitsTooBigUTxO@ errors.
-- Note that if you need this your Plutus script will probably not validate on Mainnet.
increaseTransactionLimits :: Params -> Params
increaseTransactionLimits = over protocolParamsL fixParams
  where
    fixParams pp = pp
      { protocolParamMaxTxSize = 2 * protocolParamMaxTxSize pp
      , protocolParamMaxTxExUnits = protocolParamMaxTxExUnits pp >>= (\ExecutionUnits {executionSteps, executionMemory} -> pure $ ExecutionUnits {executionSteps = 10 * executionSteps, executionMemory = 10 * executionMemory})
      }

instance Default Params where
  def = Params def def (Testnet $ NetworkMagic 1)

instance Default ProtocolParameters where
  -- The protocol parameters as they are in the Alonzo era.
  def = ProtocolParameters
    { protocolParamProtocolVersion = (6,0)
    , protocolParamDecentralization = Just (3 % 5)
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
    , protocolParamUTxOCostPerWord = Nothing -- Obsolete from babbage onwards
    , protocolParamCostModels = fromList
      [ (AnyPlutusScriptVersion PlutusScriptV1, CostModel $ fromMaybe (error "Ledger.Params: defaultCostModelParams is broken") defaultCostModelParams)
      , (AnyPlutusScriptVersion PlutusScriptV2, CostModel $ fromMaybe (error "Ledger.Params: defaultCostModelParams is broken") defaultCostModelParams) ]
    , protocolParamPrices = Just (ExecutionUnitPrices {priceExecutionSteps = 721 % 10000000, priceExecutionMemory = 577 % 10000})
    , protocolParamMaxTxExUnits = Just (ExecutionUnits {executionSteps = 10000000000, executionMemory = 10000000})
    , protocolParamMaxBlockExUnits = Just (ExecutionUnits {executionSteps = 40000000000, executionMemory = 50000000})
    , protocolParamMaxValueSize = Just 5000
    , protocolParamCollateralPercent = Just 150
    , protocolParamMaxCollateralInputs = Just 3
    , protocolParamUTxOCostPerByte =
        let (Coin coinsPerUTxOByte) = coinsPerUTxOWordToCoinsPerUTxOByte $ Coin 34482
         in Just $ Lovelace coinsPerUTxOByte
    }


-- | The default era for the emulator
type EmulatorEra = BabbageEra StandardCrypto

-- | Calculate the cardano-ledger `SlotLength`
slotLength :: Params -> SlotLength
slotLength Params { pSlotConfig } = mkSlotLength $ posixTimeToNominalDiffTime $ POSIXTime $ scSlotLength pSlotConfig


-- | A sensible default 'EpochSize' value for the emulator
emulatorEpochSize :: EpochSize
emulatorEpochSize = EpochSize 432000

-- | A sensible default 'Globals' value for the emulator
emulatorGlobals :: Params -> Globals
emulatorGlobals params@Params { pProtocolParams } = mkShelleyGlobals
  (genesisDefaultsFromParams params)
  (fixedEpochInfo emulatorEpochSize (slotLength params))
  (fst $ protocolParamProtocolVersion pProtocolParams)

genesisDefaultsFromParams :: Params -> ShelleyGenesis EmulatorEra
genesisDefaultsFromParams params@Params { pSlotConfig, pNetworkId } = shelleyGenesisDefaults
  { sgSystemStart = posixTimeToUTCTime $ scSlotZeroTime pSlotConfig
  , sgNetworkMagic = case pNetworkId of Testnet (NetworkMagic nm) -> nm; _ -> 0
  , sgNetworkId = case pNetworkId of Testnet _ -> C.Ledger.Testnet; Mainnet -> C.Ledger.Mainnet
  , sgProtocolParams = retractPP (Coin 0) d C.Ledger.NeutralNonce $ emulatorPParams params
  }
  where
    d = fromMaybe (error "3 % 5 should be valid UnitInterval") $ boundRational (3 % 5)

-- | Convert `Params` to cardano-ledger `PParams`
emulatorPParams :: Params -> PParams EmulatorEra
emulatorPParams Params { pProtocolParams } = toLedgerPParams ShelleyBasedEraBabbage pProtocolParams

-- | A sensible default 'EraHistory' value for the emulator
emulatorEraHistory :: Params -> EraHistory CardanoMode
emulatorEraHistory params = EraHistory CardanoMode (Ouroboros.mkInterpreter $ Ouroboros.summaryWithExactly list)
  where
    one = Ouroboros.nonEmptyHead $ Ouroboros.getSummary $ Ouroboros.neverForksSummary emulatorEpochSize (slotLength params)
    list = Ouroboros.Exactly $ K one :* K one :* K one :* K one :* K one :* K one :* Nil
