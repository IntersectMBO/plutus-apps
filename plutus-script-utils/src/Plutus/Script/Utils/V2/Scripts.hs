{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Plutus.Script.Utils.V2.Scripts
    ( module Export
    -- * Script data hashes
    , PV2.Datum
    , PV2.DatumHash
    , PV2.Redeemer
    , PV2.RedeemerHash
    , P.datumHash
    , P.redeemerHash
    , P.dataHash
    -- * Script hashes
    , PV2.Validator
    , PV2.ValidatorHash
    , PV2.MintingPolicy
    , PV2.MintingPolicyHash
    , PV2.StakeValidator
    , PV2.StakeValidatorHash
    , validatorHash
    , mintingPolicyHash
    , stakeValidatorHash
    , scriptHash
    -- * Script utilities
    , scriptCurrencySymbol
    ) where

import Cardano.Api qualified as Script
import Cardano.Api.Shelley qualified as Script
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as SBS
import Plutus.Script.Utils.V1.Scripts qualified as P
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx.Builtins qualified as Builtins

import Plutus.Script.Utils.V2.Scripts.MonetaryPolicies as Export hiding (forwardToValidator)
import Plutus.Script.Utils.V2.Scripts.StakeValidators as Export hiding (forwardToValidator)
import Plutus.Script.Utils.V2.Scripts.Validators as Export

-- | Hash a 'PV2.Validator' script.
validatorHash :: PV2.Validator -> PV2.ValidatorHash
validatorHash =
    PV2.ValidatorHash
  . PV2.getScriptHash
  . scriptHash
  . PV2.getValidator

-- | Hash a 'PV2.MintingPolicy' script.
mintingPolicyHash :: PV2.MintingPolicy -> PV2.MintingPolicyHash
mintingPolicyHash =
    PV2.MintingPolicyHash
  . PV2.getScriptHash
  . scriptHash
  . PV2.getMintingPolicy

-- | Hash a 'PV2.StakeValidator' script.
stakeValidatorHash :: PV2.StakeValidator -> PV2.StakeValidatorHash
stakeValidatorHash =
    PV2.StakeValidatorHash
  . PV2.getScriptHash
  . scriptHash
  . PV2.getStakeValidator

-- | Convert a 'Builtins.BuiltinsData' value to a 'cardano-api' script
--   data value.
--
-- For why we depend on `cardano-api`,
-- see note [Hash computation of datums, redeemers and scripts]
-- toCardanoAPIData :: Builtins.BuiltinData -> Script.ScriptData
-- toCardanoAPIData = Script.fromPlutusData . Builtins.builtinDataToData

-- | Hash a 'Script'
scriptHash :: PV2.Script -> PV2.ScriptHash
scriptHash =
    PV2.ScriptHash
    . Builtins.toBuiltin
    . Script.serialiseToRawBytes
    . Script.hashScript
    . toCardanoApiScript

-- | Convert a 'Script' to a 'cardano-api' script.
--
-- For why we depend on `cardano-api`,
-- see note [Hash computation of datums, redeemers and scripts]
toCardanoApiScript :: PV2.Script -> Script.Script Script.PlutusScriptV2
toCardanoApiScript =
    Script.PlutusScript Script.PlutusScriptV2
  . Script.PlutusScriptSerialised
  . SBS.toShort
  . BSL.toStrict
  . serialise

{-# INLINABLE scriptCurrencySymbol #-}
-- | The 'CurrencySymbol' of a 'MintingPolicy'.
scriptCurrencySymbol :: PV2.MintingPolicy -> PV2.CurrencySymbol
scriptCurrencySymbol scrpt =
    let (PV2.MintingPolicyHash hsh) = mintingPolicyHash scrpt in PV2.CurrencySymbol hsh

{- See Note [Hash computation of datums, redeemers and scripts] -}

{- See Note [Scripts returning Bool] -}
