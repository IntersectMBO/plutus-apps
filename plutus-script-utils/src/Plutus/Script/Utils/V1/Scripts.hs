module Plutus.Script.Utils.V1.Scripts
    ( -- * Script data hashes
      PV1.Datum
    , PV1.DatumHash
    , PV1.Redeemer
    , PV1.RedeemerHash
    , datumHash
    , redeemerHash
    , dataHash
    -- * Script hashes
    , PV1.Validator
    , PV1.ValidatorHash
    , PV1.MintingPolicy
    , PV1.MintingPolicyHash
    , PV1.StakeValidator
    , PV1.StakeValidatorHash
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
import Plutus.V1.Ledger.Api qualified as PV1
import Plutus.V1.Ledger.Scripts qualified as PV1
import PlutusTx.Builtins qualified as Builtins

-- | Hash a 'PV1.Datum builtin data.
datumHash :: PV1.Datum -> PV1.DatumHash
datumHash = PV1.DatumHash . dataHash . PV1.getDatum

-- | Hash a 'PV1.Redeemer' builtin data.
redeemerHash :: PV1.Redeemer -> PV1.RedeemerHash
redeemerHash = PV1.RedeemerHash . dataHash . PV1.getRedeemer

-- | Hash a 'PV1.Validator' script.
validatorHash :: PV1.Validator -> PV1.ValidatorHash
validatorHash =
    PV1.ValidatorHash
  . PV1.getScriptHash
  . scriptHash
  . PV1.getValidator

-- | Hash a 'PV1.MintingPolicy' script.
mintingPolicyHash :: PV1.MintingPolicy -> PV1.MintingPolicyHash
mintingPolicyHash =
    PV1.MintingPolicyHash
  . PV1.getScriptHash
  . scriptHash
  . PV1.getMintingPolicy

-- | Hash a 'PV1.StakeValidator' script.
stakeValidatorHash :: PV1.StakeValidator -> PV1.StakeValidatorHash
stakeValidatorHash =
    PV1.StakeValidatorHash
  . PV1.getScriptHash
  . scriptHash
  . PV1.getStakeValidator

-- | Hash a 'Builtins.BuiltinData'
dataHash :: Builtins.BuiltinData -> Builtins.BuiltinByteString
dataHash =
    Builtins.toBuiltin
    . Script.serialiseToRawBytes
    . Script.hashScriptData
    . toCardanoAPIData

-- | Convert a 'Builtins.BuiltinsData' value to a 'cardano-api' script
--   data value.
--
-- For why we depend on `cardano-api`,
-- see note [Hash computation of datums, redeemers and scripts]
toCardanoAPIData :: Builtins.BuiltinData -> Script.ScriptData
toCardanoAPIData = Script.fromPlutusData . Builtins.builtinDataToData

-- | Hash a 'Script'
scriptHash :: PV1.Script -> PV1.ScriptHash
scriptHash =
    PV1.ScriptHash
    . Builtins.toBuiltin
    . Script.serialiseToRawBytes
    . Script.hashScript
    . toCardanoApiScript

-- | Convert a 'Script' to a 'cardano-api' script.
--
-- For why we depend on `cardano-api`,
-- see note [Hash computation of datums, redeemers and scripts]
toCardanoApiScript :: PV1.Script -> Script.Script Script.PlutusScriptV1
toCardanoApiScript =
    Script.PlutusScript Script.PlutusScriptV1
  . Script.PlutusScriptSerialised
  . SBS.toShort
  . BSL.toStrict
  . serialise

{-# INLINABLE scriptCurrencySymbol #-}
-- | The 'CurrencySymbol' of a 'MintingPolicy'.
scriptCurrencySymbol :: PV1.MintingPolicy -> PV1.CurrencySymbol
scriptCurrencySymbol scrpt =
    let (PV1.MintingPolicyHash hsh) = mintingPolicyHash scrpt in PV1.CurrencySymbol hsh

{- Note [Hash computation of datums, redeemers and scripts]

We have three options for computing the hash (each with advantages and drawbacks):

1- Depend on `cardano-api` and use it's `Scripts.hashScriptData` and `Scripts.hashScript`
functions.
The good: most simplest way to compute the hashes.
The bad: this package has an additional pretty large dependency.

2- Depend on `cardano-ledger` instead and use their `hashScriptData` and `hashScript`.
The good: smaller footprint than `cardano-api`.
The bad: a lower-lever library than `cardano-api`.

3- Depend on `cardano-crypto-class`, and reimplement ourselves the hashing functions
from `cardano-ledger`.
The good: the lowest dependency footprint.
The bad: code duplication.

However, we expect that most Plutus script devs depending on this package will
also probably depend on `cardano-api`, so the dependency on `cardano-api` should
(probably) be an non-issue.

If this becomes an issue, we'll change the implementation.
-}
