{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
This module re-exports the module 'Plutus.V1.Ledger.Scripts', but with
additional functionality.

This module contains orphan instances of 'Cardano.Api.HasTextEnvelope', since
the Cardano Node CLI expects serialised binary values to be wrapped with a
'Cardano.Api.TextEnvelope'.
-}
module Ledger.Scripts (
    module Export
    , datumHash
    , redeemerHash
    , validatorHash
    , mintingPolicyHash
    , stakeValidatorHash
    , toCardanoApiScript
    , scriptHash
    , dataHash
    , toCardanoAPIData
    ) where

import Cardano.Api qualified as Script
import Cardano.Api.Shelley qualified as Script
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as SBS
import Plutus.V1.Ledger.Scripts as Export
import PlutusTx.Builtins as Builtins

datumHash :: Datum -> DatumHash
datumHash = DatumHash . dataHash . getDatum

redeemerHash :: Redeemer -> RedeemerHash
redeemerHash = RedeemerHash . dataHash . getRedeemer

validatorHash :: Validator -> ValidatorHash
validatorHash = ValidatorHash . getScriptHash . scriptHash . getValidator

mintingPolicyHash :: MintingPolicy -> MintingPolicyHash
mintingPolicyHash = MintingPolicyHash . getScriptHash . scriptHash . getMintingPolicy

stakeValidatorHash :: StakeValidator -> StakeValidatorHash
stakeValidatorHash = StakeValidatorHash . getScriptHash . scriptHash . getStakeValidator

-- | Hash a 'Builtins.BuiltinData'
dataHash :: Builtins.BuiltinData -> Builtins.BuiltinByteString
dataHash =
    toBuiltin
    . Script.serialiseToRawBytes
    . Script.hashScriptData
    . toCardanoAPIData

-- | Convert a 'Builtins.BuiltinsData' value to a 'cardano-api' script
--   data value.
toCardanoAPIData :: Builtins.BuiltinData -> Script.ScriptData
toCardanoAPIData = Script.fromPlutusData . builtinDataToData

-- | Hash a 'Script'
scriptHash :: Script -> ScriptHash
scriptHash =
    ScriptHash
    . toBuiltin
    . Script.serialiseToRawBytes
    . Script.hashScript
    . toCardanoApiScript

-- | Convert a 'Script' to a 'cardano-api' script
toCardanoApiScript :: Script -> Script.Script Script.PlutusScriptV1
toCardanoApiScript =
    Script.PlutusScript Script.PlutusScriptV1
    . Script.PlutusScriptSerialised
    . SBS.toShort
    . BSL.toStrict
    . serialise
