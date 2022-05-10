module Ledger.Scripts (
    module Export
    , datumHash
    , redeemerHash
    , dataHash
    , toCardanoAPIData
    , plutusV1ScriptHash
    , plutusV2ScriptHash
    , plutusV1ValidatorHash
    , plutusV2ValidatorHash
    , plutusV1MintingPolicyHash
    , plutusV2MintingPolicyHash
    , plutusV1StakeValidatorHash
    , plutusV2StakeValidatorHash
    , toCardanoApiPlutusV1Script
    , toCardanoApiPlutusV2Script
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

plutusV1ValidatorHash :: Validator -> ValidatorHash
plutusV1ValidatorHash =
    ValidatorHash
  . getScriptHash
  . plutusV1ScriptHash
  . getValidator

plutusV2ValidatorHash :: Validator -> ValidatorHash
plutusV2ValidatorHash =
    ValidatorHash
  . getScriptHash
  . plutusV2ScriptHash
  . getValidator

plutusV1MintingPolicyHash :: MintingPolicy -> MintingPolicyHash
plutusV1MintingPolicyHash =
    MintingPolicyHash
  . getScriptHash
  . plutusV1ScriptHash
  . getMintingPolicy

plutusV2MintingPolicyHash :: MintingPolicy -> MintingPolicyHash
plutusV2MintingPolicyHash =
    MintingPolicyHash
  . getScriptHash
  . plutusV2ScriptHash
  . getMintingPolicy

plutusV1StakeValidatorHash :: StakeValidator -> StakeValidatorHash
plutusV1StakeValidatorHash =
    StakeValidatorHash
  . getScriptHash
  . plutusV1ScriptHash
  . getStakeValidator

plutusV2StakeValidatorHash :: StakeValidator -> StakeValidatorHash
plutusV2StakeValidatorHash =
    StakeValidatorHash
  . getScriptHash
  . plutusV2ScriptHash
  . getStakeValidator

-- | Hash a Plutus V1 'Script'
plutusV1ScriptHash :: Script -> ScriptHash
plutusV1ScriptHash =
    ScriptHash
    . toBuiltin
    . Script.serialiseToRawBytes
    . Script.hashScript
    . toCardanoApiPlutusV1Script

-- | Hash a Plutus V2 'Script'
plutusV2ScriptHash :: Script -> ScriptHash
plutusV2ScriptHash =
    ScriptHash
    . toBuiltin
    . Script.serialiseToRawBytes
    . Script.hashScript
    . toCardanoApiPlutusV2Script

-- | Convert a 'Script' to a 'cardano-api' Plutus V1 script
toCardanoApiPlutusV1Script :: Script -> Script.Script Script.PlutusScriptV1
toCardanoApiPlutusV1Script =
    Script.PlutusScript Script.PlutusScriptV1
    . Script.PlutusScriptSerialised
    . SBS.toShort
    . BSL.toStrict
    . serialise

-- | Convert a 'Script' to a 'cardano-api' Plutus V2 script
toCardanoApiPlutusV2Script :: Script -> Script.Script Script.PlutusScriptV2
toCardanoApiPlutusV2Script =
    Script.PlutusScript Script.PlutusScriptV2
    . Script.PlutusScriptSerialised
    . SBS.toShort
    . BSL.toStrict
    . serialise
