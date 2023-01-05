{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module PlutusScripts (
    Secp256Params
  , verifySchnorrPolicyScript
  , verifySchnorrPolicyId
  , verifySchnorrParams
  , verifySchnorrMintWitness
  ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Codec.Serialise (serialise)
import Data.ByteString qualified as BS (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Helpers (unsafeFromEither)
import Plutus.Script.Utils.Typed as PSU (IsScriptContext (mkUntypedMintingPolicy))
import Plutus.V1.Ledger.Bytes as P (bytes, fromHex)
import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude qualified as P

bytesFromHex :: BS.ByteString -> BS.ByteString
bytesFromHex b = bytes $ unsafeFromEither $ P.fromHex b

-- | Default execution units with zero values
defExecutionUnits :: C.ExecutionUnits
defExecutionUnits = C.ExecutionUnits {C.executionSteps = 0, C.executionMemory = 0 }

mintScriptWitnessV2 :: C.PlutusScript C.PlutusScriptV2 -> C.ScriptData -> C.ScriptWitness C.WitCtxMint C.BabbageEra
mintScriptWitnessV2 script redeemer = C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2
        (C.PScript script) C.NoScriptDatumForMint redeemer defExecutionUnits

toRedeemer :: PlutusTx.ToData a => a -> C.ScriptData
toRedeemer a = C.fromPlutusData $ PlutusTx.toData a

-- | SECP256k1 Schnorr minting policy

data Secp256Params = Secp256Params
    { vkey :: !P.BuiltinByteString,
      msg  :: !P.BuiltinByteString,
      sig  :: !P.BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''Secp256Params

{-# INLINABLE mkVerifySchnorrPolicy #-}
mkVerifySchnorrPolicy :: (BI.BuiltinByteString, BI.BuiltinByteString, BI.BuiltinByteString)
                               -> PlutusV2.ScriptContext
                               -> Bool
mkVerifySchnorrPolicy (v, m, s) _sc = BI.verifySchnorrSecp256k1Signature v m s

verifySchnorrPolicy :: PlutusV2.MintingPolicy
verifySchnorrPolicy = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
  where
      wrap = PSU.mkUntypedMintingPolicy mkVerifySchnorrPolicy

verifySchnorrPolicyScript :: C.PlutusScript C.PlutusScriptV2
verifySchnorrPolicyScript = C.PlutusScriptSerialised
                         $ SBS.toShort
                         . LBS.toStrict
                         $ serialise
                         $ PlutusV2.unMintingPolicyScript verifySchnorrPolicy

verifySchnorrPolicyId :: C.PolicyId
verifySchnorrPolicyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV2 verifySchnorrPolicyScript :: C.PolicyId

verifySchnorrParams :: Secp256Params
verifySchnorrParams = Secp256Params
  {
    vkey = BI.toBuiltin $ bytesFromHex "599de3e582e2a3779208a210dfeae8f330b9af00a47a7fb22e9bb8ef596f301b",
    msg  = BI.toBuiltin $ bytesFromHex "30303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030",
    sig  = BI.toBuiltin $ bytesFromHex "5a56da88e6fd8419181dec4d3dd6997bab953d2fc71ab65e23cfc9e7e3d1a310613454a60f6703819a39fdac2a410a094442afd1fc083354443e8d8bb4461a9b"
  }

verifySchnorrRedeemer :: C.ScriptData
verifySchnorrRedeemer = toRedeemer verifySchnorrParams

verifySchnorrMintWitness :: (C.PolicyId, C.ScriptWitness C.WitCtxMint C.BabbageEra)
verifySchnorrMintWitness = (verifySchnorrPolicyId, mintScriptWitnessV2 verifySchnorrPolicyScript verifySchnorrRedeemer)
