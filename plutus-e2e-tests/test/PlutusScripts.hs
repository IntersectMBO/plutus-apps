{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module PlutusScripts (
    verifySchnorrAssetIdV1
  , verifySchnorrAssetIdV2
  , verifySchnorrMintWitnessV1
  , verifySchnorrMintWitnessV2
  , verifyEcdsaAssetIdV1
  , verifyEcdsaAssetIdV2
  , verifyEcdsaMintWitnessV1
  , verifyEcdsaMintWitnessV2
  ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Codec.Serialise (serialise)
import Data.ByteString qualified as BS (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Plutus.Script.Utils.Typed as PSU (IsScriptContext (mkUntypedMintingPolicy))
import Plutus.V1.Ledger.Api (MintingPolicy, mkMintingPolicyScript, unMintingPolicyScript)
import Plutus.V1.Ledger.Api qualified as PlutusV1
import Plutus.V1.Ledger.Bytes qualified as P (bytes, fromHex)
import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude qualified as P

bytesFromHex :: BS.ByteString -> BS.ByteString
bytesFromHex = P.bytes . fromEither . P.fromHex
  where
    fromEither (Left e)  = error $ show e
    fromEither (Right b) = b

-- | Default execution units with zero values
defExecutionUnits :: C.ExecutionUnits
defExecutionUnits = C.ExecutionUnits {C.executionSteps = 0, C.executionMemory = 0 }

toScriptData :: PlutusTx.ToData a => a -> C.ScriptData
toScriptData a = C.fromPlutusData $ PlutusTx.toData a

mintScriptWitnessV1 :: C.CardanoEra era
  -> C.PlutusScript C.PlutusScriptV1
  -> C.ScriptData
  -> C.ScriptWitness C.WitCtxMint era
mintScriptWitnessV1 era script redeemer = do
    let lang = C.PlutusScriptLanguage C.PlutusScriptV1
    C.PlutusScriptWitness (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
      C.PlutusScriptV1 (C.PScript script) C.NoScriptDatumForMint redeemer defExecutionUnits

mintScriptWitnessV2 :: C.CardanoEra era
  -> C.PlutusScript C.PlutusScriptV2
  -> C.ScriptData
  -> C.ScriptWitness C.WitCtxMint era
mintScriptWitnessV2 era script redeemer = do
    let lang = C.PlutusScriptLanguage C.PlutusScriptV2
    C.PlutusScriptWitness (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
      C.PlutusScriptV2 (C.PScript script) C.NoScriptDatumForMint redeemer defExecutionUnits

maybeScriptWitness :: C.CardanoEra era
    -> C.ScriptLanguage l
    -> Maybe (C.ScriptLanguageInEra l era)
    -> C.ScriptLanguageInEra l era
maybeScriptWitness era lang Nothing = error $ "Era " ++ show era
                                    ++ " does not support script language " ++ show lang
maybeScriptWitness _ _ (Just p) = p

policyScript :: MintingPolicy -> C.PlutusScript lang
policyScript = C.PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . unMintingPolicyScript

-- | SECP256k1

data Secp256Params = Secp256Params
    { vkey :: !P.BuiltinByteString,
      msg  :: !P.BuiltinByteString,
      sig  :: !P.BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''Secp256Params

-- | Schnorr minting policy

{-# INLINABLE mkVerifySchnorrPolicy #-}
mkVerifySchnorrPolicy :: (BI.BuiltinByteString, BI.BuiltinByteString, BI.BuiltinByteString)
                      -> sc
                      -> Bool
mkVerifySchnorrPolicy (v, m, s) _sc = BI.verifySchnorrSecp256k1Signature v m s

verifySchnorrPolicyV1 :: MintingPolicy
verifySchnorrPolicyV1 = mkMintingPolicyScript
  $$(PlutusTx.compile [||PSU.mkUntypedMintingPolicy @PlutusV1.ScriptContext mkVerifySchnorrPolicy||])

verifySchnorrPolicyV2 :: MintingPolicy
verifySchnorrPolicyV2 = mkMintingPolicyScript
  $$(PlutusTx.compile [||PSU.mkUntypedMintingPolicy @PlutusV2.ScriptContext mkVerifySchnorrPolicy||])

verifySchnorrPolicyIdV1 :: C.PolicyId
verifySchnorrPolicyIdV1 = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV1 (policyScript verifySchnorrPolicyV1)

verifySchnorrPolicyIdV2 :: C.PolicyId
verifySchnorrPolicyIdV2 = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV2 (policyScript verifySchnorrPolicyV2) :: C.PolicyId

schnorrAssetName :: C.AssetName
schnorrAssetName = C.AssetName "Schnorr"

verifySchnorrAssetIdV1 :: C.AssetId
verifySchnorrAssetIdV1 = C.AssetId verifySchnorrPolicyIdV1 schnorrAssetName

verifySchnorrAssetIdV2 :: C.AssetId
verifySchnorrAssetIdV2 = C.AssetId verifySchnorrPolicyIdV2 schnorrAssetName

verifySchnorrParams :: Secp256Params
verifySchnorrParams = Secp256Params
  {
    vkey = BI.toBuiltin $ bytesFromHex "599de3e582e2a3779208a210dfeae8f330b9af00a47a7fb22e9bb8ef596f301b",
    msg  = BI.toBuiltin $ bytesFromHex "30303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030",
    sig  = BI.toBuiltin $ bytesFromHex "5a56da88e6fd8419181dec4d3dd6997bab953d2fc71ab65e23cfc9e7e3d1a310613454a60f6703819a39fdac2a410a094442afd1fc083354443e8d8bb4461a9b"
  }

verifySchnorrRedeemer :: C.ScriptData
verifySchnorrRedeemer = toScriptData verifySchnorrParams

verifySchnorrMintWitnessV1 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifySchnorrMintWitnessV1 era =
    (verifySchnorrPolicyIdV1,
     mintScriptWitnessV1 era (policyScript verifySchnorrPolicyV1) verifySchnorrRedeemer)

verifySchnorrMintWitnessV2 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifySchnorrMintWitnessV2 era =
    (verifySchnorrPolicyIdV2,
     mintScriptWitnessV2 era (policyScript verifySchnorrPolicyV2) verifySchnorrRedeemer)

-- | ECDSA minting policy

{-# INLINABLE mkVerifyEcdsaPolicy #-}
mkVerifyEcdsaPolicy :: (BI.BuiltinByteString, BI.BuiltinByteString, BI.BuiltinByteString)
                    -> sc
                    -> Bool
mkVerifyEcdsaPolicy (v, m, s) _sc = BI.verifyEcdsaSecp256k1Signature v m s

verifyEcdsaPolicyV1 :: MintingPolicy
verifyEcdsaPolicyV1 = mkMintingPolicyScript
  $$(PlutusTx.compile [||PSU.mkUntypedMintingPolicy @PlutusV1.ScriptContext mkVerifyEcdsaPolicy||])

verifyEcdsaPolicyV2 :: MintingPolicy
verifyEcdsaPolicyV2 = mkMintingPolicyScript
  $$(PlutusTx.compile [||PSU.mkUntypedMintingPolicy @PlutusV2.ScriptContext mkVerifyEcdsaPolicy||])

verifyEcdsaPolicyIdV1 :: C.PolicyId
verifyEcdsaPolicyIdV1 = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV1 (policyScript verifyEcdsaPolicyV1)

verifyEcdsaPolicyIdV2 :: C.PolicyId
verifyEcdsaPolicyIdV2 = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV2 (policyScript verifyEcdsaPolicyV2)

ecdsaAssetName :: C.AssetName
ecdsaAssetName = C.AssetName "ECDSA"

verifyEcdsaAssetIdV1 :: C.AssetId
verifyEcdsaAssetIdV1 = C.AssetId verifyEcdsaPolicyIdV1 ecdsaAssetName

verifyEcdsaAssetIdV2 :: C.AssetId
verifyEcdsaAssetIdV2 = C.AssetId verifyEcdsaPolicyIdV2 ecdsaAssetName

verifyEcdsaParams :: Secp256Params
verifyEcdsaParams = Secp256Params
  {
    vkey = BI.toBuiltin $ bytesFromHex "0392d7b94bc6a11c335a043ee1ff326b6eacee6230d3685861cd62bce350a172e0",
    msg  = BI.toBuiltin $ bytesFromHex "16e0bf1f85594a11e75030981c0b670370b3ad83a43f49ae58a2fd6f6513cde9",
    sig  = BI.toBuiltin $ bytesFromHex "5fb12954b28be6456feb080cfb8467b6f5677f62eb9ad231de7a575f4b6857512754fb5ef7e0e60e270832e7bb0e2f0dc271012fa9c46c02504aa0e798be6295"
  }

verifyEcdsaRedeemer :: C.ScriptData
verifyEcdsaRedeemer = toScriptData verifyEcdsaParams

verifyEcdsaMintWitnessV1 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyEcdsaMintWitnessV1 era =
    (verifyEcdsaPolicyIdV1,
     mintScriptWitnessV1 era (policyScript verifyEcdsaPolicyV1) verifyEcdsaRedeemer)

verifyEcdsaMintWitnessV2 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyEcdsaMintWitnessV2 era =
    (verifyEcdsaPolicyIdV2,
     mintScriptWitnessV2 era (policyScript verifyEcdsaPolicyV2) verifyEcdsaRedeemer)
