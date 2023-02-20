{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra

module PlutusScripts.SECP256k1 (
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
import Plutus.Script.Utils.Typed (IsScriptContext (mkUntypedMintingPolicy))
import Plutus.V1.Ledger.Api (MintingPolicy, mkMintingPolicyScript)
import Plutus.V1.Ledger.Api qualified as PlutusV1
import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusScripts.Helpers (bytesFromHex, mintScriptWitness, plutusL1, plutusL2, policyIdV1, policyIdV2, policyScript,
                              toScriptData)
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude qualified as P

---- SECP256k1 ----

data Secp256Params = Secp256Params
    { vkey :: P.BuiltinByteString,
      msg  :: P.BuiltinByteString,
      sig  :: P.BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''Secp256Params

-- Schnorr minting policy --

{-# INLINABLE mkVerifySchnorrPolicy #-}
mkVerifySchnorrPolicy :: Secp256Params -> sc -> Bool
mkVerifySchnorrPolicy Secp256Params{..} _sc = BI.verifySchnorrSecp256k1Signature vkey msg sig

verifySchnorrPolicyV1 :: MintingPolicy
verifySchnorrPolicyV1 = mkMintingPolicyScript
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV1.ScriptContext mkVerifySchnorrPolicy

verifySchnorrPolicyV2 :: MintingPolicy
verifySchnorrPolicyV2 = mkMintingPolicyScript
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV2.ScriptContext mkVerifySchnorrPolicy

verifySchnorrPolicyScriptV1 :: C.PlutusScript C.PlutusScriptV1
verifySchnorrPolicyScriptV1 = policyScript verifySchnorrPolicyV1

verifySchnorrPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
verifySchnorrPolicyScriptV2 = policyScript verifySchnorrPolicyV2

schnorrAssetName :: C.AssetName
schnorrAssetName = C.AssetName "Schnorr"

verifySchnorrAssetIdV1 :: C.AssetId
verifySchnorrAssetIdV1 = C.AssetId (policyIdV1 verifySchnorrPolicyV1) schnorrAssetName

verifySchnorrAssetIdV2 :: C.AssetId
verifySchnorrAssetIdV2 = C.AssetId (policyIdV2 verifySchnorrPolicyV2) schnorrAssetName

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
    (policyIdV1 verifySchnorrPolicyV1,
     mintScriptWitness era plutusL1 (Left verifySchnorrPolicyScriptV1) verifySchnorrRedeemer)

verifySchnorrMintWitnessV2 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifySchnorrMintWitnessV2 era =
    (policyIdV2 verifySchnorrPolicyV2,
     mintScriptWitness era plutusL2 (Left verifySchnorrPolicyScriptV2) verifySchnorrRedeemer)

-- ECDSA minting policy --

{-# INLINABLE mkVerifyEcdsaPolicy #-}
mkVerifyEcdsaPolicy :: Secp256Params -> sc -> Bool
mkVerifyEcdsaPolicy Secp256Params{..} _sc = BI.verifyEcdsaSecp256k1Signature vkey msg sig

verifyEcdsaPolicyV1 :: MintingPolicy
verifyEcdsaPolicyV1 = mkMintingPolicyScript
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV1.ScriptContext mkVerifyEcdsaPolicy

verifyEcdsaPolicyV2 :: MintingPolicy
verifyEcdsaPolicyV2 = mkMintingPolicyScript
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV2.ScriptContext mkVerifyEcdsaPolicy

verifyEcdsaPolicyScriptV1 :: C.PlutusScript C.PlutusScriptV1
verifyEcdsaPolicyScriptV1 = policyScript verifyEcdsaPolicyV1

verifyEcdsaPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
verifyEcdsaPolicyScriptV2 = policyScript verifyEcdsaPolicyV2

ecdsaAssetName :: C.AssetName
ecdsaAssetName = C.AssetName "ECDSA"

verifyEcdsaAssetIdV1 :: C.AssetId
verifyEcdsaAssetIdV1 = C.AssetId (policyIdV1 verifyEcdsaPolicyV1) ecdsaAssetName

verifyEcdsaAssetIdV2 :: C.AssetId
verifyEcdsaAssetIdV2 = C.AssetId (policyIdV2 verifyEcdsaPolicyV2) ecdsaAssetName

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
    (policyIdV1 verifyEcdsaPolicyV1,
     mintScriptWitness era plutusL1 (Left verifyEcdsaPolicyScriptV1) verifyEcdsaRedeemer)

verifyEcdsaMintWitnessV2 :: C.CardanoEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyEcdsaMintWitnessV2 era =
    (policyIdV2 verifyEcdsaPolicyV2,
     mintScriptWitness era plutusL2 (Left verifyEcdsaPolicyScriptV2) verifyEcdsaRedeemer)
