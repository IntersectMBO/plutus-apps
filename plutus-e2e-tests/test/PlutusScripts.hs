{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra

module PlutusScripts (
    alwaysSucceedPolicyScriptV2
  , alwaysSucceedAssetIdV2
  , alwaysSucceedMintWitnessV2

  , alwaysSucceedSpendScriptV2
  , alwaysSucceedSpendScriptHashV2
  , alwaysSucceedSpendWitnessV2

  , verifySchnorrAssetIdV1
  , verifySchnorrAssetIdV2
  , verifySchnorrMintWitnessV1
  , verifySchnorrMintWitnessV2

  , verifyEcdsaAssetIdV1
  , verifyEcdsaAssetIdV2
  , verifyEcdsaMintWitnessV1
  , verifyEcdsaMintWitnessV2

  , toScriptData
  , unPlutusScriptV2
  ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Codec.Serialise (serialise)
import Data.ByteString qualified as BS (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Plutus.Script.Utils.Typed (IsScriptContext (mkUntypedMintingPolicy))
import Plutus.Script.Utils.V2.Address as PSU.V2
import Plutus.V1.Ledger.Api (Address, MintingPolicy (MintingPolicy), Validator (Validator), mkMintingPolicyScript,
                             mkValidatorScript, unMintingPolicyScript, unValidatorScript)
import Plutus.V1.Ledger.Api qualified as PlutusV1
import Plutus.V1.Ledger.Bytes qualified as P (bytes, fromHex)
import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude qualified as P

-- | Treat string of hexidecimal bytes literally, without encoding. Useful for hashes.
bytesFromHex :: BS.ByteString -> BS.ByteString
bytesFromHex = P.bytes . fromEither . P.fromHex
  where
    fromEither (Left e)  = error $ show e
    fromEither (Right b) = b

-- | Default execution units with zero values. Needed for valid script witness in txbody.
--   Useful when exunits are automatically balanced.
defExecutionUnits :: C.ExecutionUnits
defExecutionUnits = C.ExecutionUnits {C.executionSteps = 0, C.executionMemory = 0 }

-- | Any data to ScriptData. Used for script datum and redeemer.
toScriptData :: PlutusTx.ToData a => a -> C.ScriptData
toScriptData a = C.fromPlutusData $ PlutusTx.toData a

plutusL1 :: C.ScriptLanguage C.PlutusScriptV1
plutusL1 = C.PlutusScriptLanguage C.PlutusScriptV1

plutusL2 :: C.ScriptLanguage C.PlutusScriptV2
plutusL2 = C.PlutusScriptLanguage C.PlutusScriptV2

-- | Witness token mint for including in txbody's txMintValue
--   Provide either the script or TxIn for reference script to include in witness
mintScriptWitness :: C.CardanoEra era
  -> C.ScriptLanguage lang
  -> Either (C.PlutusScript lang) C.TxIn -- either script or reference to script
  -> C.ScriptData
  -> C.ScriptWitness C.WitCtxMint era
-- V1 script
mintScriptWitness era lang@(C.PlutusScriptLanguage C.PlutusScriptV1) (Left script) redeemer = do
    C.PlutusScriptWitness (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
      C.PlutusScriptV1 (C.PScript script) C.NoScriptDatumForMint redeemer defExecutionUnits
-- V2 script
mintScriptWitness era lang@(C.PlutusScriptLanguage C.PlutusScriptV2) (Left script) redeemer = do
    C.PlutusScriptWitness (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
      C.PlutusScriptV2 (C.PScript script) C.NoScriptDatumForMint redeemer defExecutionUnits
-- V2 reference script
mintScriptWitness era lang@(C.PlutusScriptLanguage C.PlutusScriptV2) (Right refTxIn) redeemer = do
    C.PlutusScriptWitness (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
      C.PlutusScriptV2 (C.PReferenceScript refTxIn Nothing) C.NoScriptDatumForMint redeemer defExecutionUnits

spendScriptWitness :: C.CardanoEra era
  -> C.ScriptLanguage lang
  -> Either (C.PlutusScript lang) C.TxIn -- either script or reference to script
  -> (C.ScriptDatum C.WitCtxTxIn)
  -> C.ScriptData
  -> C.ScriptWitness C.WitCtxTxIn era
-- V2 reference script
spendScriptWitness era lang@(C.PlutusScriptLanguage C.PlutusScriptV2) (Right refTxIn) datumWit redeemer = do
    C.PlutusScriptWitness (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
      C.PlutusScriptV2 (C.PReferenceScript refTxIn Nothing) datumWit redeemer defExecutionUnits -- tried with (Just scriptHash) instead of Nothing because hash isn't needed?

-- | Produce ScriptLanguageInEra. Throw error when era doesn't support the script language.
maybeScriptWitness :: C.CardanoEra era
    -> C.ScriptLanguage l
    -> Maybe (C.ScriptLanguageInEra l era)
    -> C.ScriptLanguageInEra l era
maybeScriptWitness era lang Nothing = error $ "Era " ++ show era
                                    ++ " does not support script language " ++ show lang
maybeScriptWitness _ _ (Just p) = p

-- | Serialised plutus script from minting policy
policyScript :: MintingPolicy -> C.PlutusScript lang
policyScript = C.PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . unMintingPolicyScript

-- | Serialised plutus script from validator
validatorScript :: Validator -> C.PlutusScript lang
validatorScript = C.PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . unValidatorScript

-- | V1 Plutus Script to general Script, Needed for producing reference script.
unPlutusScriptV1 :: C.PlutusScript C.PlutusScriptV1 -> C.Script C.PlutusScriptV1
unPlutusScriptV1 = C.PlutusScript C.PlutusScriptV1

-- | V2 Plutus Script to general Script, Needed for producing reference script.
unPlutusScriptV2 :: C.PlutusScript C.PlutusScriptV2 -> C.Script C.PlutusScriptV2
unPlutusScriptV2 = C.PlutusScript C.PlutusScriptV2

-- | PolicyId of a V1 minting policy
policyIdV1 :: MintingPolicy -> C.PolicyId
policyIdV1 = C.scriptPolicyId . unPlutusScriptV1 . policyScript

-- | PolicyId of a V2 minting policy
policyIdV2 :: MintingPolicy -> C.PolicyId
policyIdV2 = C.scriptPolicyId . unPlutusScriptV2 . policyScript

-- AlwaysSucceeds minting policy --

alwaysSucceedPolicy :: MintingPolicy
alwaysSucceedPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| \_ _ -> () ||])

alwaysSucceedPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
alwaysSucceedPolicyScriptV2 = policyScript alwaysSucceedPolicy

alwaysSucceedAssetIdV2 :: C.AssetId
alwaysSucceedAssetIdV2 = C.AssetId (policyIdV2 alwaysSucceedPolicy) ""

-- | Witness token mint for including in txbody's txMintValue
-- Use Nothing to include script in witness, else provide TxIn to reference script
alwaysSucceedMintWitnessV2 :: C.CardanoEra era
  -> Maybe C.TxIn -- maybe reference input
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysSucceedMintWitnessV2 era Nothing =
    (policyIdV2 alwaysSucceedPolicy,
     mintScriptWitness era plutusL2 (Left alwaysSucceedPolicyScriptV2) (toScriptData ()))
alwaysSucceedMintWitnessV2 era (Just refTxIn) =
    (policyIdV2 alwaysSucceedPolicy,
     mintScriptWitness era plutusL2 (Right refTxIn) (toScriptData ()))

-- AlwaysSucceeds validator --

alwaysSucceedSpend :: Validator
alwaysSucceedSpend = mkValidatorScript $$(PlutusTx.compile [|| \_ _ _ -> () ||])

alwaysSucceedSpendScriptV2 :: C.PlutusScript C.PlutusScriptV2
alwaysSucceedSpendScriptV2 = validatorScript alwaysSucceedSpend

alwaysSucceedSpendScriptHashV2 :: C.ScriptHash
alwaysSucceedSpendScriptHashV2 = C.hashScript $ C.PlutusScript C.PlutusScriptV2 alwaysSucceedSpendScriptV2

alwaysSucceedSpendWitnessV2 :: C.CardanoEra era
  -> Maybe C.TxIn
  -> Maybe C.ScriptData
  -> C.Witness C.WitCtxTxIn era
alwaysSucceedSpendWitnessV2 era mRefScript mDatum =
    C.ScriptWitness C.ScriptWitnessForSpending $ spendScriptWitness era plutusL2
      (maybe (Left alwaysSucceedSpendScriptV2) (\refScript -> Right refScript) mRefScript) -- script or reference script
      (maybe C.InlineScriptDatum (\datum -> C.ScriptDatumForTxIn datum) mDatum) -- inline datum or datum value
      (toScriptData ()) -- redeemer

---- SECP256k1 ----

data Secp256Params = Secp256Params
    { vkey :: !P.BuiltinByteString,
      msg  :: !P.BuiltinByteString,
      sig  :: !P.BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''Secp256Params

-- Schnorr minting policy --

{-# INLINABLE mkVerifySchnorrPolicy #-}
mkVerifySchnorrPolicy :: (BI.BuiltinByteString, BI.BuiltinByteString, BI.BuiltinByteString)
                      -> sc
                      -> Bool
mkVerifySchnorrPolicy (v, m, s) _sc = BI.verifySchnorrSecp256k1Signature v m s

verifySchnorrPolicyV1 :: MintingPolicy
verifySchnorrPolicyV1 = mkMintingPolicyScript
  $$(PlutusTx.compile [||mkUntypedMintingPolicy @PlutusV1.ScriptContext mkVerifySchnorrPolicy||])

verifySchnorrPolicyV2 :: MintingPolicy
verifySchnorrPolicyV2 = mkMintingPolicyScript
  $$(PlutusTx.compile [||mkUntypedMintingPolicy @PlutusV2.ScriptContext mkVerifySchnorrPolicy||])

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
mkVerifyEcdsaPolicy :: (BI.BuiltinByteString, BI.BuiltinByteString, BI.BuiltinByteString)
                    -> sc
                    -> Bool
mkVerifyEcdsaPolicy (v, m, s) _sc = BI.verifyEcdsaSecp256k1Signature v m s

verifyEcdsaPolicyV1 :: MintingPolicy
verifyEcdsaPolicyV1 = mkMintingPolicyScript
  $$(PlutusTx.compile [||mkUntypedMintingPolicy @PlutusV1.ScriptContext mkVerifyEcdsaPolicy||])

verifyEcdsaPolicyV2 :: MintingPolicy
verifyEcdsaPolicyV2 = mkMintingPolicyScript
  $$(PlutusTx.compile [||mkUntypedMintingPolicy @PlutusV2.ScriptContext mkVerifyEcdsaPolicy||])

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
