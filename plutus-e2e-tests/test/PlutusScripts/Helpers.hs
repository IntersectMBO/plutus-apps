{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra

module PlutusScripts.Helpers where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Codec.Serialise (serialise)
import Data.ByteString qualified as BS (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Plutus.Script.Utils.Value (CurrencySymbol)
import Plutus.V1.Ledger.Api (MintingPolicy, Validator, unMintingPolicyScript, unValidatorScript)
import Plutus.V1.Ledger.Api qualified as PlutusV1
import Plutus.V1.Ledger.Bytes qualified as P (bytes, fromHex)
import Plutus.V1.Ledger.Scripts (Datum (Datum), Redeemer (Redeemer))
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI

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

asRedeemer :: PlutusTx.ToData a => a -> Redeemer
asRedeemer a = Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a

asDatum :: PlutusTx.ToData a => a -> Datum
asDatum a = Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a

plutusL1 :: C.ScriptLanguage C.PlutusScriptV1
plutusL1 = C.PlutusScriptLanguage C.PlutusScriptV1

plutusL2 :: C.ScriptLanguage C.PlutusScriptV2
plutusL2 = C.PlutusScriptLanguage C.PlutusScriptV2

-- | Witness token mint for including in txbody's txMintValue.
--   Provide either the script or TxIn for reference script to include in witness.
--   Zero execution units can only be used with convenience build function.
mintScriptWitness :: C.CardanoEra era
  -> C.ScriptLanguage lang
  -> Either (C.PlutusScript lang) C.TxIn -- either script or reference to script
  -> C.ScriptData
  -> C.ScriptWitness C.WitCtxMint era
-- V1 script
mintScriptWitness era lang@(C.PlutusScriptLanguage C.PlutusScriptV1) eScript redeemer =
    mintScriptWitness' era lang eScript redeemer defExecutionUnits
-- V2 script
mintScriptWitness era lang@(C.PlutusScriptLanguage C.PlutusScriptV2) (Left script) redeemer = do
    C.PlutusScriptWitness (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
      C.PlutusScriptV2 (C.PScript script) C.NoScriptDatumForMint redeemer defExecutionUnits
-- V2 reference script
mintScriptWitness era lang@(C.PlutusScriptLanguage C.PlutusScriptV2) (Right refTxIn) redeemer = do
    C.PlutusScriptWitness (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
      C.PlutusScriptV2 (C.PReferenceScript refTxIn Nothing) C.NoScriptDatumForMint redeemer defExecutionUnits

-- Witness token mint with explicit execution units. Used when building raw txbody content.
mintScriptWitness' :: C.CardanoEra era
  -> C.ScriptLanguage lang
  -> Either (C.PlutusScript lang) C.TxIn -- either script or reference to script
  -> C.ScriptData
  -> C.ExecutionUnits
  -> C.ScriptWitness C.WitCtxMint era
-- V1 script
mintScriptWitness' era lang@(C.PlutusScriptLanguage C.PlutusScriptV1) (Left script) redeemer = do
    C.PlutusScriptWitness (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
      C.PlutusScriptV1 (C.PScript script) C.NoScriptDatumForMint redeemer
-- V2 script
mintScriptWitness' era lang@(C.PlutusScriptLanguage C.PlutusScriptV2) (Left script) redeemer = do
    C.PlutusScriptWitness (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
      C.PlutusScriptV2 (C.PScript script) C.NoScriptDatumForMint redeemer
-- V2 reference script
mintScriptWitness' era lang@(C.PlutusScriptLanguage C.PlutusScriptV2) (Right refTxIn) redeemer = do
    C.PlutusScriptWitness (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
      C.PlutusScriptV2 (C.PReferenceScript refTxIn Nothing) C.NoScriptDatumForMint redeemer

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

fromPolicyId :: C.PolicyId -> CurrencySymbol
fromPolicyId (C.PolicyId hash) = PlutusV1.CurrencySymbol . BI.toBuiltin $ C.serialiseToRawBytes hash
