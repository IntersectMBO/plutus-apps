{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra

module PlutusScripts.AlwaysSucceeds (
    alwaysSucceedPolicyScriptV2
  , alwaysSucceedAssetIdV2
  , alwaysSucceedMintWitnessV2

  , alwaysSucceedSpendScriptV2
  , alwaysSucceedSpendScriptHashV2
  , alwaysSucceedSpendWitnessV2
  ) where

import Cardano.Api qualified as C
import Plutus.V1.Ledger.Api (MintingPolicy, Validator, mkMintingPolicyScript, mkValidatorScript)
import PlutusScripts.Helpers (mintScriptWitness, plutusL2, policyIdV2, policyScript, spendScriptWitness, toScriptData,
                              validatorScript)
import PlutusTx qualified

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
