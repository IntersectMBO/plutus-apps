{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module PlutusScripts.V1TxInfo (
    txInfoInputs
  , txInfoOutputs
  , txInfoFee
  , txInfoMint
  , txInfoSigs
  , txInfoData
  , checkV1TxInfoScriptV1
  , checkV1TxInfoAssetIdV1
  , checkV1TxInfoRedeemer
  , checkV1TxInfoMintWitnessV1
  ) where

import Cardano.Api qualified as C
import Ledger.Tx.CardanoAPI.Internal (fromCardanoPaymentKeyHash, fromCardanoScriptData, fromCardanoTxIn,
                                      fromCardanoTxOutToPV1TxInfoTxOut, fromCardanoTxOutToPV1TxInfoTxOut',
                                      fromCardanoValue)
import Plutus.Script.Utils.Typed (IsScriptContext (mkUntypedMintingPolicy))
import Plutus.V1.Ledger.Api (mkMintingPolicyScript)
import Plutus.V1.Ledger.Api qualified as PlutusV1
import Plutus.V1.Ledger.Interval qualified as P
import PlutusScripts.Helpers (mintScriptWitness', plutusL1, policyIdV1, policyScript, toScriptData)
import PlutusTx qualified
import PlutusTx.Builtins qualified as P
import PlutusTx.Prelude qualified as P

data V1TxInfo = V1TxInfo
  { expTxInfoInputs      :: [PlutusV1.TxInInfo] -- ^ Transaction inputs; cannot be an empty list
  , expTxInfoOutputs     :: [PlutusV1.TxOut] -- ^ Transaction outputs
  , expTxInfoFee         :: PlutusV1.Value -- ^ The fee paid by this transaction.
  , expTxInfoMint        :: PlutusV1.Value -- ^ The 'Value' minted by this transaction.
  , expTxInfoDCert       :: [PlutusV1.DCert] -- ^ Digests of certificates included in this transaction
  , expTxInfoWdrl        :: [(PlutusV1.StakingCredential, Integer)] -- ^ Withdrawals
  , expTxInfoValidRange  :: PlutusV1.POSIXTimeRange -- ^ The valid range for the transaction.
  , expTxInfoSignatories :: [PlutusV1.PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
  , expTxInfoData        :: [(PlutusV1.DatumHash, PlutusV1.Datum)] -- ^ The lookup table of datums attached to the transaction
  -- , expTxInfoId          :: PlutusV1.TxId  -- ^ Hash of the pending transaction body (i.e. transaction excluding witnesses). Cannot be verified onchain.
  }
PlutusTx.unstableMakeIsData ''V1TxInfo

checkV1TxInfoRedeemer :: [PlutusV1.TxInInfo] -> [PlutusV1.TxOut] -> PlutusV1.Value ->
  PlutusV1.Value -> [PlutusV1.DCert] -> [(PlutusV1.StakingCredential, Integer)] ->
  PlutusV1.POSIXTimeRange -> [PlutusV1.PubKeyHash] -> [(PlutusV1.DatumHash, PlutusV1.Datum)] -> C.ScriptData
checkV1TxInfoRedeemer expIns expOuts expFee expMint expDCert expWdrl expRange expSigs expData =
  toScriptData $ V1TxInfo expIns expOuts expFee expMint expDCert expWdrl  expRange expSigs expData

txInfoInputs :: (C.TxIn, C.TxOut C.CtxUTxO era) -> PlutusV1.TxInInfo
txInfoInputs (txIn, txOut) = do PlutusV1.TxInInfo {
      PlutusV1.txInInfoOutRef = fromCardanoTxIn txIn
    , PlutusV1.txInInfoResolved = fromCardanoTxOutToPV1TxInfoTxOut' txOut
    }

txInfoOutputs :: [C.TxOut C.CtxTx era] -> [PlutusV1.TxOut]
txInfoOutputs = map fromCardanoTxOutToPV1TxInfoTxOut

txInfoFee :: C.Lovelace -> PlutusV1.Value
txInfoFee = fromCardanoValue . C.lovelaceToValue

txInfoMint :: C.Value -> PlutusV1.Value
txInfoMint = fromCardanoValue

txInfoSigs :: [C.VerificationKey C.PaymentKey] -> [PlutusV1.PubKeyHash]
txInfoSigs = map (fromCardanoPaymentKeyHash . C.verificationKeyHash)

txInfoData :: [C.ScriptData] -> [(PlutusV1.DatumHash, PlutusV1.Datum)]
txInfoData = map (\ datum ->
  (PlutusV1.DatumHash $ PlutusV1.toBuiltin $ C.serialiseToRawBytes $ C.hashScriptData datum,
  PlutusV1.Datum $ fromCardanoScriptData datum))

-- minting policy --

{-# INLINABLE mkCheckV1TxInfo #-}
mkCheckV1TxInfo :: V1TxInfo -> PlutusV1.ScriptContext -> Bool
mkCheckV1TxInfo V1TxInfo{..} ctx =
  P.traceIfFalse "unexpected txInfoInputs" checkTxInfoInputs &&
  P.traceIfFalse "unexpected txInfoOutputs" checkTxInfoOutputs &&
  P.traceIfFalse "unexpected txInfoFee" checkTxInfoFee &&
  P.traceIfFalse "unexpected txInfoMint" checkTxInfoMint &&
  P.traceIfFalse "unexpected txInfoDCert" checkTxInfoDCert &&
  P.traceIfFalse "unexpected txInfoWdrl" checkTxInfoWdrl &&
  P.traceIfFalse "provided range doesn't contain txInfoValidRange" checkTxInfoValidRange &&
  P.traceIfFalse "unexpected txInfoSignatories" checkTxInfoSignatories &&
  P.traceIfFalse "unexpected txInfoData" checkTxInfoData &&
  P.traceIfFalse "txInfoId isn't the expected TxId length" checkTxInfoId
 where
  info :: PlutusV1.TxInfo
  info = PlutusV1.scriptContextTxInfo ctx

  checkTxInfoInputs = expTxInfoInputs P.== PlutusV1.txInfoInputs info
  checkTxInfoOutputs = expTxInfoOutputs P.== PlutusV1.txInfoOutputs info
  checkTxInfoFee = expTxInfoFee P.== PlutusV1.txInfoFee info
  checkTxInfoMint = expTxInfoMint P.== PlutusV1.txInfoMint info
  checkTxInfoDCert = expTxInfoDCert P.== PlutusV1.txInfoDCert info
  checkTxInfoWdrl = expTxInfoWdrl P.== PlutusV1.txInfoWdrl info
  checkTxInfoValidRange = expTxInfoValidRange `P.contains` PlutusV1.txInfoValidRange info
  checkTxInfoSignatories = expTxInfoSignatories P.== PlutusV1.txInfoSignatories info
  checkTxInfoData = expTxInfoData P.== PlutusV1.txInfoData info
  checkTxInfoId = P.equalsInteger 32 (P.lengthOfByteString P.$ PlutusV1.getTxId P.$ PlutusV1.txInfoId info)

checkV1TxInfoV1 :: PlutusV1.MintingPolicy
checkV1TxInfoV1 = mkMintingPolicyScript
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV1.ScriptContext mkCheckV1TxInfo

checkV1TxInfoScriptV1 :: C.PlutusScript C.PlutusScriptV1
checkV1TxInfoScriptV1 = policyScript checkV1TxInfoV1

checkV1TxInfoAssetIdV1 :: C.AssetId
checkV1TxInfoAssetIdV1 = C.AssetId (policyIdV1 checkV1TxInfoV1) "V1TxInfo"

checkV1TxInfoMintWitnessV1 :: C.CardanoEra era
  -> C.ScriptData
  -> C.ExecutionUnits
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
checkV1TxInfoMintWitnessV1 era redeemer exunits =
  (policyIdV1 checkV1TxInfoV1,
   mintScriptWitness' era plutusL1 (Left checkV1TxInfoScriptV1) redeemer exunits)
