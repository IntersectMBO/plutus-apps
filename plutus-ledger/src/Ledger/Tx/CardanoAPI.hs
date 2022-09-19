{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

{-|

Interface to the transaction types from 'cardano-api'

-}
module Ledger.Tx.CardanoAPI(
  module Ledger.Tx.CardanoAPI.Internal
  , CardanoBuildTx(..)
  , SomeCardanoApiTx(..)
  , fromCardanoTxInsCollateral
  , toCardanoTxBody
  , toCardanoTxBodyContent
  , toCardanoTxInsCollateral
  , toCardanoTxInWitness
  , toCardanoMintValue
  , ToCardanoError(..)
  , FromCardanoError(..)
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Data.Map (Map)
import Data.Map qualified as Map
import Ledger.Address qualified as P
import Ledger.Params qualified as P
import Ledger.Scripts qualified as P
import Ledger.Tx.CardanoAPI.Internal
import Ledger.Tx.Internal qualified as P
import Plutus.V1.Ledger.Api qualified as PV1

toCardanoTxBodyContent
    :: P.Params
    -> [P.PaymentPubKeyHash] -- ^ Required signers of the transaction
    -> P.Tx
    -> Either ToCardanoError CardanoBuildTx
toCardanoTxBodyContent P.Params{P.pProtocolParams, P.pNetworkId} sigs tx@P.Tx{..} = do
    -- TODO: translate all fields
    txIns <- traverse (toCardanoTxInBuild tx) txInputs
    txInsReference <- traverse (toCardanoTxIn . P.txInputRef) txReferenceInputs
    txInsCollateral <- toCardanoTxInsCollateral txCollateral
    let txOuts = P.getTxOut <$> txOutputs
    txFee' <- toCardanoFee txFee
    txValidityRange <- toCardanoValidityRange txValidRange
    txMintValue <- toCardanoMintValue tx
    txExtraKeyWits <- C.TxExtraKeyWitnesses C.ExtraKeyWitnessesInBabbageEra <$> traverse toCardanoPaymentKeyHash sigs
    withdrawals <- toWithdrawals txScripts pNetworkId txWithdrawals
    pure $ CardanoBuildTx $ C.TxBodyContent
        { txIns = txIns
        , txInsReference = C.TxInsReference C.ReferenceTxInsScriptsInlineDatumsInBabbageEra txInsReference
        , txInsCollateral = txInsCollateral
        , txOuts = txOuts
        , txTotalCollateral = C.TxTotalCollateralNone -- TODO Change when going to Babbage era txs
        , txReturnCollateral = C.TxReturnCollateralNone -- TODO Change when going to Babbage era txs
        , txFee = txFee'
        , txValidityRange = txValidityRange
        , txMintValue = txMintValue
        , txProtocolParams = C.BuildTxWith $ Just pProtocolParams
        , txScriptValidity = C.TxScriptValidityNone
        , txExtraKeyWits
        -- unused:
        , txMetadata = C.TxMetadataNone
        , txAuxScripts = C.TxAuxScriptsNone
        , txWithdrawals = withdrawals
        , txCertificates = C.TxCertificatesNone
        , txUpdateProposal = C.TxUpdateProposalNone
        }

toWithdrawals :: Map P.ScriptHash (P.Versioned P.Script)
  -> C.NetworkId
  -> [P.Withdrawal]
  -> Either ToCardanoError (C.TxWithdrawals C.BuildTx C.BabbageEra)
toWithdrawals txScripts networkId = \case
  [] -> pure C.TxWithdrawalsNone
  xs -> C.TxWithdrawals C.WithdrawalsInBabbageEra <$> mapM toWithdraw xs

  where
    toWithdraw P.Withdrawal{withdrawalCredential, withdrawalAmount, withdrawalRedeemer} = do
      saddr <- toCardanoStakeAddress networkId withdrawalCredential
      witness <- toStakeWitness withdrawalRedeemer withdrawalCredential
      pure (saddr, C.Lovelace withdrawalAmount, witness)

    toStakeWitness withdrawalRedeemer cred = case cred of
      PV1.PubKeyCredential _pkh -> pure $ C.BuildTxWith $ C.KeyWitness C.KeyWitnessForStakeAddr
      PV1.ScriptCredential _vh -> case (,) <$> withdrawalRedeemer <*> P.lookupValidator txScripts _vh of
        Just (redeemer, script) -> C.BuildTxWith . C.ScriptWitness C.ScriptWitnessForStakeAddr <$> toCardanoScriptWitness C.NoScriptDatumForStake redeemer (fmap P.getValidator script)
        Nothing                    -> Left MissingStakeValidator

toCardanoMintWitness :: PV1.Redeemer -> Maybe (P.Versioned PV1.MintingPolicy) -> Either ToCardanoError (C.ScriptWitness C.WitCtxMint C.BabbageEra)
toCardanoMintWitness _ Nothing = Left MissingMintingPolicy
toCardanoMintWitness redeemer (Just script) =
  toCardanoScriptWitness C.NoScriptDatumForMint redeemer (fmap P.getMintingPolicy script)

toCardanoScriptWitness :: PV1.ToData a =>
  C.ScriptDatum witctx
  -> a
  -> P.Versioned PV1.Script
  -> Either ToCardanoError (C.ScriptWitness witctx C.BabbageEra)
toCardanoScriptWitness datum redeemer (P.Versioned script lang) = (case lang of
    P.PlutusV1 ->
      C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1
          <$> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) script)
    P.PlutusV2 ->
      C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2
          <$> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) script)
  ) <*> pure datum
    <*> pure (C.fromPlutusData $ PV1.toData redeemer)
    <*> pure zeroExecutionUnits

toCardanoStakeAddress :: C.NetworkId -> PV1.Credential -> Either ToCardanoError C.StakeAddress
toCardanoStakeAddress networkId credential =
  C.StakeAddress (C.toShelleyNetwork networkId) . C.toShelleyStakeCredential <$> toCardanoStakingCredential credential

toCardanoStakingCredential :: PV1.Credential -> Either ToCardanoError C.StakeCredential
toCardanoStakingCredential (PV1.PubKeyCredential pubKeyHash) = C.StakeCredentialByKey <$> toCardanoStakeKeyHash pubKeyHash
toCardanoStakingCredential (PV1.ScriptCredential validatorHash) = C.StakeCredentialByScript <$> toCardanoScriptHash validatorHash


toCardanoTxBody ::
    P.Params -- ^ Parameters to use.
    -> [P.PaymentPubKeyHash] -- ^ Required signers of the transaction
    -> P.Tx
    -> Either ToCardanoError (C.TxBody C.BabbageEra)
toCardanoTxBody params sigs tx = do
    txBodyContent <- toCardanoTxBodyContent params sigs tx
    makeTransactionBody mempty txBodyContent

toCardanoTxInBuild :: P.Tx -> P.TxInput -> Either ToCardanoError (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra))
toCardanoTxInBuild tx (P.TxInput txInRef txInType) = (,) <$> toCardanoTxIn txInRef <*> (C.BuildTxWith <$> toCardanoTxInWitness tx txInType)

fromCardanoTxInsCollateral :: C.TxInsCollateral era -> [P.TxIn]
fromCardanoTxInsCollateral C.TxInsCollateralNone       = []
fromCardanoTxInsCollateral (C.TxInsCollateral _ txIns) = map (P.pubKeyTxIn . fromCardanoTxIn) txIns

toCardanoTxInsCollateral :: [P.TxInput] -> Either ToCardanoError (C.TxInsCollateral C.BabbageEra)
toCardanoTxInsCollateral = fmap (C.TxInsCollateral C.CollateralInBabbageEra) . traverse (toCardanoTxIn . P.txInputRef)

toCardanoTxInWitness :: P.Tx -> P.TxInputType -> Either ToCardanoError (C.Witness C.WitCtxTxIn C.BabbageEra)
toCardanoTxInWitness _ P.TxConsumePublicKeyAddress = pure (C.KeyWitness C.KeyWitnessForSpending)
toCardanoTxInWitness _ P.TxConsumeSimpleScriptAddress = Left SimpleScriptsNotSupportedToCardano -- TODO: Better support for simple scripts
toCardanoTxInWitness tx
    (P.TxConsumeScriptAddress
        (P.Redeemer redeemer)
        valh
        dh)
    = do
      (PV1.Datum datum) <- maybe (Left MissingDatum) pure $ Map.lookup dh (P.txData tx)
      (P.Versioned (P.Validator validator) lang) <- maybe (Left MissingInputValidator) pure $ P.lookupValidator (P.txScripts tx) valh
      case lang of
        P.PlutusV1 ->
          C.ScriptWitness C.ScriptWitnessForSpending <$>
            (C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1
            <$> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) validator)
            <*> pure (C.ScriptDatumForTxIn $ toCardanoScriptData datum)
            <*> pure (toCardanoScriptData redeemer)
            <*> pure zeroExecutionUnits
            )
        P.PlutusV2 ->
          C.ScriptWitness C.ScriptWitnessForSpending <$>
            (C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2
            <$> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) validator)
            <*> pure (C.ScriptDatumForTxIn $ toCardanoScriptData datum)
            <*> pure (toCardanoScriptData redeemer)
            <*> pure zeroExecutionUnits
            )

toCardanoMintValue :: P.Tx -> Either ToCardanoError (C.TxMintValue C.BuildTx C.BabbageEra)
toCardanoMintValue tx@P.Tx{..} =
    let indexedMps = Map.assocs txMintingScripts
     in C.TxMintValue C.MultiAssetInBabbageEra
        <$> toCardanoValue txMint
        <*> (C.BuildTxWith . Map.fromList <$> traverse (\(mph, rd) ->
          (,) <$> toCardanoPolicyId mph <*> toCardanoMintWitness rd (P.lookupMintingPolicy (P.txScripts tx) mph)) indexedMps)

