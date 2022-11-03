{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}

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
  , toCardanoDatumWitness
  , toCardanoTxInReferenceWitnessHeader
  , toCardanoTxInScriptWitnessHeader
  , toCardanoMintValue
  , ToCardanoError(..)
  , FromCardanoError(..)
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Data.Bitraversable (bisequence)
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
    txInsCollateral <- toCardanoTxInsCollateral txCollateralInputs
    let txOuts = P.getTxOut <$> txOutputs
    -- Workaround for missing export https://github.com/input-output-hk/cardano-node/pull/4496
    (returnCollateral, totalCollateral) <- case C.totalAndReturnCollateralSupportedInEra C.BabbageEra of
      Just txTotalAndReturnCollateralInBabbageEra ->
        (maybe C.TxReturnCollateralNone (C.TxReturnCollateral txTotalAndReturnCollateralInBabbageEra . P.getTxOut) txReturnCollateral,)
        <$> maybe (pure C.TxTotalCollateralNone) (fmap (C.TxTotalCollateral txTotalAndReturnCollateralInBabbageEra) . toCardanoLovelace) txTotalCollateral
      Nothing -> pure (C.TxReturnCollateralNone, C.TxTotalCollateralNone)
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
        , txTotalCollateral = totalCollateral
        , txReturnCollateral = returnCollateral
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

toWithdrawals :: P.ScriptsMap
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
        Just (redeemer, script) -> C.BuildTxWith . C.ScriptWitness C.ScriptWitnessForStakeAddr <$> toCardanoScriptWitness C.NoScriptDatumForStake redeemer (Left $ fmap P.getValidator script)
        Nothing                    -> Left MissingStakeValidator

toCardanoMintWitness :: PV1.Redeemer -> Maybe (P.Versioned PV1.TxOutRef) -> Maybe (P.Versioned PV1.MintingPolicy) -> Either ToCardanoError (C.ScriptWitness C.WitCtxMint C.BabbageEra)
toCardanoMintWitness _ Nothing Nothing = Left MissingMintingPolicy
toCardanoMintWitness redeemer (Just ref) _ =
  toCardanoScriptWitness C.NoScriptDatumForMint redeemer (Right ref)
toCardanoMintWitness redeemer _ (Just script) =
  toCardanoScriptWitness C.NoScriptDatumForMint redeemer (Left (fmap P.getMintingPolicy script))

toCardanoScriptWitness :: PV1.ToData a =>
  C.ScriptDatum witctx
  -> a
  -> Either (P.Versioned PV1.Script) (P.Versioned PV1.TxOutRef)
  -> Either ToCardanoError (C.ScriptWitness witctx C.BabbageEra)
toCardanoScriptWitness datum redeemer scriptOrRef = (case lang of
    P.PlutusV1 ->
      C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1
          <$> (case scriptOrRef of
            Left (P.Versioned script _) -> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) script)
            Right (P.Versioned ref _) -> flip C.PReferenceScript Nothing <$> (toCardanoTxIn ref)
          )
    P.PlutusV2 ->
      C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2
          <$> (case scriptOrRef of
            Left (P.Versioned script _) -> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) script)
            Right (P.Versioned ref _) -> flip C.PReferenceScript Nothing <$> (toCardanoTxIn ref)
          )
  ) <*> pure datum
    <*> pure (C.fromPlutusData $ PV1.toData redeemer)
    <*> pure zeroExecutionUnits
  where
    lang = either P.version P.version scriptOrRef

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
toCardanoTxInsCollateral [] = pure C.TxInsCollateralNone
toCardanoTxInsCollateral inputs = fmap (C.TxInsCollateral C.CollateralInBabbageEra) (traverse (toCardanoTxIn . P.txInputRef) inputs)

toCardanoTxInWitness :: P.Tx -> P.TxInputType -> Either ToCardanoError (C.Witness C.WitCtxTxIn C.BabbageEra)
toCardanoTxInWitness _ P.TxConsumePublicKeyAddress = pure (C.KeyWitness C.KeyWitnessForSpending)
toCardanoTxInWitness _ P.TxConsumeSimpleScriptAddress = Left SimpleScriptsNotSupportedToCardano -- TODO: Better support for simple scripts
toCardanoTxInWitness tx
    (P.TxScriptAddress
        (P.Redeemer redeemer)
        valhOrRef
        dh)
    = do
      mDatum <- traverse (maybe (Left MissingDatum) pure . (`Map.lookup` P.txData tx)) dh
      mkWitness <- case valhOrRef of
        Left valh -> maybe (Left MissingInputValidator) (toCardanoTxInScriptWitnessHeader . fmap PV1.getValidator) $ P.lookupValidator (P.txScripts tx) valh
        Right vref -> toCardanoTxInReferenceWitnessHeader vref
      pure $ C.ScriptWitness C.ScriptWitnessForSpending $ mkWitness
            (toCardanoDatumWitness mDatum)
            (toCardanoScriptData redeemer)
            zeroExecutionUnits

toCardanoDatumWitness :: Maybe PV1.Datum -> C.ScriptDatum C.WitCtxTxIn
toCardanoDatumWitness = maybe C.InlineScriptDatum (C.ScriptDatumForTxIn . toCardanoScriptData . PV1.getDatum)

type WitnessHeader = C.ScriptDatum C.WitCtxTxIn -> C.ScriptRedeemer -> C.ExecutionUnits -> C.ScriptWitness C.WitCtxTxIn C.BabbageEra

toCardanoTxInReferenceWitnessHeader :: P.Versioned PV1.TxOutRef -> Either ToCardanoError WitnessHeader
toCardanoTxInReferenceWitnessHeader (P.Versioned ref lang) = do
    txIn <- toCardanoTxIn ref
    pure $ case lang of
        P.PlutusV1 ->
            C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1 $
                C.PReferenceScript txIn Nothing
        P.PlutusV2 ->
            C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2 $
                C.PReferenceScript txIn Nothing

toCardanoTxInScriptWitnessHeader :: P.Versioned PV1.Script -> Either ToCardanoError WitnessHeader
toCardanoTxInScriptWitnessHeader (P.Versioned script lang) =
    case lang of
        P.PlutusV1 ->
            C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1 . C.PScript <$>
                toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) script
        P.PlutusV2 ->
            C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2 . C.PScript <$>
                toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) script

toCardanoMintValue :: P.Tx -> Either ToCardanoError (C.TxMintValue C.BuildTx C.BabbageEra)
toCardanoMintValue tx@P.Tx{..} =
    let indexedMps = Map.assocs txMintingWitnesses
    in C.TxMintValue C.MultiAssetInBabbageEra
       <$> toCardanoValue txMint
       <*> fmap (C.BuildTxWith . Map.fromList)
             (traverse (\(mph, (rd, mTxOutRef)) ->
                bisequence (toCardanoPolicyId mph, toCardanoMintWitness rd mTxOutRef (P.lookupMintingPolicy (P.txScripts tx) mph)))
                indexedMps)
