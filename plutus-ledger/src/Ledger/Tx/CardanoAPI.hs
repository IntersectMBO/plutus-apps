{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -Wno-orphans        #-}

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
  , ToCardanoError(..)
  , FromCardanoError(..)
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Ledger.Address qualified as P
import Ledger.Params qualified as P
import Ledger.Scripts qualified as P
import Ledger.Tx.CardanoAPI.Internal
import Ledger.Tx.Internal qualified as P

toCardanoTxBodyContent
    :: P.Params -- ^ Parameters to use.
    -> [P.PaymentPubKeyHash] -- ^ Required signers of the transaction
    -> P.Tx
    -> Either ToCardanoError CardanoBuildTx
toCardanoTxBodyContent P.Params{P.pProtocolParams} sigs P.Tx{..} = do
    txIns <- traverse toCardanoTxInBuild txInputs
    txInsReference <- traverse (\(P.TxIn ref _) -> toCardanoTxIn ref) txReferenceInputs
    txInsCollateral <- toCardanoTxInsCollateral txCollateral
    let txOuts = P.getTxOut <$> txOutputs
    txFee' <- toCardanoFee txFee
    txValidityRange <- toCardanoValidityRange txValidRange
    txMintValue <- toCardanoMintValue txRedeemers txMint txMintScripts
    txExtraKeyWits <- C.TxExtraKeyWitnesses C.ExtraKeyWitnessesInBabbageEra <$> traverse toCardanoPaymentKeyHash sigs
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
        , txWithdrawals = C.TxWithdrawalsNone
        , txCertificates = C.TxCertificatesNone
        , txUpdateProposal = C.TxUpdateProposalNone
        }

toCardanoTxBody ::
    P.Params -- ^ Parameters to use.
    -> [P.PaymentPubKeyHash] -- ^ Required signers of the transaction
    -> P.Tx
    -> Either ToCardanoError (C.TxBody C.BabbageEra)
toCardanoTxBody params sigs tx = do
    txBodyContent <- toCardanoTxBodyContent params sigs tx
    makeTransactionBody mempty txBodyContent

toCardanoTxInBuild :: P.TxIn -> Either ToCardanoError (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra))
toCardanoTxInBuild (P.TxIn txInRef (Just txInType)) = (,) <$> toCardanoTxIn txInRef <*> (C.BuildTxWith <$> toCardanoTxInWitness txInType)
toCardanoTxInBuild (P.TxIn _ Nothing) = Left MissingTxInType

fromCardanoTxInsCollateral :: C.TxInsCollateral era -> [P.TxIn]
fromCardanoTxInsCollateral C.TxInsCollateralNone       = []
fromCardanoTxInsCollateral (C.TxInsCollateral _ txIns) = map (P.pubKeyTxIn . fromCardanoTxIn) txIns

toCardanoTxInsCollateral :: [P.TxIn] -> Either ToCardanoError (C.TxInsCollateral C.BabbageEra)
toCardanoTxInsCollateral = fmap (C.TxInsCollateral C.CollateralInBabbageEra) . traverse (toCardanoTxIn . P.txInRef)

toCardanoTxInWitness :: P.TxInType -> Either ToCardanoError (C.Witness C.WitCtxTxIn C.BabbageEra)
toCardanoTxInWitness P.ConsumePublicKeyAddress = pure (C.KeyWitness C.KeyWitnessForSpending)
toCardanoTxInWitness P.ConsumeSimpleScriptAddress = Left SimpleScriptsNotSupportedToCardano -- TODO: Better support for simple scripts
toCardanoTxInWitness
    (P.ConsumeScriptAddress
        P.PlutusV1
        (P.Validator validator)
        (P.Redeemer redeemer)
        (P.Datum datum))
    = C.ScriptWitness C.ScriptWitnessForSpending <$>
        (C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1
        <$> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) validator)
        <*> pure (C.ScriptDatumForTxIn $ toCardanoScriptData datum)
        <*> pure (toCardanoScriptData redeemer)
        <*> pure zeroExecutionUnits
        )
toCardanoTxInWitness
    (P.ConsumeScriptAddress
        P.PlutusV2
        (P.Validator validator)
        (P.Redeemer redeemer)
        (P.Datum datum))
    = C.ScriptWitness C.ScriptWitnessForSpending <$>
        (C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2
        <$> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) validator)
        <*> pure (C.ScriptDatumForTxIn $ toCardanoScriptData datum)
        <*> pure (toCardanoScriptData redeemer)
        <*> pure zeroExecutionUnits
        )
