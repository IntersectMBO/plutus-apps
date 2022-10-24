{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE NamedFieldPuns           #-}
-- Code temporarily copied over from cardano-api,
-- until https://github.com/input-output-hk/cardano-node/pull/2936 or something similar gets merged.
module Ledger.Tx.CardanoAPITemp (makeTransactionBody', toShelleyTxOut) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Sequence.Strict qualified as Seq
import Data.Set qualified as Set
import Data.Word (Word64)

import Cardano.Api
import Cardano.Api.Shelley hiding (toShelleyTxOut)
import Cardano.Ledger.AuxiliaryData qualified as Ledger (hashAuxiliaryData)
import Cardano.Ledger.BaseTypes (StrictMaybe (..), maybeToStrictMaybe)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Serialization qualified as CBOR (mkSized)
import Ouroboros.Consensus.Shelley.Eras (StandardBabbage)

import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Era qualified as Ledger

import Cardano.Ledger.Alonzo.Data qualified as Alonzo
import Cardano.Ledger.Alonzo.Language qualified as Alonzo
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Alonzo.Tx qualified as Alonzo
import Cardano.Ledger.Alonzo.TxBody qualified as Alonzo
import Cardano.Ledger.Alonzo.TxWitness qualified as Alonzo

import Cardano.Ledger.Babbage.PParams qualified as Babbage
import Cardano.Ledger.Babbage.Tx qualified as Babbage
import Cardano.Ledger.Babbage.TxBody qualified as Babbage

import Cardano.Ledger.ShelleyMA.TxBody qualified as Allegra

import Cardano.Ledger.Keys qualified as Shelley
import Cardano.Ledger.Shelley.Tx qualified as Shelley
import Cardano.Ledger.Shelley.TxBody qualified as Shelley

makeTransactionBody'
    :: Map.Map Alonzo.RdmrPtr Alonzo.ExUnits
    -> TxBodyContent BuildTx BabbageEra
    -> Either TxBodyError (TxBody BabbageEra)
makeTransactionBody'
    exUnits
    txbodycontent@TxBodyContent {
        txIns,
        txInsCollateral,
        txInsReference,
        txOuts,
        txReturnCollateral,
        txTotalCollateral,
        txFee,
        txValidityRange = (lowerBound, upperBound),
        txExtraKeyWits,
        txWithdrawals,
        txCertificates,
        txMintValue,
        txScriptValidity,
        txProtocolParams,
        txMetadata,
        txAuxScripts
    } =
    return $
      ShelleyTxBody era
        (Babbage.TxBody
          { Babbage.inputs = Set.fromList (map (toShelleyTxIn . fst) txIns)
          , Babbage.collateral =
              case txInsCollateral of
                TxInsCollateralNone     -> Set.empty
                TxInsCollateral _ txins -> Set.fromList (map toShelleyTxIn txins)
          , Babbage.referenceInputs =
              case txInsReference of
                TxInsReferenceNone     -> Set.empty
                TxInsReference _ txins -> Set.fromList (map toShelleyTxIn txins)
          , Babbage.outputs = Seq.fromList (map (CBOR.mkSized . toShelleyTxOut era) txOuts)
          , Babbage.collateralReturn =
              case txReturnCollateral of
                TxReturnCollateralNone        -> SNothing
                TxReturnCollateral _ colTxOut -> SJust $ CBOR.mkSized $ toShelleyTxOut era colTxOut
          , Babbage.totalCollateral =
              case txTotalCollateral of
                TxTotalCollateralNone  -> SNothing
                TxTotalCollateral _ lv -> SJust $ toShelleyLovelace lv
          , Babbage.txcerts =
              case txCertificates of
                TxCertificatesNone    -> Seq.empty
                TxCertificates _ cs _ -> Seq.fromList (map toShelleyCertificate cs)
          , Babbage.txwdrls =
              case txWithdrawals of
                TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
                TxWithdrawals _ ws -> toShelleyWithdrawal ws
          , Babbage.txfee =
              case txFee of
                TxFeeImplicit era'  -> case era' of {}
                TxFeeExplicit _ fee -> toShelleyLovelace fee
          , Babbage.txvldt =
              Allegra.ValidityInterval {
                invalidBefore    = case lowerBound of
                                              TxValidityNoLowerBound   -> SNothing
                                              TxValidityLowerBound _ s -> SJust s,
                invalidHereafter = case upperBound of
                                              TxValidityNoUpperBound _ -> SNothing
                                              TxValidityUpperBound _ s -> SJust s
              }
          , Babbage.txUpdates = SNothing -- ignoring txUpdateProposal in CardanoAPITemp
          , Babbage.reqSignerHashes =
              case txExtraKeyWits of
                TxExtraKeyWitnessesNone   -> Set.empty
                TxExtraKeyWitnesses _ khs -> Set.fromList
                                                [ Shelley.coerceKeyRole kh
                                                | PaymentKeyHash kh <- khs ]
          , Babbage.mint =
              case txMintValue of
                TxMintNone        -> mempty
                TxMintValue _ v _ -> toMaryValue v
          , Babbage.scriptIntegrityHash =
              case txProtocolParams of
                BuildTxWith Nothing        -> SNothing
                BuildTxWith (Just pparams) ->
                  Alonzo.hashScriptIntegrity
                    (Set.map
                        (Alonzo.getLanguageView (toLedgerPParams era pparams))
                        languages
                    )
                    redeemers
                    datums
          , Babbage.adHash =
              maybeToStrictMaybe (Ledger.hashAuxiliaryData <$> txAuxData)
          , Babbage.txnetworkid = SNothing
          })
        scripts
        (TxBodyScriptData ScriptDataInBabbageEra datums redeemers)
        txAuxData
        txScriptValidity
        -- TODO alonzo: support the supplementary script data
  where
    era = ShelleyBasedEraBabbage

    witnesses :: [(ScriptWitnessIndex, AnyScriptWitness BabbageEra)]
    witnesses = collectTxBodyScriptWitnesses txbodycontent

    scripts :: [Ledger.Script StandardBabbage]
    scripts = Maybe.catMaybes
      [ toShelleyScript <$> scriptWitnessScript scriptwitness
      | (_, AnyScriptWitness scriptwitness) <- witnesses
      ]

    datums :: Alonzo.TxDats StandardBabbage
    datums =
      Alonzo.TxDats $
        Map.fromList
          [ (Alonzo.hashData d', d')
          | d <- scriptdata
          , let d' = toAlonzoData d
          ]

    scriptdata :: [ScriptData]
    scriptdata = List.nub $
      [ d | TxOut _ _ (TxOutDatumInTx _ d) _ <- txOuts ]
      ++ [ d | (_, AnyScriptWitness
                      (PlutusScriptWitness
                        _ _ _ (ScriptDatumForTxIn d) _ _)) <- witnesses
              ]

    redeemers :: Alonzo.Redeemers StandardBabbage
    redeemers =
      Alonzo.Redeemers $
        Map.fromList
          [ let ptr = toAlonzoRdmrPtr idx in (ptr, (toAlonzoData d, Maybe.fromMaybe (toAlonzoExUnits e) $ Map.lookup ptr exUnits))
          | (idx, AnyScriptWitness
                    (PlutusScriptWitness _ _ _ _ d e)) <- witnesses
          ]

    languages :: Set.Set Alonzo.Language
    languages =
      Set.fromList $ Maybe.catMaybes
        [ getScriptLanguage sw
        | (_, AnyScriptWitness sw) <- witnesses
        ]

    getScriptLanguage :: ScriptWitness witctx era -> Maybe Alonzo.Language
    getScriptLanguage (PlutusScriptWitness _ v _ _ _ _) =
      Just $ toAlonzoLanguage (AnyPlutusScriptVersion v)
    getScriptLanguage SimpleScriptWitness{} = Nothing

    txAuxData :: Maybe (Ledger.AuxiliaryData StandardBabbage)
    txAuxData
      | Map.null ms
      , null ss   = Nothing
      | otherwise = Just (toAlonzoAuxiliaryData ms ss)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'
        ss = case txAuxScripts of
               TxAuxScriptsNone   -> []
               TxAuxScripts _ ss' -> ss'


toShelleyWithdrawal :: [(StakeAddress, Lovelace, a)] -> Shelley.Wdrl StandardCrypto
toShelleyWithdrawal withdrawals =
    Shelley.Wdrl $
      Map.fromList
        [ (toShelleyStakeAddr stakeAddr, toShelleyLovelace value)
        | (stakeAddr, value, _) <- withdrawals ]

toShelleyTxOut :: forall era ledgerera.
                 (ShelleyLedgerEra era ~ ledgerera, IsShelleyBasedEra era)
               => ShelleyBasedEra era -> TxOut CtxTx era -> Ledger.TxOut ledgerera
toShelleyTxOut _ (TxOut _ (TxOutAdaOnly AdaOnlyInByronEra _) _ _) =
    case shelleyBasedEra :: ShelleyBasedEra era of {}

toShelleyTxOut _ (TxOut addr (TxOutAdaOnly AdaOnlyInShelleyEra value) _ _) =
    Shelley.TxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOut _ (TxOut addr (TxOutAdaOnly AdaOnlyInAllegraEra value) _ _) =
    Shelley.TxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOut _ (TxOut addr (TxOutValue MultiAssetInMaryEra value) _ _) =
    Shelley.TxOut (toShelleyAddr addr) (toMaryValue value)

-- | Copied from Cardano API to handle all TxOut contexts, we can
-- propose a PR to cardano-node to propagate this version and remove the code duplication
toShelleyTxOut _ (TxOut addr (TxOutValue MultiAssetInAlonzoEra value) txoutdata _) =
    Alonzo.TxOut (toShelleyAddr addr) (toMaryValue value)
                 (toAlonzoTxOutDataHash txoutdata)

toShelleyTxOut era (TxOut addr (TxOutValue MultiAssetInBabbageEra value) txoutdata refScript) =
    let cEra = shelleyBasedToCardanoEra era
     in Babbage.TxOut (toShelleyAddr addr) (toMaryValue value)
                      (toBabbageTxOutDatum txoutdata)
                      (refScriptToShelleyScript cEra refScript)


toAlonzoTxOutDataHash :: TxOutDatum CtxTx era
                      -> StrictMaybe (Alonzo.DataHash StandardCrypto)
toAlonzoTxOutDataHash TxOutDatumNone                         = SNothing
toAlonzoTxOutDataHash (TxOutDatumHash _ (ScriptDataHash dh)) = SJust dh
toAlonzoTxOutDataHash (TxOutDatumInTx _ d)                   = let ScriptDataHash dh = hashScriptData d in SJust dh
toAlonzoTxOutDataHash (TxOutDatumInline _ d)                 = let ScriptDataHash dh = hashScriptData d in SJust dh

toBabbageTxOutDatum
  :: Ledger.Crypto (ShelleyLedgerEra era) ~ StandardCrypto
  => TxOutDatum CtxTx era -> Babbage.Datum (ShelleyLedgerEra era)
toBabbageTxOutDatum  TxOutDatumNone = Babbage.NoDatum
toBabbageTxOutDatum (TxOutDatumHash _ (ScriptDataHash dh)) = Babbage.DatumHash dh
toBabbageTxOutDatum (TxOutDatumInTx _ d) = let ScriptDataHash dh = hashScriptData d in Babbage.DatumHash dh
toBabbageTxOutDatum (TxOutDatumInline _ sd) = scriptDataToInlineDatum sd

scriptDataToInlineDatum :: ScriptData -> Babbage.Datum ledgerera
scriptDataToInlineDatum = Babbage.Datum . Alonzo.dataToBinaryData . toAlonzoData

toAlonzoLanguage :: AnyPlutusScriptVersion -> Alonzo.Language
toAlonzoLanguage (AnyPlutusScriptVersion PlutusScriptV1) = Alonzo.PlutusV1
toAlonzoLanguage (AnyPlutusScriptVersion PlutusScriptV2) = Alonzo.PlutusV2

-- | In the Alonzo and later eras the auxiliary data consists of the tx metadata
-- and the axiliary scripts, and the axiliary script data.
--
toAlonzoAuxiliaryData :: forall era ledgerera.
                         ShelleyLedgerEra era ~ ledgerera
                      => Ledger.AuxiliaryData ledgerera ~ Alonzo.AuxiliaryData ledgerera
                      => Ledger.Script ledgerera ~ Alonzo.Script ledgerera
                      => Ledger.Era ledgerera
                      => Map.Map Word64 TxMetadataValue
                      -> [ScriptInEra era]
                      -> Ledger.AuxiliaryData ledgerera
toAlonzoAuxiliaryData m ss =
    Alonzo.AuxiliaryData
      (toShelleyMetadata m)
      (Seq.fromList (map toShelleyScript ss))
