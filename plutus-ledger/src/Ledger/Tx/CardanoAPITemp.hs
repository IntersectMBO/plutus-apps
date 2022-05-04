{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE NamedFieldPuns           #-}
-- Code temporarily copied over from cardano-api,
-- until https://github.com/input-output-hk/cardano-node/pull/2936 or something similar gets merged.
module Ledger.Tx.CardanoAPITemp (makeTransactionBody') where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Sequence.Strict qualified as Seq
import Data.Set qualified as Set

import Cardano.Api
import Cardano.Api.Shelley hiding (toShelleyTxOut)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Ouroboros.Consensus.Shelley.Eras (StandardAlonzo)

import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Era qualified as Ledger

import Cardano.Ledger.Alonzo.Data qualified as Alonzo
import Cardano.Ledger.Alonzo.Language qualified as Alonzo
import Cardano.Ledger.Alonzo.PParams qualified as Alonzo
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Alonzo.Tx qualified as Alonzo
import Cardano.Ledger.Alonzo.TxBody qualified as Alonzo
import Cardano.Ledger.Alonzo.TxWitness qualified as Alonzo
import Cardano.Ledger.Babbage.TxBody qualified as Babbage

import Cardano.Ledger.ShelleyMA.TxBody qualified as Allegra

import Cardano.Ledger.Keys qualified as Shelley
import Cardano.Ledger.Shelley.Tx qualified as Shelley
import Cardano.Ledger.Shelley.TxBody qualified as Shelley

makeTransactionBody'
    :: Map.Map Alonzo.RdmrPtr Alonzo.ExUnits
    -> TxBodyContent BuildTx AlonzoEra
    -> Either TxBodyError (TxBody AlonzoEra)
makeTransactionBody'
    exUnits
    txbodycontent@TxBodyContent {
        txIns,
        txInsCollateral,
        txOuts,
        txFee,
        txValidityRange = (lowerBound, upperBound),
        txExtraKeyWits,
        txWithdrawals,
        txCertificates,
        txMintValue,
        txScriptValidity,
        txProtocolParams
    } =
    return $
      ShelleyTxBody ShelleyBasedEraAlonzo
        (Alonzo.TxBody
          (Set.fromList (map (toShelleyTxIn . fst) txIns))
          (case txInsCollateral of
             TxInsCollateralNone     -> Set.empty
             TxInsCollateral _ txins -> Set.fromList (map toShelleyTxIn txins))
          (Seq.fromList (map (toShelleyTxOut ShelleyBasedEraAlonzo) txOuts))
          (case txCertificates of
             TxCertificatesNone    -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList (map toShelleyCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
             TxWithdrawals _ ws -> toShelleyWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toShelleyLovelace fee)
          (Allegra.ValidityInterval {
             invalidBefore    = case lowerBound of
                                          TxValidityNoLowerBound   -> SNothing
                                          TxValidityLowerBound _ s -> SJust s,
             invalidHereafter = case upperBound of
                                          TxValidityNoUpperBound _ -> SNothing
                                          TxValidityUpperBound _ s -> SJust s
           })
          SNothing -- ignoring txUpdatePropsal in CardanoAPITemp
          (case txExtraKeyWits of
             TxExtraKeyWitnessesNone   -> Set.empty
             TxExtraKeyWitnesses _ khs -> Set.fromList
                                            [ Shelley.coerceKeyRole kh
                                            | PaymentKeyHash kh <- khs ])
          (case txMintValue of
             TxMintNone        -> mempty
             TxMintValue _ v _ -> toMaryValue v)
          (case txProtocolParams of
             BuildTxWith Nothing        -> SNothing
             BuildTxWith (Just pparams) -> do
               let x = toLedgerPParams ShelleyBasedEraAlonzo pparams
               Alonzo.hashScriptIntegrity
                 x
                 languages
                 redeemers
                 datums)
          SNothing -- ignoring txMetadata and txAuxScripts in CardanoAPITemp
          SNothing) -- TODO alonzo: support optional network id in TxBodyContent
        scripts
        (TxBodyScriptData ScriptDataInAlonzoEra datums redeemers)
        Nothing -- ignoring txMetadata and txAuxScripts in CardanoAPITemp
        txScriptValidity
        -- TODO alonzo: support the supplementary script data
  where
    witnesses :: [(ScriptWitnessIndex, AnyScriptWitness AlonzoEra)]
    witnesses = collectTxBodyScriptWitnesses txbodycontent

    scripts :: [Ledger.Script StandardAlonzo]
    scripts =
      [ toShelleyScript (scriptWitnessScript scriptwitness)
      | (_, AnyScriptWitness scriptwitness) <- witnesses
      ]

    datums :: Alonzo.TxDats StandardAlonzo
    datums =
      Alonzo.TxDats $
        Map.fromList
          [ (Alonzo.hashData d', d')
          | d <- scriptdata
          , let d' = toAlonzoData d
          ]

    scriptdata :: [ScriptData]
    scriptdata = List.nub $
      [ d | TxOut _ _ (TxOutDatumInTx ScriptDataInAlonzoEra d) _ <- txOuts ]
      ++ [ d | (_, AnyScriptWitness
                      (PlutusScriptWitness
                        _ _ _ (ScriptDatumForTxIn d) _ _)) <- witnesses
              ]

    redeemers :: Alonzo.Redeemers StandardAlonzo
    redeemers =
      Alonzo.Redeemers $
        Map.fromList
          [ let ptr = toAlonzoRdmrPtr idx in (ptr, (toAlonzoData d, Maybe.fromMaybe (toAlonzoExUnits e) $ Map.lookup ptr exUnits))
          | (idx, AnyScriptWitness
                    (PlutusScriptWitness _ _ _ _ d e)) <- witnesses
          ]

    languages :: Set.Set Alonzo.Language
    languages =
      Set.fromList
        [ toAlonzoLanguage (AnyPlutusScriptVersion v)
        | (_, AnyScriptWitness (PlutusScriptWitness _ v _ _ _ _)) <- witnesses
        ]

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
