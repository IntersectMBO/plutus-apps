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
  , fromCardanoTotalCollateral
  , fromCardanoReturnCollateral
  , toCardanoTxInsCollateral
  , toCardanoTotalCollateral
  , toCardanoReturnCollateral
  , toCardanoDatumWitness
  , toCardanoTxInReferenceWitnessHeader
  , toCardanoTxInScriptWitnessHeader
  , toCardanoMintWitness
  , ToCardanoError(..)
  , FromCardanoError(..)
  , getRequiredSigners
  -- * Conversion from Plutus types
  , fromPlutusIndex
  , fromPlutusTxOut
  , fromPlutusTxOutRef
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (..))
import Cardano.Ledger.Babbage qualified as Babbage
import Cardano.Ledger.Babbage.TxBody (TxBody (TxBody, reqSignerHashes))
import Cardano.Ledger.BaseTypes (mkTxIxPartial)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (bitraverse)
import Data.Map qualified as Map
import Ledger.Address qualified as P
import Ledger.Index.Internal qualified as P
import Ledger.Scripts qualified as P
import Ledger.Tx.CardanoAPI.Internal
import Ledger.Tx.Internal qualified as P
import Plutus.V1.Ledger.Api qualified as PV1


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
            Right (P.Versioned ref _) -> flip C.PReferenceScript Nothing <$> toCardanoTxIn ref
          )
    P.PlutusV2 ->
      C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2
          <$> (case scriptOrRef of
            Left (P.Versioned script _) -> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) script)
            Right (P.Versioned ref _) -> flip C.PReferenceScript Nothing <$> toCardanoTxIn ref
          )
  ) <*> pure datum
    <*> pure (C.fromPlutusData $ PV1.toData redeemer)
    <*> pure zeroExecutionUnits
  where
    lang = either P.version P.version scriptOrRef

fromCardanoTxInsCollateral :: C.TxInsCollateral era -> [P.TxIn]
fromCardanoTxInsCollateral C.TxInsCollateralNone       = []
fromCardanoTxInsCollateral (C.TxInsCollateral _ txIns) = map (P.pubKeyTxIn . fromCardanoTxIn) txIns

toCardanoTxInsCollateral :: [P.TxInput] -> Either ToCardanoError (C.TxInsCollateral C.BabbageEra)
toCardanoTxInsCollateral [] = pure C.TxInsCollateralNone
toCardanoTxInsCollateral inputs = fmap (C.TxInsCollateral C.CollateralInBabbageEra) (traverse (toCardanoTxIn . P.txInputRef) inputs)

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

fromCardanoTotalCollateral :: C.TxTotalCollateral C.BabbageEra -> Maybe C.Lovelace
fromCardanoTotalCollateral C.TxTotalCollateralNone    = Nothing
fromCardanoTotalCollateral (C.TxTotalCollateral _ lv) = Just lv

toCardanoTotalCollateral :: Maybe C.Lovelace -> C.TxTotalCollateral C.BabbageEra
toCardanoTotalCollateral totalCollateral =
  case C.totalAndReturnCollateralSupportedInEra C.BabbageEra of
    Just txTotalAndReturnCollateralInBabbageEra ->
      maybe C.TxTotalCollateralNone (C.TxTotalCollateral txTotalAndReturnCollateralInBabbageEra) totalCollateral
    Nothing -> C.TxTotalCollateralNone

fromCardanoReturnCollateral :: C.TxReturnCollateral C.CtxTx C.BabbageEra -> Maybe P.TxOut
fromCardanoReturnCollateral C.TxReturnCollateralNone       = Nothing
fromCardanoReturnCollateral (C.TxReturnCollateral _ txOut) = Just $ P.TxOut txOut

toCardanoReturnCollateral :: Maybe P.TxOut -> C.TxReturnCollateral C.CtxTx C.BabbageEra
toCardanoReturnCollateral returnCollateral =
  case C.totalAndReturnCollateralSupportedInEra C.BabbageEra of
    Just txTotalAndReturnCollateralInBabbageEra ->
      maybe C.TxReturnCollateralNone (C.TxReturnCollateral txTotalAndReturnCollateralInBabbageEra . P.getTxOut) returnCollateral
    Nothing -> C.TxReturnCollateralNone

getRequiredSigners :: C.Tx C.BabbageEra -> [P.PaymentPubKeyHash]
getRequiredSigners (C.ShelleyTx _ (ValidatedTx TxBody { reqSignerHashes = rsq } _ _ _)) =
  foldMap (pure . P.PaymentPubKeyHash . P.toPlutusPubKeyHash . C.PaymentKeyHash . C.Ledger.coerceKeyRole) rsq

fromPlutusIndex :: P.UtxoIndex -> Either (Either P.ValidationErrorInPhase ToCardanoError) (C.Ledger.UTxO (Babbage.BabbageEra StandardCrypto))
fromPlutusIndex (P.UtxoIndex m) =
  first Right $ C.Ledger.UTxO . Map.fromList <$> traverse (bitraverse fromPlutusTxOutRef (pure . fromPlutusTxOut)) (Map.toList m)

fromPlutusTxOutRef :: P.TxOutRef -> Either ToCardanoError (C.Ledger.TxIn StandardCrypto)
fromPlutusTxOutRef (P.TxOutRef txId i) = C.Ledger.TxIn <$> fromPlutusTxId txId <*> pure (mkTxIxPartial i)

fromPlutusTxId :: PV1.TxId -> Either ToCardanoError (C.Ledger.TxId StandardCrypto)
fromPlutusTxId = fmap C.toShelleyTxId . toCardanoTxId

fromPlutusTxOut :: P.TxOut -> Babbage.TxOut (Babbage.BabbageEra StandardCrypto)
fromPlutusTxOut = C.toShelleyTxOut C.ShelleyBasedEraBabbage . C.toCtxUTxOTxOut . P.getTxOut
