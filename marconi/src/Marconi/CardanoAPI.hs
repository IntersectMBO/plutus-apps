{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TypeFamilies       #-}
-- | This module provides several type aliases and utility functions to deal with the current Cardano era.
module Marconi.CardanoAPI
       (
       CurrentEra,
       pattern AsCurrentEra,
       currentEra,
       C.TxIn(C.TxIn),
       TxOut,
       TxOutRef,
       Address,
       Datum,
       DatumHash,
       Ledger.Crypto,
       Ledger.StandardCrypto,
       txScriptValidityToScriptValidity,
       txOutRef,
       scriptDataFromCardanoTxBody) where

import Cardano.Api qualified as C

import Cardano.Api.Byron qualified as Byron
import "cardano-api" Cardano.Api.Shelley qualified as Shelley
import Cardano.Ledger.Alonzo.TxWitness qualified as Alonzo
import Cardano.Ledger.Crypto qualified as Ledger (StandardCrypto)
import Cardano.Ledger.Era qualified as Ledger
import Data.Map (Map)
import Data.Map qualified as Map

-- * Alias to the current Cardano era
--
-- | An alias for the current era, to ease the transition from one era to the next one
type CurrentEra = C.BabbageEra

currentEra :: C.CardanoEra CurrentEra
currentEra = C.BabbageEra

pattern AsCurrentEra :: C.AsType CurrentEra
pattern AsCurrentEra = C.AsBabbageEra

-- | A Cardano TxOut of the current Era
type TxOut = C.TxOut C.CtxTx CurrentEra

-- | A Cardano Address of the current Era
type Address = C.AddressInEra CurrentEra

-- | A reference to a transaction output. This is a
-- pair of a transaction reference, and an index indicating which of the outputs
-- of that transaction we are referring to.
--
type TxOutRef = C.TxIn

txOutRef :: C.TxId -> C.TxIx -> C.TxIn
txOutRef = C.TxIn

type Datum = C.ScriptData
type DatumHash = C.Hash C.ScriptData

scriptDataFromCardanoTxBody :: C.TxBody era -> Map DatumHash Datum
scriptDataFromCardanoTxBody Byron.ByronTxBody {} = mempty
scriptDataFromCardanoTxBody (Shelley.ShelleyTxBody _ _ _ C.TxBodyNoScriptData _ _) = mempty
scriptDataFromCardanoTxBody
  (Shelley.ShelleyTxBody _ _ _ (C.TxBodyScriptData _ dats _) _ _) =
      extractData dats


-- * Duplicated from cardano-api (not exposed in cardano-api)

extractData :: Alonzo.TxDats era -> Map DatumHash Datum
extractData (Alonzo.TxDats' xs) =
  Map.fromList
  . fmap ((\x -> (C.hashScriptData x, x)) . Shelley.fromAlonzoData)
  . Map.elems $ xs

-- TODO: Open a ticket to expose it in cardano-api
txScriptValidityToScriptValidity :: C.TxScriptValidity era -> C.ScriptValidity
txScriptValidityToScriptValidity C.TxScriptValidityNone                = C.ScriptValid
txScriptValidityToScriptValidity (C.TxScriptValidity _ scriptValidity) = scriptValidity

