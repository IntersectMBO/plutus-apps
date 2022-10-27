{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TypeFamilies       #-}
-- | This module provides several type aliases and utility functions to deal with the current Cardano era.
module Marconi.CardanoAPI
       (
       CurrentEra,
       pattern AsCurrentEra,
       pattern CurrentEra,
       C.TxIn(C.TxIn),
       TxOut,
       TxOutRef,
       Ledger.Crypto,
       Ledger.StandardCrypto,
       txScriptValidityToScriptValidity,
       txOutRef
       ) where

import Cardano.Api qualified as C

import Cardano.Ledger.Crypto qualified as Ledger (StandardCrypto)
import Cardano.Ledger.Era qualified as Ledger

-- * Alias to the current Cardano era
--
-- | An alias for the current era, to ease the transition from one era to the next one
type CurrentEra = C.BabbageEra

pattern CurrentEra :: C.CardanoEra CurrentEra
pattern CurrentEra = C.BabbageEra

pattern AsCurrentEra :: C.AsType CurrentEra
pattern AsCurrentEra = C.AsBabbageEra

-- | A Cardano TxOut of the current Era
type TxOut = C.TxOut C.CtxTx CurrentEra

-- | A reference to a transaction output. This is a
-- pair of a transaction reference, and an index indicating which of the outputs
-- of that transaction we are referring to.
--
type TxOutRef = C.TxIn

txOutRef :: C.TxId -> C.TxIx -> C.TxIn
txOutRef = C.TxIn

-- * Duplicated from cardano-api (not exposed in cardano-api)

-- TODO: Open a ticket to expose it in cardano-api
txScriptValidityToScriptValidity :: C.TxScriptValidity era -> C.ScriptValidity
txScriptValidityToScriptValidity C.TxScriptValidityNone                = C.ScriptValid
txScriptValidityToScriptValidity (C.TxScriptValidity _ scriptValidity) = scriptValidity

