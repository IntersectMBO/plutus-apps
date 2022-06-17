{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Ledger.Tx.Types.TxInput where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.DCert.Orphans ()
import Ledger.Tx.Orphans ()
import Plutus.V1.Ledger.Api (TxOutRef)
import Plutus.V1.Ledger.Scripts (DatumHash, Redeemer, ValidatorHash)
import Prettyprinter (Pretty (pretty), hang, vsep, (<+>))

-- | The type of a transaction input. Contains redeemer if consumes a script.
data TxInputType =
      TxConsumeScriptAddress !Redeemer !ValidatorHash !DatumHash -- ^ A transaction input that consumes a script address with the given validator, redeemer, and datum.
    | TxConsumePublicKeyAddress -- ^ A transaction input that consumes a public key address.
    | TxConsumeSimpleScriptAddress -- ^ Consume a simple script
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise, NFData)

-- | A transaction input, consisting of a transaction output reference and an input type.
-- Differs with TxIn by: TxIn *maybe* contains *full* data witnesses, TxInput always contains redeemer witness, but datum/validator hashes.
data TxInput = TxInput {
    txInputRef  :: !TxOutRef,
    txInputType :: !TxInputType
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise, NFData)

-- same as TxIn
instance Pretty TxInput where
    pretty TxInput{txInputRef,txInputType} =
        let rest =
                case txInputType of
                    TxConsumeScriptAddress redeemer _ _ ->
                        pretty redeemer
                    _ -> mempty
        in hang 2 $ vsep ["-" <+> pretty txInputRef, rest]
