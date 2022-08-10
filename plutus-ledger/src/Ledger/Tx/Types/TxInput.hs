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

