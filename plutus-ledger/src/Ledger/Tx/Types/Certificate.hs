{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}

module Ledger.Tx.Types.Certificate where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.DCert.Orphans ()
import Ledger.Tx.Orphans ()
import Plutus.V1.Ledger.Api (DCert)
import Plutus.V1.Ledger.Scripts (Redeemer)
import Prettyprinter (Pretty (pretty), viaShow)

data Certificate = Certificate
  { certificateDcert    :: DCert
  , certificateRedeemer :: Maybe Redeemer           -- ^ redeemer for script credential
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise, NFData)

instance Pretty Certificate where
    pretty = viaShow
