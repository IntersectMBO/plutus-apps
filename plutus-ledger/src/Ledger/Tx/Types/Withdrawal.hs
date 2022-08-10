{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}

module Ledger.Tx.Types.Withdrawal where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.DCert.Orphans ()
import Ledger.Tx.Orphans ()
import Plutus.V1.Ledger.Api (Credential)
import Plutus.V1.Ledger.Scripts
import Prettyprinter (Pretty (pretty), viaShow)

-- | Stake withdrawal, if applicable the script should be included in txScripts.
data Withdrawal = Withdrawal
  { withdrawalCredential :: Credential         -- ^ staking credential
  , withdrawalAmount     :: Integer                   -- ^ amount of withdrawal in Lovelace, must withdraw all eligible amount
  , withdrawalRedeemer   :: Maybe Redeemer            -- ^ redeemer for script credential
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise, NFData)

instance Pretty Withdrawal where
    pretty = viaShow
