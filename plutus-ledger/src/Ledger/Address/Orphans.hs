{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ledger.Address.Orphans where

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, ToJSON)

import Ledger.Credential.Orphans ()
import Ledger.Scripts.Orphans ()
import PlutusLedgerApi.V1.Address

deriving anyclass instance ToJSON Address
deriving anyclass instance FromJSON Address
deriving anyclass instance Serialise Address
