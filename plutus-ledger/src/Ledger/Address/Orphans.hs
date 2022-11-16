{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ledger.Address.Orphans where

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, ToJSON)

import Data.OpenApi (ToSchema)
import Ledger.Credential.Orphans ()
import Ledger.Scripts.Orphans ()
import Plutus.V1.Ledger.Address

deriving anyclass instance ToJSON Address
deriving anyclass instance FromJSON Address
deriving anyclass instance Serialise Address
deriving anyclass instance ToSchema Address
