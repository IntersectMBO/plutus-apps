{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ledger.Credential.Orphans where

import Ledger.Crypto.Orphans ()
import Ledger.Scripts.Orphans ()

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)

import Plutus.V1.Ledger.Credential

deriving anyclass instance ToJSON Credential
deriving anyclass instance FromJSON Credential
deriving anyclass instance Hashable Credential
deriving anyclass instance Serialise Credential
deriving anyclass instance ToSchema Credential

deriving anyclass instance ToJSON StakingCredential
deriving anyclass instance FromJSON StakingCredential
deriving anyclass instance Hashable StakingCredential
deriving anyclass instance Serialise StakingCredential
deriving anyclass instance ToSchema StakingCredential
