{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ledger.Credential.Orphans where

import Ledger.Crypto.Orphans ()
import Ledger.Scripts.Orphans ()

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)

import Plutus.V1.Ledger.Credential

deriving anyclass instance ToJSON Credential
deriving anyclass instance FromJSON Credential
deriving anyclass instance Hashable Credential
deriving anyclass instance Serialise Credential

deriving anyclass instance ToJSON StakingCredential
deriving anyclass instance FromJSON StakingCredential
deriving anyclass instance Hashable StakingCredential
deriving anyclass instance Serialise StakingCredential
