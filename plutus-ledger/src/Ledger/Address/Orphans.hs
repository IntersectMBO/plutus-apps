{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ledger.Address.Orphans where

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, ToJSON)

import Ledger.Credential.Orphans ()
import Plutus.V1.Ledger.Address

deriving anyclass instance ToJSON Address
deriving anyclass instance FromJSON Address
deriving anyclass instance Serialise Address
