{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ledger.Crypto.Orphans where

import Ledger.Builtins.Orphans ()

import Codec.Serialise (Serialise)
import Control.Newtype.Generics (Newtype)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)

import Plutus.V1.Ledger.Crypto

deriving anyclass instance ToJSON PubKeyHash
deriving anyclass instance FromJSON PubKeyHash
deriving anyclass instance FromJSONKey PubKeyHash
deriving anyclass instance ToJSONKey PubKeyHash
deriving anyclass instance Newtype PubKeyHash
deriving newtype instance Serialise PubKeyHash
deriving newtype instance Hashable PubKeyHash
