{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ledger.DCert.Orphans where

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, ToJSON)

import Ledger.Credential.Orphans ()
import Ledger.Crypto.Orphans ()
import PlutusLedgerApi.V1.DCert (DCert)

deriving anyclass instance ToJSON DCert
deriving anyclass instance FromJSON DCert
deriving anyclass instance Serialise DCert

