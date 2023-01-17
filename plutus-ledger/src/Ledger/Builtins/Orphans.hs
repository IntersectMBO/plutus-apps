{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ledger.Builtins.Orphans where

import PlutusTx.Prelude qualified as PlutusTx

import Data.Aeson.Extras qualified as JSON

import Codec.Serialise (Serialise (decode, encode))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson qualified as JSON
import GHC.Generics (Generic)
import PlutusCore.Data
import PlutusTx qualified as PlutusTx
import PlutusTx.Builtins.Internal (BuiltinData (..))

instance ToJSON PlutusTx.BuiltinByteString where
    toJSON = JSON.String . JSON.encodeByteString . PlutusTx.fromBuiltin

instance FromJSON PlutusTx.BuiltinByteString where
    parseJSON v = PlutusTx.toBuiltin <$> JSON.decodeByteString v

instance ToJSON PlutusTx.BuiltinData where
  toJSON = toJSON . PlutusTx.builtinDataToData

instance FromJSON PlutusTx.BuiltinData where
  parseJSON v = parseJSON v >>= pure . PlutusTx.dataToBuiltinData

instance Serialise PlutusTx.BuiltinData where
  encode = encode . PlutusTx.builtinDataToData
  decode = PlutusTx.dataToBuiltinData <$> decode

deriving stock instance Generic BuiltinData

deriving via (JSON.JSONViaSerialise Data) instance ToJSON Data
deriving via (JSON.JSONViaSerialise Data) instance FromJSON Data
