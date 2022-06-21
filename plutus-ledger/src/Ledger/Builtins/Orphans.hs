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
import Control.Lens
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson qualified as JSON
import Data.OpenApi qualified as OpenApi
import Data.Proxy (Proxy (Proxy))
import GHC.Exts (IsList (fromList))
import GHC.Generics (Generic)
import PlutusCore.Data
import PlutusTx qualified as PlutusTx
import PlutusTx.Builtins.Internal (BuiltinData (..))

instance OpenApi.ToSchema PlutusTx.BuiltinByteString where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "Bytes") mempty

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
deriving instance OpenApi.ToSchema BuiltinData

deriving via (JSON.JSONViaSerialise Data) instance ToJSON Data
deriving via (JSON.JSONViaSerialise Data) instance FromJSON Data

instance OpenApi.ToSchema Data where
  declareNamedSchema _ = do
    integerSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy Integer)
    constrArgsSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy (Integer, [Data]))
    mapArgsSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy [(Data, Data)])
    listArgsSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy [Data])
    bytestringSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy String)
    return $ OpenApi.NamedSchema (Just "Data") $ mempty
      & OpenApi.type_ ?~ OpenApi.OpenApiObject
      & OpenApi.properties .~
          fromList
          [ ("Constr", constrArgsSchema)
          , ("Map", mapArgsSchema)
          , ("List", listArgsSchema)
          , ("I", integerSchema)
          , ("B", bytestringSchema)
          ]
