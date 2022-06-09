{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans where

import Cardano.Api (BlockHeader (BlockHeader), BlockNo, ChainPoint (ChainPoint, ChainPointAtGenesis),
                    HasTypeProxy (proxyToAsType), Hash, SerialiseAsRawBytes (deserialiseFromRawBytes), ToJSON)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as C8
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Plutus.Streaming (ChainSyncEvent)

-- https://github.com/input-output-hk/cardano-node/pull/3608
instance IsString (Hash BlockHeader) where
  fromString = either error id . deserialiseFromRawBytesBase16 . C8.pack
    where
      deserialiseFromRawBytesBase16 str =
        case Base16.decode str of
          Right raw -> case deserialiseFromRawBytes ttoken raw of
            Just x  -> Right x
            Nothing -> Left ("cannot deserialise " ++ show str)
          Left msg -> Left ("invalid hex " ++ show str ++ ", " ++ msg)
        where
          ttoken = proxyToAsType (Proxy :: Proxy a)

deriving instance Generic ChainPoint

instance ToJSON ChainPoint

instance ToJSON BlockNo

deriving instance Generic BlockHeader

instance ToJSON BlockHeader

instance ToJSON a => ToJSON (ChainSyncEvent a)
