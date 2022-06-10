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

deriving instance Generic ChainPoint

instance ToJSON ChainPoint

instance ToJSON BlockNo

deriving instance Generic BlockHeader

instance ToJSON BlockHeader

instance ToJSON a => ToJSON (ChainSyncEvent a)
