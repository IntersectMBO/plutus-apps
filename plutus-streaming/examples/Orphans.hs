{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans where

import Cardano.Api (BlockHeader (BlockHeader), BlockNo, ChainPoint (ChainPoint, ChainPointAtGenesis), ToJSON)
import GHC.Generics (Generic)
import Plutus.Streaming (ChainSyncEvent)

deriving instance Generic ChainPoint

instance ToJSON ChainPoint

instance ToJSON BlockNo

deriving instance Generic BlockHeader

instance ToJSON BlockHeader

instance ToJSON a => ToJSON (ChainSyncEvent a)
