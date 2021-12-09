{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Cardano.Crypto.KES.NeverUsed
  ( NeverKES
  , VerKeyKES (..)
  , SignKeyKES (..)
  , SigKES (..)
  )
where

import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

import Cardano.Crypto.KES.Class


-- | KES never used
--
-- The type of keys and signatures is isomorphic to unit, but when actually
-- trying to sign or verify something a runtime exception will be thrown.
data NeverKES

instance KESAlgorithm NeverKES where
  type SeedSizeKES NeverKES = 0

  data VerKeyKES  NeverKES = NeverUsedVerKeyKES
      deriving (Show, Eq, Generic, NoThunks)

  data SignKeyKES NeverKES = NeverUsedSignKeyKES
      deriving (Show, Eq, Generic, NoThunks)

  data SigKES     NeverKES = NeverUsedSigKES
      deriving (Show, Eq, Generic, NoThunks)

  algorithmNameKES _ = "never"

  deriveVerKeyKES _ = NeverUsedVerKeyKES

  signKES   = error "KES not available"
  verifyKES = error "KES not available"
  updateKES = error "KES not available"

  totalPeriodsKES _ = 0

  genKeyKES       _ = NeverUsedSignKeyKES

  sizeVerKeyKES  _ = 0
  sizeSignKeyKES _ = 0
  sizeSigKES     _ = 0

  rawSerialiseVerKeyKES  _ = mempty
  rawSerialiseSignKeyKES _ = mempty
  rawSerialiseSigKES     _ = mempty

  rawDeserialiseVerKeyKES  _ = Just NeverUsedVerKeyKES
  rawDeserialiseSignKeyKES _ = Just NeverUsedSignKeyKES
  rawDeserialiseSigKES     _ = Just NeverUsedSigKES

