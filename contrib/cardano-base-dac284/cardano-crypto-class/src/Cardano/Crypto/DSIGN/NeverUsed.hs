{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Cardano.Crypto.DSIGN.NeverUsed
  ( NeverDSIGN
  , VerKeyDSIGN (..)
  , SignKeyDSIGN (..)
  , SigDSIGN (..)
  )
where

import GHC.Generics (Generic)

import NoThunks.Class (NoThunks)

import Cardano.Crypto.DSIGN.Class


-- | DSIGN never used
--
-- The type of keys and signatures is isomorphic to unit, but when actually
-- trying to sign or verify something a runtime exception will be thrown.
data NeverDSIGN

instance DSIGNAlgorithm NeverDSIGN where
  type SeedSizeDSIGN NeverDSIGN = 0
  type SizeVerKeyDSIGN  NeverDSIGN = 0
  type SizeSignKeyDSIGN NeverDSIGN = 0
  type SizeSigDSIGN     NeverDSIGN = 0

  data VerKeyDSIGN  NeverDSIGN = NeverUsedVerKeyDSIGN
     deriving (Show, Eq, Generic, NoThunks)

  data SignKeyDSIGN NeverDSIGN = NeverUsedSignKeyDSIGN
     deriving (Show, Eq, Generic, NoThunks)

  data SigDSIGN     NeverDSIGN = NeverUsedSigDSIGN
     deriving (Show, Eq, Generic, NoThunks)

  algorithmNameDSIGN _ = "never"

  deriveVerKeyDSIGN _ = NeverUsedVerKeyDSIGN

  signDSIGN   = error "DSIGN not available"
  verifyDSIGN = error "DSIGN not available"

  genKeyDSIGN       _ = NeverUsedSignKeyDSIGN

  rawSerialiseVerKeyDSIGN  _ = mempty
  rawSerialiseSignKeyDSIGN _ = mempty
  rawSerialiseSigDSIGN     _ = mempty

  rawDeserialiseVerKeyDSIGN  _ = Just NeverUsedVerKeyDSIGN
  rawDeserialiseSignKeyDSIGN _ = Just NeverUsedSignKeyDSIGN
  rawDeserialiseSigDSIGN     _ = Just NeverUsedSigDSIGN

