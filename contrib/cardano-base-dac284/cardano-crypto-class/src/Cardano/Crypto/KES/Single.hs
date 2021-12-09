{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A standard signature scheme is a forward-secure signature scheme with a
-- single time period.
--
-- This is the base case in the naive recursive implementation of the sum
-- composition from section 3 of the \"MMM\" paper:
--
-- /Composition and Efficiency Tradeoffs for Forward-Secure Digital Signatures/
-- By Tal Malkin, Daniele Micciancio and Sara Miner
-- <https://eprint.iacr.org/2001/034>
--
-- Specfically it states:
--
-- > In order to unify the presentation, we regard standard signature schemes
-- > as forward-seure signature schemes with one time period, namely T = 1.
--
-- So this module simply provides a wrapper 'SingleKES' that turns any
-- 'DSIGNAlgorithm' into an instance of 'KESAlgorithm' with a single period.
--
-- See "Cardano.Crypto.KES.Sum" for the composition case.
--
module Cardano.Crypto.KES.Single (
    SingleKES
  , VerKeyKES (..)
  , SignKeyKES (..)
  , SigKES (..)
  ) where

import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

import Control.Exception (assert)

import Cardano.Binary (FromCBOR (..), ToCBOR (..))

import Cardano.Crypto.Hash.Class
import Cardano.Crypto.DSIGN.Class
import qualified Cardano.Crypto.DSIGN as DSIGN
import Cardano.Crypto.KES.Class
import Control.DeepSeq (NFData)


-- | A standard signature scheme is a forward-secure signature scheme with a
-- single time period.
--
data SingleKES d

deriving instance NFData (VerKeyDSIGN d) => NFData (VerKeyKES (SingleKES d))
deriving instance NFData (SignKeyDSIGN d) => NFData (SignKeyKES (SingleKES d))
deriving instance NFData (SigDSIGN d) => NFData (SigKES (SingleKES d))

instance (DSIGNAlgorithm d, Typeable d) => KESAlgorithm (SingleKES d) where
    type SeedSizeKES (SingleKES d) = SeedSizeDSIGN d

    --
    -- Key and signature types
    --

    newtype VerKeyKES (SingleKES d) = VerKeySingleKES (VerKeyDSIGN d)
        deriving Generic

    newtype SignKeyKES (SingleKES d) = SignKeySingleKES (SignKeyDSIGN d)
        deriving Generic

    newtype SigKES (SingleKES d) = SigSingleKES (SigDSIGN d)
        deriving Generic


    --
    -- Metadata and basic key operations
    --

    algorithmNameKES _ = algorithmNameDSIGN (Proxy :: Proxy d) ++ "_kes_2^0"

    deriveVerKeyKES (SignKeySingleKES sk) =
        VerKeySingleKES (deriveVerKeyDSIGN sk)

    hashVerKeyKES (VerKeySingleKES vk) =
        castHash (hashVerKeyDSIGN vk)


    --
    -- Core algorithm operations
    --

    type ContextKES (SingleKES d) = DSIGN.ContextDSIGN d
    type Signable   (SingleKES d) = DSIGN.Signable     d

    signKES ctxt t a (SignKeySingleKES sk) =
        assert (t == 0) $
        SigSingleKES (signDSIGN ctxt a sk)

    verifyKES ctxt (VerKeySingleKES vk) t a (SigSingleKES sig) =
        assert (t == 0) $
        verifyDSIGN ctxt vk a sig

    updateKES _ctx (SignKeySingleKES _sk) _to = Nothing

    totalPeriodsKES  _ = 1

    --
    -- Key generation
    --

    seedSizeKES _ = seedSizeDSIGN (Proxy :: Proxy d)
    genKeyKES seed = SignKeySingleKES (genKeyDSIGN seed)


    --
    -- raw serialise/deserialise
    --

    sizeVerKeyKES  _ = sizeVerKeyDSIGN  (Proxy :: Proxy d)
    sizeSignKeyKES _ = sizeSignKeyDSIGN (Proxy :: Proxy d)
    sizeSigKES     _ = sizeSigDSIGN     (Proxy :: Proxy d)

    rawSerialiseVerKeyKES  (VerKeySingleKES  vk) = rawSerialiseVerKeyDSIGN vk
    rawSerialiseSignKeyKES (SignKeySingleKES sk) = rawSerialiseSignKeyDSIGN sk
    rawSerialiseSigKES     (SigSingleKES    sig) = rawSerialiseSigDSIGN sig

    rawDeserialiseVerKeyKES  = fmap VerKeySingleKES  . rawDeserialiseVerKeyDSIGN
    rawDeserialiseSignKeyKES = fmap SignKeySingleKES . rawDeserialiseSignKeyDSIGN
    rawDeserialiseSigKES     = fmap SigSingleKES     . rawDeserialiseSigDSIGN


--
-- VerKey instances
--

deriving instance DSIGNAlgorithm d => Show (VerKeyKES (SingleKES d))
deriving instance DSIGNAlgorithm d => Eq   (VerKeyKES (SingleKES d))

instance DSIGNAlgorithm d => NoThunks (SignKeyKES (SingleKES d))

instance DSIGNAlgorithm d => ToCBOR (VerKeyKES (SingleKES d)) where
  toCBOR = encodeVerKeyKES
  encodedSizeExpr _size = encodedVerKeyKESSizeExpr

instance DSIGNAlgorithm d => FromCBOR (VerKeyKES (SingleKES d)) where
  fromCBOR = decodeVerKeyKES


--
-- SignKey instances
--

deriving instance DSIGNAlgorithm d => Show (SignKeyKES (SingleKES d))

instance DSIGNAlgorithm d => NoThunks (VerKeyKES  (SingleKES d))

instance DSIGNAlgorithm d => ToCBOR (SignKeyKES (SingleKES d)) where
  toCBOR = encodeSignKeyKES
  encodedSizeExpr _size = encodedSignKeyKESSizeExpr

instance DSIGNAlgorithm d => FromCBOR (SignKeyKES (SingleKES d)) where
  fromCBOR = decodeSignKeyKES


--
-- Sig instances
--

deriving instance DSIGNAlgorithm d => Show (SigKES (SingleKES d))
deriving instance DSIGNAlgorithm d => Eq   (SigKES (SingleKES d))

instance DSIGNAlgorithm d => NoThunks (SigKES (SingleKES d))

instance DSIGNAlgorithm d => ToCBOR (SigKES (SingleKES d)) where
  toCBOR = encodeSigKES
  encodedSizeExpr _size = encodedSigKESSizeExpr

instance DSIGNAlgorithm d => FromCBOR (SigKES (SingleKES d)) where
  fromCBOR = decodeSigKES
