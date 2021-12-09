{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A key evolving signatures implementation.
--
-- It is a naive recursive implementation of the sum composition from
-- section 3.1 of the \"MMM\" paper:
--
-- /Composition and Efficiency Tradeoffs for Forward-Secure Digital Signatures/
-- By Tal Malkin, Daniele Micciancio and Sara Miner
-- <https://eprint.iacr.org/2001/034>
--
-- Specfically we do the binary sum composition directly as in the paper, and
-- then use that in a nested\/recursive fashion to construct a 7-level deep
-- binary tree version.
--
-- This relies on "Cardano.Crypto.KES.Single" for the base case.
--
module Cardano.Crypto.KES.Sum (
    SumKES
  , VerKeyKES (..)
  , SignKeyKES (..)
  , SigKES (..)

    -- * Type aliases for powers of binary sums
  , Sum0KES
  , Sum1KES
  , Sum2KES
  , Sum3KES
  , Sum4KES
  , Sum5KES
  , Sum6KES
  , Sum7KES
  ) where

import           Data.Proxy (Proxy(..))
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import qualified Data.ByteString as BS
import           Control.Monad (guard)
import           NoThunks.Class (NoThunks)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))

import           Cardano.Crypto.Seed
import           Cardano.Crypto.Hash.Class
import           Cardano.Crypto.KES.Class
import           Cardano.Crypto.KES.Single (SingleKES)
import Data.Word (Word8)
import Control.DeepSeq (NFData)


-- | A 2^0 period KES
type Sum0KES d   = SingleKES d

-- | A 2^1 period KES
type Sum1KES d h = SumKES h (Sum0KES d)

-- | A 2^2 period KES
type Sum2KES d h = SumKES h (Sum1KES d h)

-- | A 2^3 period KES
type Sum3KES d h = SumKES h (Sum2KES d h)

-- | A 2^4 period KES
type Sum4KES d h = SumKES h (Sum3KES d h)

-- | A 2^5 period KES
type Sum5KES d h = SumKES h (Sum4KES d h)

-- | A 2^6 period KES
type Sum6KES d h = SumKES h (Sum5KES d h)

-- | A 2^7 period KES
type Sum7KES d h = SumKES h (Sum6KES d h)


-- | A composition of two KES schemes to give a KES scheme with the sum of
-- the time periods.
--
-- While we could do this with two independent KES schemes (i.e. two types)
-- we only need it for two instances of the same scheme, and we save
-- substantially on the size of the type and runtime dictionaries if we do it
-- this way, especially when we start applying it recursively.
--
data SumKES h d

instance (NFData (SigKES d), NFData (VerKeyKES d)) =>
  NFData (SigKES (SumKES h d)) where

instance (NFData (SignKeyKES d), NFData (VerKeyKES d)) =>
  NFData (SignKeyKES (SumKES h d)) where

instance (KESAlgorithm d, HashAlgorithm h, Typeable d)
      => KESAlgorithm (SumKES h d) where

    type SeedSizeKES (SumKES h d) = SeedSizeKES d

    --
    -- Key and signature types
    --

    -- | From Section 3,1:
    --
    -- The verification key @vk@ for the sum scheme is the hash of the
    -- verification keys @vk_0, vk_1@ of the two constituent schemes.
    --
    newtype VerKeyKES (SumKES h d) =
              VerKeySumKES (Hash h (VerKeyKES d, VerKeyKES d))
        deriving Generic
        deriving newtype NFData

    -- | From Figure 3: @(sk_0, r_1, vk_0, vk_1)@
    --
    data SignKeyKES (SumKES h d) =
           SignKeySumKES !(SignKeyKES d)
                         !Seed
                         !(VerKeyKES d)
                         !(VerKeyKES d)
        deriving Generic

    -- | From Figure 3: @(sigma, vk_0, vk_1)@
    --
    data SigKES (SumKES h d) =
           SigSumKES !(SigKES d)
                     !(VerKeyKES d)
                     !(VerKeyKES d)
        deriving Generic


    --
    -- Metadata and basic key operations
    --

    algorithmNameKES _ = mungeName (algorithmNameKES (Proxy :: Proxy d))

    deriveVerKeyKES (SignKeySumKES _ _ vk_0 vk_1) =
        VerKeySumKES (hashPairOfVKeys (vk_0, vk_1))

    -- The verification key in this scheme is actually a hash already
    -- however the type of hashVerKeyKES says the caller gets to choose
    -- the hash, not the implementation. So that's why we have to hash
    -- the hash here. We could alternatively provide a "key identifier"
    -- function and let the implementation choose what that is.
    hashVerKeyKES (VerKeySumKES vk) = castHash (hashWith hashToBytes vk)


    --
    -- Core algorithm operations
    --

    type Signable   (SumKES h d) = Signable   d
    type ContextKES (SumKES h d) = ContextKES d

    signKES ctxt t a (SignKeySumKES sk _r_1 vk_0 vk_1) =
        SigSumKES sigma vk_0 vk_1
      where
        sigma | t < _T    = signKES ctxt  t       a sk
              | otherwise = signKES ctxt (t - _T) a sk

        _T = totalPeriodsKES (Proxy :: Proxy d)

    verifyKES ctxt (VerKeySumKES vk) t a (SigSumKES sigma vk_0 vk_1)
      | hashPairOfVKeys (vk_0, vk_1) /= vk
                  = Left "Reject"
      | t < _T    = verifyKES ctxt vk_0  t       a sigma
      | otherwise = verifyKES ctxt vk_1 (t - _T) a sigma
      where
        _T = totalPeriodsKES (Proxy :: Proxy d)

    updateKES ctx (SignKeySumKES sk r_1 vk_0 vk_1) t
      | t+1 <  _T = do sk' <- updateKES ctx sk t
                       return $ SignKeySumKES sk' r_1 vk_0 vk_1
      | t+1 == _T = do let sk' = genKeyKES r_1
                       return $ SignKeySumKES sk' zero vk_0 vk_1
      | otherwise = do sk' <- updateKES ctx sk (t - _T)
                       return $ SignKeySumKES sk' r_1 vk_0 vk_1
      where
        _T = totalPeriodsKES (Proxy :: Proxy d)
        zero = zeroSeed (Proxy :: Proxy d)

    totalPeriodsKES  _ = 2 * totalPeriodsKES (Proxy :: Proxy d)


    --
    -- Key generation
    --

    seedSizeKES _ = seedSizeKES (Proxy :: Proxy d)
    genKeyKES r = SignKeySumKES sk_0 r1 vk_0 vk_1
      where
        (r0, r1) = expandSeed (Proxy :: Proxy h) r

        sk_0 = genKeyKES r0
        vk_0 = deriveVerKeyKES sk_0

        sk_1 = genKeyKES r1
        vk_1 = deriveVerKeyKES sk_1


    --
    -- raw serialise/deserialise
    --

    sizeVerKeyKES  _ = sizeHash       (Proxy :: Proxy h)
    sizeSignKeyKES _ = sizeSignKeyKES (Proxy :: Proxy d)
                     + seedSizeKES    (Proxy :: Proxy d)
                     + sizeVerKeyKES  (Proxy :: Proxy d) * 2
    sizeSigKES     _ = sizeSigKES     (Proxy :: Proxy d)
                     + sizeVerKeyKES  (Proxy :: Proxy d) * 2

    rawSerialiseVerKeyKES  (VerKeySumKES  vk) = hashToBytes vk

    rawSerialiseSignKeyKES (SignKeySumKES sk r_1 vk_0 vk_1) =
      mconcat
        [ rawSerialiseSignKeyKES sk
        , getSeedBytes r_1
        , rawSerialiseVerKeyKES vk_0
        , rawSerialiseVerKeyKES vk_1
        ]

    rawSerialiseSigKES (SigSumKES sigma vk_0 vk_1) =
      mconcat
        [ rawSerialiseSigKES sigma
        , rawSerialiseVerKeyKES vk_0
        , rawSerialiseVerKeyKES vk_1
        ]

    rawDeserialiseVerKeyKES = fmap VerKeySumKES  . hashFromBytes

    rawDeserialiseSignKeyKES b = do
        guard (BS.length b == fromIntegral size_total)
        sk   <- rawDeserialiseSignKeyKES b_sk
        let r = mkSeedFromBytes          b_r
        vk_0 <- rawDeserialiseVerKeyKES  b_vk0
        vk_1 <- rawDeserialiseVerKeyKES  b_vk1
        return (SignKeySumKES sk r vk_0 vk_1)
      where
        b_sk  = slice off_sk  size_sk b
        b_r   = slice off_r   size_r  b
        b_vk0 = slice off_vk0 size_vk b
        b_vk1 = slice off_vk1 size_vk b

        size_sk    = sizeSignKeyKES (Proxy :: Proxy d)
        size_r     = seedSizeKES    (Proxy :: Proxy d)
        size_vk    = sizeVerKeyKES  (Proxy :: Proxy d)
        size_total = sizeSignKeyKES (Proxy :: Proxy (SumKES h d))

        off_sk     = 0 :: Word
        off_r      = size_sk
        off_vk0    = off_r + size_r
        off_vk1    = off_vk0 + size_vk

    rawDeserialiseSigKES b = do
        guard (BS.length b == fromIntegral size_total)
        sigma <- rawDeserialiseSigKES    b_sig
        vk_0  <- rawDeserialiseVerKeyKES b_vk0
        vk_1  <- rawDeserialiseVerKeyKES b_vk1
        return (SigSumKES sigma vk_0 vk_1)
      where
        b_sig = slice off_sig size_sig b
        b_vk0 = slice off_vk0 size_vk  b
        b_vk1 = slice off_vk1 size_vk  b

        size_sig   = sizeSigKES    (Proxy :: Proxy d)
        size_vk    = sizeVerKeyKES (Proxy :: Proxy d)
        size_total = sizeSigKES    (Proxy :: Proxy (SumKES h d))

        off_sig    = 0 :: Word
        off_vk0    = size_sig
        off_vk1    = off_vk0 + size_vk

hashPairOfVKeys :: (KESAlgorithm d, HashAlgorithm h)
                => (VerKeyKES d, VerKeyKES d)
                -> Hash h (VerKeyKES d, VerKeyKES d)
hashPairOfVKeys =
    hashWith $ \(a,b) ->
      rawSerialiseVerKeyKES a <> rawSerialiseVerKeyKES b

slice :: Word -> Word -> ByteString -> ByteString
slice offset size = BS.take (fromIntegral size)
                  . BS.drop (fromIntegral offset)

zeroSeed :: KESAlgorithm d => Proxy d -> Seed
zeroSeed p = mkSeedFromBytes (BS.replicate seedSize (0 :: Word8))
  where
    seedSize :: Int
    seedSize = fromIntegral (seedSizeKES p)

mungeName :: String -> String
mungeName basename
  | (name, '^':nstr) <- span (/= '^') basename
  , [(n, "")] <- reads nstr
  = name ++ '^' : show (n+1 :: Word)

  | otherwise
  = basename ++ "_2^1"


--
-- VerKey instances
--

deriving instance HashAlgorithm h => Show (VerKeyKES (SumKES h d))
deriving instance Eq   (VerKeyKES (SumKES h d))

instance (KESAlgorithm d) => NoThunks (SignKeyKES (SumKES h d))

instance (KESAlgorithm d, HashAlgorithm h, Typeable d)
      => ToCBOR (VerKeyKES (SumKES h d)) where
  toCBOR = encodeVerKeyKES
  encodedSizeExpr _size = encodedVerKeyKESSizeExpr

instance (KESAlgorithm d, HashAlgorithm h, Typeable d)
      => FromCBOR (VerKeyKES (SumKES h d)) where
  fromCBOR = decodeVerKeyKES


--
-- SignKey instances
--

deriving instance KESAlgorithm d => Show (SignKeyKES (SumKES h d))

instance (KESAlgorithm d) => NoThunks (VerKeyKES  (SumKES h d))

instance (KESAlgorithm d, HashAlgorithm h, Typeable d)
      => ToCBOR (SignKeyKES (SumKES h d)) where
  toCBOR = encodeSignKeyKES
  encodedSizeExpr _size = encodedSignKeyKESSizeExpr

instance (KESAlgorithm d, HashAlgorithm h, Typeable d)
      => FromCBOR (SignKeyKES (SumKES h d)) where
  fromCBOR = decodeSignKeyKES


--
-- Sig instances
--

deriving instance KESAlgorithm d => Show (SigKES (SumKES h d))
deriving instance KESAlgorithm d => Eq   (SigKES (SumKES h d))

instance KESAlgorithm d => NoThunks (SigKES (SumKES h d))

instance (KESAlgorithm d, HashAlgorithm h, Typeable d)
      => ToCBOR (SigKES (SumKES h d)) where
  toCBOR = encodeSigKES
  encodedSizeExpr _size = encodedSigKESSizeExpr

instance (KESAlgorithm d, HashAlgorithm h, Typeable d)
      => FromCBOR (SigKES (SumKES h d)) where
  fromCBOR = decodeSigKES
