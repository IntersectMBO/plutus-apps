{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Mock implementation of digital signatures.
module Cardano.Crypto.DSIGN.Mock
  ( MockDSIGN
  , SignKeyDSIGN (..)
  , VerKeyDSIGN (..)
  , SigDSIGN (..)
  , mockSign
  )
where

import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.TypeLits (type (+))
import Data.Proxy (Proxy (..))
import GHC.Stack
import NoThunks.Class (NoThunks)

import Cardano.Prelude (NFData)
import Cardano.Binary (FromCBOR (..), ToCBOR (..))

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.Seed
import Cardano.Crypto.Hash
import Cardano.Crypto.Util


data MockDSIGN

instance DSIGNAlgorithm MockDSIGN where
    type SeedSizeDSIGN MockDSIGN = 8
    type SizeVerKeyDSIGN  MockDSIGN = 8 -- for 64 bit int
    type SizeSignKeyDSIGN MockDSIGN = 8
    type SizeSigDSIGN     MockDSIGN = SizeHash ShortHash + 8

    --
    -- Key and signature types
    --

    newtype VerKeyDSIGN MockDSIGN = VerKeyMockDSIGN Word64
        deriving stock   (Show, Eq, Generic)
        deriving newtype (Num, NoThunks, NFData)

    newtype SignKeyDSIGN MockDSIGN = SignKeyMockDSIGN Word64
        deriving stock   (Show, Eq, Generic)
        deriving newtype (Num, NoThunks, NFData)

    data SigDSIGN MockDSIGN = SigMockDSIGN !(Hash ShortHash ()) !Word64
        deriving stock    (Show, Eq, Ord, Generic)
        deriving anyclass (NoThunks, NFData)


    --
    -- Metadata and basic key operations
    --

    algorithmNameDSIGN _ = "mock"

    deriveVerKeyDSIGN (SignKeyMockDSIGN n) = VerKeyMockDSIGN n

    --
    -- Core algorithm operations
    --

    type Signable MockDSIGN = SignableRepresentation

    signDSIGN () a sk = mockSign a sk

    verifyDSIGN () (VerKeyMockDSIGN n) a s =
      if s == mockSign a (SignKeyMockDSIGN n)
        then Right ()
        else Left $ show $ MockVerificationFailure {
                 vErrVerKey    = VerKeyMockDSIGN n
               , vErrSignature = s
               , vErrCallStack = prettyCallStack callStack
               }

    --
    -- Key generation
    --

    genKeyDSIGN seed   =
      SignKeyMockDSIGN (runMonadRandomWithSeed seed getRandomWord64)


    --
    -- raw serialise/deserialise
    --


    rawSerialiseVerKeyDSIGN  (VerKeyMockDSIGN  k) = writeBinaryWord64 k
    rawSerialiseSignKeyDSIGN (SignKeyMockDSIGN k) = writeBinaryWord64 k
    rawSerialiseSigDSIGN     (SigMockDSIGN   h k) = hashToBytes h
                                                 <> writeBinaryWord64 k

    rawDeserialiseVerKeyDSIGN bs
      | [kb] <- splitsAt [8] bs
      , let k = readBinaryWord64 kb
      = Just $! VerKeyMockDSIGN k

      | otherwise
      = Nothing

    rawDeserialiseSignKeyDSIGN bs
      | [kb] <- splitsAt [8] bs
      , let k = readBinaryWord64 kb
      = Just $! SignKeyMockDSIGN k

      | otherwise
      = Nothing

    rawDeserialiseSigDSIGN bs
      | [hb, kb] <- splitsAt [fromIntegral $ sizeHash (Proxy :: Proxy ShortHash), 8] bs
      , Just h   <- hashFromBytes hb
      , let k = readBinaryWord64 kb
      = Just $! SigMockDSIGN h k

      | otherwise
      = Nothing


instance ToCBOR (VerKeyDSIGN MockDSIGN) where
  toCBOR = encodeVerKeyDSIGN
  encodedSizeExpr _ = encodedVerKeyDSIGNSizeExpr

instance FromCBOR (VerKeyDSIGN MockDSIGN) where
  fromCBOR = decodeVerKeyDSIGN

instance ToCBOR (SignKeyDSIGN MockDSIGN) where
  toCBOR = encodeSignKeyDSIGN
  encodedSizeExpr _ = encodedSignKeyDESIGNSizeExpr

instance FromCBOR (SignKeyDSIGN MockDSIGN) where
  fromCBOR = decodeSignKeyDSIGN

instance ToCBOR (SigDSIGN MockDSIGN) where
  toCBOR = encodeSigDSIGN
  encodedSizeExpr _ = encodedSigDSIGNSizeExpr

instance FromCBOR (SigDSIGN MockDSIGN) where
  fromCBOR = decodeSigDSIGN


-- | Debugging: provide information about the verification failure
--
-- We don't include the actual value here as that would require propagating a
-- 'Show' constraint.
data VerificationFailure
  = MockVerificationFailure
      { vErrVerKey :: VerKeyDSIGN MockDSIGN
      , vErrSignature :: SigDSIGN MockDSIGN
      , vErrCallStack :: String
      }
  deriving Show

mockSign :: SignableRepresentation a
         => a -> SignKeyDSIGN MockDSIGN -> SigDSIGN MockDSIGN
mockSign a (SignKeyMockDSIGN n) =
  SigMockDSIGN (castHash (hashWith getSignableRepresentation a)) n
