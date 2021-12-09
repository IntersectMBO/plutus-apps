{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Abstract digital signatures.
module Cardano.Crypto.DSIGN.Class
  (
    -- * DSIGN algorithm class
    DSIGNAlgorithm (..)
  , Seed
  , seedSizeDSIGN
  , sizeVerKeyDSIGN
  , sizeSignKeyDSIGN
  , sizeSigDSIGN

    -- * 'SignedDSIGN' wrapper
  , SignedDSIGN (..)
  , signedDSIGN
  , verifySignedDSIGN

    -- * CBOR encoding and decoding
  , encodeVerKeyDSIGN
  , decodeVerKeyDSIGN
  , encodeSignKeyDSIGN
  , decodeSignKeyDSIGN
  , encodeSigDSIGN
  , decodeSigDSIGN
  , encodeSignedDSIGN
  , decodeSignedDSIGN

    -- * Encoded 'Size' expresssions
  , encodedVerKeyDSIGNSizeExpr
  , encodedSignKeyDESIGNSizeExpr
  , encodedSigDSIGNSizeExpr
  )
where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import GHC.Exts (Constraint)
import GHC.Generics (Generic)
import GHC.Stack
import GHC.TypeLits (KnownNat, Nat, natVal, TypeError, ErrorMessage (..))
import NoThunks.Class (NoThunks)

import Cardano.Binary (Decoder, decodeBytes, Encoding, encodeBytes, Size, withWordSize)

import Cardano.Crypto.Util (Empty)
import Cardano.Crypto.Seed
import Cardano.Crypto.Hash.Class (HashAlgorithm, Hash, hashWith)



class ( Typeable v
      , Show (VerKeyDSIGN v)
      , Eq (VerKeyDSIGN v)
      , Show (SignKeyDSIGN v)
      , Show (SigDSIGN v)
      , Eq (SigDSIGN v)
      , NoThunks (SigDSIGN v)
      , NoThunks (SignKeyDSIGN v)
      , NoThunks (VerKeyDSIGN v)
      , KnownNat (SeedSizeDSIGN v)
      , KnownNat (SizeVerKeyDSIGN v)
      , KnownNat (SizeSignKeyDSIGN v)
      , KnownNat (SizeSigDSIGN v)
      )
      => DSIGNAlgorithm v where

  type SeedSizeDSIGN    v :: Nat
  type SizeVerKeyDSIGN  v :: Nat
  type SizeSignKeyDSIGN v :: Nat
  type SizeSigDSIGN     v :: Nat

  --
  -- Key and signature types
  --

  data VerKeyDSIGN  v :: Type
  data SignKeyDSIGN v :: Type
  data SigDSIGN     v :: Type

  --
  -- Metadata and basic key operations
  --

  algorithmNameDSIGN :: proxy v -> String

  deriveVerKeyDSIGN :: SignKeyDSIGN v -> VerKeyDSIGN v

  hashVerKeyDSIGN :: HashAlgorithm h => VerKeyDSIGN v -> Hash h (VerKeyDSIGN v)
  hashVerKeyDSIGN = hashWith rawSerialiseVerKeyDSIGN


  --
  -- Core algorithm operations
  --

  -- | Context required to run the DSIGN algorithm
  --
  -- Unit by default (no context required)
  type ContextDSIGN v :: Type
  type ContextDSIGN v = ()

  type Signable v :: Type -> Constraint
  type Signable v = Empty

  signDSIGN
    :: (Signable v a, HasCallStack)
    => ContextDSIGN v
    -> a
    -> SignKeyDSIGN v
    -> SigDSIGN v

  verifyDSIGN
    :: (Signable v a, HasCallStack)
    => ContextDSIGN v
    -> VerKeyDSIGN v
    -> a
    -> SigDSIGN v
    -> Either String ()


  --
  -- Key generation
  --

  -- | Note that this function may error (with 'SeedBytesExhausted') if the
  -- provided seed is not long enough. Callers should ensure that the seed has
  -- is at least 'seedSizeDSIGN' bytes long.
  genKeyDSIGN :: Seed -> SignKeyDSIGN v

  --
  -- Serialisation/(de)serialisation in fixed-size raw format
  --

  rawSerialiseVerKeyDSIGN    :: VerKeyDSIGN  v -> ByteString
  rawSerialiseSignKeyDSIGN   :: SignKeyDSIGN v -> ByteString
  rawSerialiseSigDSIGN       :: SigDSIGN     v -> ByteString

  rawDeserialiseVerKeyDSIGN  :: ByteString -> Maybe (VerKeyDSIGN  v)
  rawDeserialiseSignKeyDSIGN :: ByteString -> Maybe (SignKeyDSIGN v)
  rawDeserialiseSigDSIGN     :: ByteString -> Maybe (SigDSIGN     v)

--
-- Do not provide Ord instances for keys, see #38
--

instance ( TypeError ('Text "Ord not supported for signing keys, use the hash instead")
         , Eq (SignKeyDSIGN v)
         )
      => Ord (SignKeyDSIGN v) where
    compare = error "unsupported"

instance ( TypeError ('Text "Ord not supported for verification keys, use the hash instead")
         , Eq (VerKeyDSIGN v)
         )
      => Ord (VerKeyDSIGN v) where
    compare = error "unsupported"

-- | The upper bound on the 'Seed' size needed by 'genKeyDSIGN'
seedSizeDSIGN :: forall v proxy. DSIGNAlgorithm v => proxy v -> Word
seedSizeDSIGN _ = fromInteger (natVal (Proxy @(SeedSizeDSIGN v)))

sizeVerKeyDSIGN    :: forall v proxy. DSIGNAlgorithm v => proxy v -> Word
sizeVerKeyDSIGN  _ = fromInteger (natVal (Proxy @(SizeVerKeyDSIGN v)))
sizeSignKeyDSIGN   :: forall v proxy. DSIGNAlgorithm v => proxy v -> Word
sizeSignKeyDSIGN _ = fromInteger (natVal (Proxy @(SizeSignKeyDSIGN v)))
sizeSigDSIGN       :: forall v proxy. DSIGNAlgorithm v => proxy v -> Word
sizeSigDSIGN     _ = fromInteger (natVal (Proxy @(SizeSigDSIGN v)))

--
-- Convenient CBOR encoding/decoding
--
-- Implementations in terms of the raw (de)serialise
--

encodeVerKeyDSIGN :: DSIGNAlgorithm v => VerKeyDSIGN v -> Encoding
encodeVerKeyDSIGN = encodeBytes . rawSerialiseVerKeyDSIGN

encodeSignKeyDSIGN :: DSIGNAlgorithm v => SignKeyDSIGN v -> Encoding
encodeSignKeyDSIGN = encodeBytes . rawSerialiseSignKeyDSIGN

encodeSigDSIGN :: DSIGNAlgorithm v => SigDSIGN v -> Encoding
encodeSigDSIGN = encodeBytes . rawSerialiseSigDSIGN

decodeVerKeyDSIGN :: forall v s. DSIGNAlgorithm v => Decoder s (VerKeyDSIGN v)
decodeVerKeyDSIGN = do
    bs <- decodeBytes
    case rawDeserialiseVerKeyDSIGN bs of
      Just vk -> return vk
      Nothing
        | actual /= expected
                    -> fail ("decodeVerKeyDSIGN: wrong length, expected " ++
                             show expected ++ " bytes but got " ++ show actual)
        | otherwise -> fail "decodeVerKeyDSIGN: cannot decode key"
        where
          expected = fromIntegral (sizeVerKeyDSIGN (Proxy :: Proxy v))
          actual   = BS.length bs

decodeSignKeyDSIGN :: forall v s. DSIGNAlgorithm v => Decoder s (SignKeyDSIGN v)
decodeSignKeyDSIGN = do
    bs <- decodeBytes
    case rawDeserialiseSignKeyDSIGN bs of
      Just sk -> return sk
      Nothing
        | actual /= expected
                    -> fail ("decodeSignKeyDSIGN: wrong length, expected " ++
                             show expected ++ " bytes but got " ++ show actual)
        | otherwise -> fail "decodeSignKeyDSIGN: cannot decode key"
        where
          expected = fromIntegral (sizeSignKeyDSIGN (Proxy :: Proxy v))
          actual   = BS.length bs

decodeSigDSIGN :: forall v s. DSIGNAlgorithm v => Decoder s (SigDSIGN v)
decodeSigDSIGN = do
    bs <- decodeBytes
    case rawDeserialiseSigDSIGN bs of
      Just sig -> return sig
      Nothing
        | actual /= expected
                    -> fail ("decodeSigDSIGN: wrong length, expected " ++
                             show expected ++ " bytes but got " ++ show actual)
        | otherwise -> fail "decodeSigDSIGN: cannot decode signature"
        where
          expected = fromIntegral (sizeSigDSIGN (Proxy :: Proxy v))
          actual   = BS.length bs


newtype SignedDSIGN v a = SignedDSIGN (SigDSIGN v)
  deriving Generic

deriving instance DSIGNAlgorithm v => Show (SignedDSIGN v a)
deriving instance DSIGNAlgorithm v => Eq   (SignedDSIGN v a)

instance DSIGNAlgorithm v => NoThunks (SignedDSIGN v a)
  -- use generic instance

signedDSIGN
  :: (DSIGNAlgorithm v, Signable v a)
  => ContextDSIGN v
  -> a
  -> SignKeyDSIGN v
  -> SignedDSIGN v a
signedDSIGN ctxt a key = SignedDSIGN (signDSIGN ctxt a key)

verifySignedDSIGN
  :: (DSIGNAlgorithm v, Signable v a, HasCallStack)
  => ContextDSIGN v
  -> VerKeyDSIGN v
  -> a
  -> SignedDSIGN v a
  -> Either String ()
verifySignedDSIGN ctxt key a (SignedDSIGN s) = verifyDSIGN ctxt key a s

encodeSignedDSIGN :: DSIGNAlgorithm v => SignedDSIGN v a -> Encoding
encodeSignedDSIGN (SignedDSIGN s) = encodeSigDSIGN s

decodeSignedDSIGN :: DSIGNAlgorithm v => Decoder s (SignedDSIGN v a)
decodeSignedDSIGN = SignedDSIGN <$> decodeSigDSIGN

--
-- Encoded 'Size' expressions for 'ToCBOR' instances
--

-- | 'Size' expression for 'VerKeyDSIGN' which is using 'sizeVerKeyDSIGN'
-- encoded as 'Size'.
--
encodedVerKeyDSIGNSizeExpr :: forall v. DSIGNAlgorithm v => Proxy (VerKeyDSIGN v) -> Size
encodedVerKeyDSIGNSizeExpr _proxy =
      -- 'encodeBytes' envelope
      fromIntegral ((withWordSize :: Word -> Integer) (sizeVerKeyDSIGN (Proxy :: Proxy v)))
      -- payload
    + fromIntegral (sizeVerKeyDSIGN (Proxy :: Proxy v))

-- | 'Size' expression for 'SignKeyDSIGN' which is using 'sizeSignKeyDSIGN'
-- encoded as 'Size'.
--
encodedSignKeyDESIGNSizeExpr :: forall v. DSIGNAlgorithm v => Proxy (SignKeyDSIGN v) -> Size
encodedSignKeyDESIGNSizeExpr _proxy =
      -- 'encodeBytes' envelope
      fromIntegral ((withWordSize :: Word -> Integer) (sizeSignKeyDSIGN (Proxy :: Proxy v)))
      -- payload
    + fromIntegral (sizeSignKeyDSIGN (Proxy :: Proxy v))

-- | 'Size' expression for 'SigDSIGN' which is using 'sizeSigDSIGN' encoded as
-- 'Size'.
--
encodedSigDSIGNSizeExpr :: forall v. DSIGNAlgorithm v => Proxy (SigDSIGN v) -> Size
encodedSigDSIGNSizeExpr _proxy =
      -- 'encodeBytes' envelope
      fromIntegral ((withWordSize :: Word -> Integer) (sizeSigDSIGN (Proxy :: Proxy v)))
      -- payload
    + fromIntegral (sizeSigDSIGN (Proxy :: Proxy v))
