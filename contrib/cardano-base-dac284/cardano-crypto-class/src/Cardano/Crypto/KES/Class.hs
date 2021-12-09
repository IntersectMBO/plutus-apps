{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Abstract key evolving signatures.
module Cardano.Crypto.KES.Class
  (
    -- * KES algorithm class
    KESAlgorithm (..)
  , Period

    -- * 'SignedKES' wrapper
  , SignedKES (..)
  , signedKES
  , verifySignedKES

    -- * CBOR encoding and decoding
  , encodeVerKeyKES
  , decodeVerKeyKES
  , encodeSignKeyKES
  , decodeSignKeyKES
  , encodeSigKES
  , decodeSigKES
  , encodeSignedKES
  , decodeSignedKES

    -- * Encoded 'Size' expressions
  , encodedVerKeyKESSizeExpr
  , encodedSignKeyKESSizeExpr
  , encodedSigKESSizeExpr
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import GHC.Exts (Constraint)
import GHC.Generics (Generic)
import GHC.Stack
import GHC.TypeLits (Nat, KnownNat, natVal, TypeError, ErrorMessage (..))
import NoThunks.Class (NoThunks)

import Cardano.Binary (Decoder, decodeBytes, Encoding, encodeBytes, Size, withWordSize)

import Cardano.Crypto.Seed
import Cardano.Crypto.Util (Empty)
import Cardano.Crypto.Hash.Class (HashAlgorithm, Hash, hashWith)


class ( Typeable v
      , Show (VerKeyKES v)
      , Eq (VerKeyKES v)
      , Show (SignKeyKES v)
      , Show (SigKES v)
      , Eq (SigKES v)
      , NoThunks (SigKES v)
      , NoThunks (SignKeyKES v)
      , NoThunks (VerKeyKES v)
      , KnownNat (SeedSizeKES v)
      )
      => KESAlgorithm v where

  type SeedSizeKES v :: Nat

  --
  -- Key and signature types
  --

  data VerKeyKES  v :: Type
  data SignKeyKES v :: Type
  data SigKES     v :: Type


  --
  -- Metadata and basic key operations
  --

  algorithmNameKES :: proxy v -> String

  deriveVerKeyKES :: SignKeyKES v -> VerKeyKES v

  hashVerKeyKES :: HashAlgorithm h => VerKeyKES v -> Hash h (VerKeyKES v)
  hashVerKeyKES = hashWith rawSerialiseVerKeyKES


  --
  -- Core algorithm operations
  --

  -- | Context required to run the KES algorithm
  --
  -- Unit by default (no context required)
  type ContextKES v :: Type
  type ContextKES v = ()

  type Signable v :: Type -> Constraint
  type Signable v = Empty

  signKES
    :: (Signable v a, HasCallStack)
    => ContextKES v
    -> Period  -- ^ The /current/ period for the key
    -> a
    -> SignKeyKES v
    -> SigKES v

  verifyKES
    :: (Signable v a, HasCallStack)
    => ContextKES v
    -> VerKeyKES v
    -> Period  -- ^ The /current/ period for the key
    -> a
    -> SigKES v
    -> Either String ()

  -- | Update the KES signature key to the /next/ period, given the /current/
  -- period.
  --
  -- It returns 'Nothing' if the cannot be evolved any further.
  --
  -- The precondition (to get a 'Just' result) is that the current KES period
  -- of the input key is not the last period. The given period must be the
  -- current KES period of the input key (not the next or target).
  --
  -- The postcondition is that in case a key is returned, its current KES
  -- period is incremented by one compared to before.
  --
  -- Note that you must track the current period separately, and to skip to a
  -- later period requires repeated use of this function, since it only
  -- increments one period at once.
  --
  updateKES
    :: HasCallStack
    => ContextKES v
    -> SignKeyKES v
    -> Period  -- ^ The /current/ period for the key, not the target period.
    -> Maybe (SignKeyKES v)

  -- | Return the total number of KES periods supported by this algorithm. The
  -- KES algorithm is assumed to support a fixed maximum number of periods, not
  -- a variable number.
  --
  -- Do note that this is the total number of /periods/ not the total number of
  -- evolutions. The difference is off-by-one. For example if there are 2
  -- periods (period 0 and 1) then there is only one evolution.
  --
  totalPeriodsKES
    :: proxy v -> Word


  --
  -- Key generation
  --

  genKeyKES :: Seed -> SignKeyKES v

  -- | The upper bound on the 'Seed' size needed by 'genKeyKES'
  seedSizeKES :: proxy v -> Word
  seedSizeKES _ = fromInteger (natVal (Proxy @(SeedSizeKES v)))

  --
  -- Secure forgetting
  --

  -- | Forget a signing key synchronously, rather than waiting for GC. In some
  -- non-mock instances this provides a guarantee that the signing key is no
  -- longer in memory.
  --
  -- The precondition is that this key value will not be used again.
  --
  forgetSignKeyKES :: SignKeyKES v -> IO ()
  forgetSignKeyKES = const $ return ()

  --
  -- Serialisation/(de)serialisation in fixed-size raw format
  --

  sizeVerKeyKES  :: proxy v -> Word
  sizeSignKeyKES :: proxy v -> Word
  sizeSigKES     :: proxy v -> Word

  rawSerialiseVerKeyKES    :: VerKeyKES  v -> ByteString
  rawSerialiseSignKeyKES   :: SignKeyKES v -> ByteString
  rawSerialiseSigKES       :: SigKES     v -> ByteString

  rawDeserialiseVerKeyKES  :: ByteString -> Maybe (VerKeyKES v)
  rawDeserialiseSignKeyKES :: ByteString -> Maybe (SignKeyKES v)
  rawDeserialiseSigKES     :: ByteString -> Maybe (SigKES v)

--
-- Do not provide Ord instances for keys, see #38
--

instance ( TypeError ('Text "Ord not supported for signing keys, use the hash instead")
         , Eq (SignKeyKES v)
         )
      => Ord (SignKeyKES v) where
    compare = error "unsupported"

instance ( TypeError ('Text "Ord not supported for verification keys, use the hash instead")
         , KESAlgorithm v
         )
      => Ord (VerKeyKES v) where
    compare = error "unsupported"

--
-- Convenient CBOR encoding/decoding
--
-- Implementations in terms of the raw (de)serialise
--

encodeVerKeyKES :: KESAlgorithm v => VerKeyKES v -> Encoding
encodeVerKeyKES = encodeBytes . rawSerialiseVerKeyKES

encodeSignKeyKES :: KESAlgorithm v => SignKeyKES v -> Encoding
encodeSignKeyKES = encodeBytes . rawSerialiseSignKeyKES

encodeSigKES :: KESAlgorithm v => SigKES v -> Encoding
encodeSigKES = encodeBytes . rawSerialiseSigKES

decodeVerKeyKES :: forall v s. KESAlgorithm v => Decoder s (VerKeyKES v)
decodeVerKeyKES = do
    bs <- decodeBytes
    case rawDeserialiseVerKeyKES bs of
      Just vk -> return vk
      Nothing
        | actual /= expected
                    -> fail ("decodeVerKeyKES: wrong length, expected " ++
                             show expected ++ " bytes but got " ++ show actual)
        | otherwise -> fail "decodeVerKeyKES: cannot decode key"
        where
          expected = fromIntegral (sizeVerKeyKES (Proxy :: Proxy v))
          actual   = BS.length bs

decodeSignKeyKES :: forall v s. KESAlgorithm v => Decoder s (SignKeyKES v)
decodeSignKeyKES = do
    bs <- decodeBytes
    case rawDeserialiseSignKeyKES bs of
      Just sk -> return sk
      Nothing
        | actual /= expected
                    -> fail ("decodeSignKeyKES: wrong length, expected " ++
                             show expected ++ " bytes but got " ++ show actual)
        | otherwise -> fail "decodeSignKeyKES: cannot decode key"
        where
          expected = fromIntegral (sizeSignKeyKES (Proxy :: Proxy v))
          actual   = BS.length bs

decodeSigKES :: forall v s. KESAlgorithm v => Decoder s (SigKES v)
decodeSigKES = do
    bs <- decodeBytes
    case rawDeserialiseSigKES bs of
      Just sig -> return sig
      Nothing
        | actual /= expected
                    -> fail ("decodeSigKES: wrong length, expected " ++
                             show expected ++ " bytes but got " ++ show actual)
        | otherwise -> fail "decodeSigKES: cannot decode key"
        where
          expected = fromIntegral (sizeSigKES (Proxy :: Proxy v))
          actual   = BS.length bs


-- | The KES period. Periods are enumerated from zero.
--
-- Be careful of fencepost errors: if there are 2 periods (period 0 and 1)
-- then there is only one key evolution.
--
type Period = Word

newtype SignedKES v a = SignedKES {getSig :: SigKES v}
  deriving Generic

deriving instance KESAlgorithm v => Show (SignedKES v a)
deriving instance KESAlgorithm v => Eq   (SignedKES v a)

instance KESAlgorithm v => NoThunks (SignedKES v a)
  -- use generic instance

signedKES
  :: (KESAlgorithm v, Signable v a)
  => ContextKES v
  -> Period
  -> a
  -> SignKeyKES v
  -> SignedKES v a
signedKES ctxt time a key = SignedKES (signKES ctxt time a key)

verifySignedKES
  :: (KESAlgorithm v, Signable v a)
  => ContextKES v
  -> VerKeyKES v
  -> Period
  -> a
  -> SignedKES v a
  -> Either String ()
verifySignedKES ctxt vk j a (SignedKES sig) = verifyKES ctxt vk j a sig

encodeSignedKES :: KESAlgorithm v => SignedKES v a -> Encoding
encodeSignedKES (SignedKES s) = encodeSigKES s

decodeSignedKES :: KESAlgorithm v => Decoder s (SignedKES v a)
decodeSignedKES = SignedKES <$> decodeSigKES

--
-- 'Size' expressions for 'ToCBOR' instances.
--

-- | 'Size' expression for 'VerKeyKES' which is using 'sizeVerKeyKES' encoded
-- as 'Size'.
--
encodedVerKeyKESSizeExpr :: forall v. KESAlgorithm v => Proxy (VerKeyKES v) -> Size
encodedVerKeyKESSizeExpr _proxy =
      -- 'encodeBytes' envelope
      fromIntegral ((withWordSize :: Word -> Integer) (sizeVerKeyKES (Proxy :: Proxy v)))
      -- payload
    + fromIntegral (sizeVerKeyKES (Proxy :: Proxy v))

-- | 'Size' expression for 'SignKeyKES' which is using 'sizeSignKeyKES' encoded
-- as 'Size'.
--
encodedSignKeyKESSizeExpr :: forall v. KESAlgorithm v => Proxy (SignKeyKES v) -> Size
encodedSignKeyKESSizeExpr _proxy =
      -- 'encodeBytes' envelope
      fromIntegral ((withWordSize :: Word -> Integer) (sizeSignKeyKES (Proxy :: Proxy v)))
      -- payload
    + fromIntegral (sizeSignKeyKES (Proxy :: Proxy v))

-- | 'Size' expression for 'SigKES' which is using 'sizeSigKES' encoded as
-- 'Size'.
--
encodedSigKESSizeExpr :: forall v. KESAlgorithm v => Proxy (SigKES v) -> Size
encodedSigKESSizeExpr _proxy =
      -- 'encodeBytes' envelope
      fromIntegral ((withWordSize :: Word -> Integer) (sizeSigKES (Proxy :: Proxy v)))
      -- payload
    + fromIntegral (sizeSigKES (Proxy :: Proxy v))
