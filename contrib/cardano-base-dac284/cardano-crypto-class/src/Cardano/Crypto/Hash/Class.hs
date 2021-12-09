{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MagicHash #-}

-- | Abstract hashing functionality.
module Cardano.Crypto.Hash.Class
  ( HashAlgorithm (..)
  , sizeHash
  , ByteString
  , Hash(UnsafeHash)

    -- * Core operations
  , hashWith
  , hashWithSerialiser

    -- * Conversions
  , castHash
  , hashToBytes
  , hashFromBytes
  , hashToBytesShort
  , hashFromBytesShort
  , ViewHash32 (..)
  , unsafeMkHash32
  , viewHash32

    -- * Rendering and parsing
  , hashToBytesAsHex
  , hashFromBytesAsHex
  , hashToTextAsHex
  , hashFromTextAsHex
  , hashToStringAsHex
  , hashFromStringAsHex

    -- * Other operations
  , xor

    -- * Deprecated
  , hash
  , fromHash
  , hashRaw
  , getHash
  , getHashBytesAsHex
  )
where

import Data.List (foldl')
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat, natVal, sameNat)
import Data.Type.Equality ((:~:)(Refl))

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.Word (Word8)
import Numeric.Natural (Natural)

import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Data.Aeson (FromJSON(..), FromJSONKey(..), ToJSON(..), ToJSONKey(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Aeson.Types as Aeson

import Control.DeepSeq (NFData)

import NoThunks.Class (NoThunks)

import Cardano.Binary (Encoding, FromCBOR(..), Size, ToCBOR(..), decodeBytes,
                       serializeEncoding')
import Cardano.Crypto.PackedBytes
import Cardano.Prelude (HeapWords (..))

import qualified Data.ByteString.Short.Internal as SBSI

class (KnownNat (SizeHash h), Typeable h) => HashAlgorithm h where
  --TODO: eliminate this Typeable constraint needed only for the ToCBOR
  -- the ToCBOR should not need it either

  -- | Size of hash digest
  type SizeHash h :: Nat

  hashAlgorithmName :: proxy h -> String

  digest :: proxy h -> ByteString -> ByteString

-- | The size in bytes of the output of 'digest'
sizeHash :: forall h proxy. HashAlgorithm h => proxy h -> Word
sizeHash _ = fromInteger (natVal (Proxy @(SizeHash h)))

newtype Hash h a = UnsafeHashRep (PackedBytes (SizeHash h))
  deriving (Eq, Ord, Generic, NoThunks, NFData)

pattern UnsafeHash :: forall h a. HashAlgorithm h => ShortByteString -> Hash h a
pattern UnsafeHash bytes <- UnsafeHashRep (unpackBytes -> bytes)
  where
  UnsafeHash bytes =
    case hashFromBytesShort bytes of
      Nothing ->
        error "UnsafeHash: mismatched size of the supplied ShortByteString and the expected digest"
      Just h -> h
{-# COMPLETE UnsafeHash #-}

--
-- Core operations
--

-- | Hash the given value, using a serialisation function to turn it into bytes.
--
hashWith :: forall h a. HashAlgorithm h => (a -> ByteString) -> a -> Hash h a
hashWith serialise =
    UnsafeHashRep
  . packPinnedBytes
  . digest (Proxy :: Proxy h)
  . serialise


-- | A variation on 'hashWith', but specially for CBOR encodings.
--
hashWithSerialiser :: forall h a. HashAlgorithm h => (a -> Encoding) -> a -> Hash h a
hashWithSerialiser toEnc = hashWith (serializeEncoding' . toEnc)


--
-- Conversions
--

-- | Cast the type of the hashed data.
--
-- The 'Hash' type has a phantom type parameter to indicate what type the
-- hash is of. It is sometimes necessary to fake this and hash a value of one
-- type and use it where as hash of a different type is expected.
--
castHash :: Hash h a -> Hash h b
castHash (UnsafeHashRep h) = UnsafeHashRep h


-- | The representation of the hash as bytes.
--
hashToBytes :: Hash h a -> ByteString
hashToBytes (UnsafeHashRep h) = unpackPinnedBytes h


-- | Make a hash from it bytes representation.
--
-- It must be a a bytestring of the correct length, as given by 'sizeHash'.
--
hashFromBytes :: forall h a. HashAlgorithm h => ByteString -> Maybe (Hash h a)
hashFromBytes bytes
  | BS.length bytes == fromIntegral (sizeHash (Proxy :: Proxy h))
  = Just $! UnsafeHashRep (packPinnedBytes bytes)

  | otherwise
  = Nothing

-- | Make a hash from it bytes representation, as a 'ShortByteString'.
--
-- It must be a a bytestring of the correct length, as given by 'sizeHash'.
--
hashFromBytesShort :: forall h a. HashAlgorithm h
                   => ShortByteString -> Maybe (Hash h a)
hashFromBytesShort bytes
  | SBS.length bytes == fromIntegral (sizeHash (Proxy :: Proxy h))
  = Just $! UnsafeHashRep (packBytes bytes)

  | otherwise
  = Nothing


-- | The representation of the hash as bytes, as a 'ShortByteString'.
--
hashToBytesShort :: Hash h a -> ShortByteString
hashToBytesShort (UnsafeHashRep h) = unpackBytes h


--
-- Rendering and parsing
--

-- | Convert the hash to hex encoding, as 'String'.
hashToStringAsHex :: Hash h a -> String
hashToStringAsHex = Text.unpack . hashToTextAsHex

-- | Make a hash from hex-encoded 'String' representation.
--
-- This can fail for the same reason as 'hashFromBytes', or because the input
-- is invalid hex. The whole byte string must be valid hex, not just a prefix.
--
hashFromStringAsHex :: HashAlgorithm h => String -> Maybe (Hash h a)
hashFromStringAsHex = hashFromTextAsHex . Text.pack

-- | Convert the hash to hex encoding, as 'Text'.
--
hashToTextAsHex :: Hash h a -> Text
hashToTextAsHex = Text.decodeUtf8 . hashToBytesAsHex

-- | Make a hash from hex-encoded 'Text' representation.
--
-- This can fail for the same reason as 'hashFromBytes', or because the input
-- is invalid hex. The whole byte string must be valid hex, not just a prefix.
--
hashFromTextAsHex :: HashAlgorithm h => Text -> Maybe (Hash h a)
hashFromTextAsHex = hashFromBytesAsHex . Text.encodeUtf8

-- | Convert the hash to hex encoding, as 'ByteString'.
--
hashToBytesAsHex :: Hash h a -> ByteString
hashToBytesAsHex = Base16.encode . hashToBytes

-- | Make a hash from hex-encoded 'ByteString' representation.
--
-- This can fail for the same reason as 'hashFromBytes', or because the input
-- is invalid hex. The whole byte string must be valid hex, not just a prefix.
--
hashFromBytesAsHex :: HashAlgorithm h => ByteString -> Maybe (Hash h a)
hashFromBytesAsHex bsHex = do
  Right bs <- Just $ Base16.decode bsHex
  hashFromBytes bs

instance Show (Hash h a) where
  show = show . hashToStringAsHex

instance HashAlgorithm h => Read (Hash h a) where
  readsPrec p str = [ (h, y) | (x, y) <- readsPrec p str, h <- maybeToList (hashFromStringAsHex x) ]

instance HashAlgorithm h => IsString (Hash h a) where
  fromString str =
    case hashFromBytesAsHex (BSC.pack str) of
      Just x  -> x
      Nothing -> error ("fromString: cannot decode hash " ++ show str)

instance HashAlgorithm h => ToJSONKey (Hash h a) where
  toJSONKey = Aeson.ToJSONKeyText hashToText (Aeson.text . hashToText)

instance HashAlgorithm h => FromJSONKey (Hash h a) where
  fromJSONKey = Aeson.FromJSONKeyTextParser parseHash

instance HashAlgorithm h => ToJSON (Hash h a) where
  toJSON = toJSON . hashToText

instance HashAlgorithm h => FromJSON (Hash h a) where
  parseJSON = Aeson.withText "hash" parseHash

instance HeapWords (Hash h a) where
  heapWords (UnsafeHashRep (PackedBytes8 _)) = 1 + 1
  heapWords (UnsafeHashRep (PackedBytes28 _ _ _ _)) = 1 + 4
  heapWords (UnsafeHashRep (PackedBytes32 _ _ _ _)) = 1 + 4
  heapWords (UnsafeHashRep (PackedBytes# ba#)) = heapWords (SBSI.SBS ba#)

-- utils used in the instances above
hashToText :: Hash h a -> Text
hashToText = Text.decodeLatin1 . hashToBytesAsHex

parseHash :: HashAlgorithm crypto => Text -> Aeson.Parser (Hash crypto a)
parseHash t =
    case Base16.decode (Text.encodeUtf8 t) of
      Right bytes -> maybe badSize return (hashFromBytes bytes)
      Left _      -> badHex
  where
    badHex :: Aeson.Parser b
    badHex = fail "Hashes are expected in hex encoding"

    badSize :: Aeson.Parser (Hash crypto a)
    badSize = fail "Hash is the wrong length"

--
-- CBOR serialisation
--

instance (HashAlgorithm h, Typeable a) => ToCBOR (Hash h a) where
  toCBOR (UnsafeHash h) = toCBOR h

  -- | 'Size' expression for @Hash h a@, which is expressed using the 'ToCBOR'
  -- instance for 'ByteString' (as is the above 'toCBOR' method).  'Size'
  -- computation of length of the bytestring is passed as the first argument to
  -- 'encodedSizeExpr'.  The 'ByteString' instance will use it to calculate
  -- @'size' ('Proxy' @('LengthOf' 'ByteString'))@.
  --
  encodedSizeExpr _size proxy =
      encodedSizeExpr (const hashSize) (hashToBytes <$> proxy)
    where
      hashSize :: Size
      hashSize = fromIntegral (sizeHash (Proxy :: Proxy h))

instance (HashAlgorithm h, Typeable a) => FromCBOR (Hash h a) where
  fromCBOR = do
    bs <- decodeBytes
    case hashFromBytes bs of
      Just x  -> return x
      Nothing -> fail $ "hash bytes wrong size, expected " ++ show expected
                     ++ " but got " ++ show actual
        where
          expected = sizeHash (Proxy :: Proxy h)
          actual   = BS.length bs

data ViewHash32 h a where
  ViewHash32 :: SizeHash h ~ 32
    => Word64
    -> Word64
    -> Word64
    -> Word64
    -> ViewHash32 h a
  ViewHashNot32 :: ViewHash32 h a

viewHash32 :: forall h a. HashAlgorithm h => Hash h a -> ViewHash32 h a
viewHash32 (UnsafeHashRep p) = go p
  where
    go :: forall n. PackedBytes n -> ViewHash32 h a
    go (PackedBytes32 a b c d) =
      case sameNat (Proxy :: Proxy (SizeHash h)) (Proxy :: Proxy 32) of
        Just Refl -> ViewHash32 a b c d
        Nothing -> ViewHashNot32
    go _ = ViewHashNot32

unsafeMkHash32 ::
  SizeHash h ~ 32 => Word64 -> Word64 -> Word64 -> Word64 -> Hash h a
unsafeMkHash32 a b c d = UnsafeHashRep (PackedBytes32 a b c d)

--
-- Deprecated
--

{-# DEPRECATED hash "Use hashWith or hashWithSerialiser" #-}
hash :: forall h a. (HashAlgorithm h, ToCBOR a) => a -> Hash h a
hash = hashWithSerialiser toCBOR

{-# DEPRECATED fromHash "Use bytesToNatural . hashToBytes" #-}
fromHash :: Hash h a -> Natural
fromHash = foldl' f 0 . BS.unpack . hashToBytes
  where
    f :: Natural -> Word8 -> Natural
    f n b = n * 256 + fromIntegral b

{-# DEPRECATED hashRaw "Use hashWith" #-}
hashRaw :: forall h a. HashAlgorithm h => (a -> ByteString) -> a -> Hash h a
hashRaw = hashWith

{-# DEPRECATED getHash "Use hashToBytes" #-}
getHash :: Hash h a -> ByteString
getHash = hashToBytes

{-# DEPRECATED getHashBytesAsHex "Use hashToBytesAsHex" #-}
getHashBytesAsHex :: Hash h a -> ByteString
getHashBytesAsHex = hashToBytesAsHex

-- | XOR two hashes together
xor :: Hash h a -> Hash h a -> Hash h a
xor (UnsafeHashRep x) (UnsafeHashRep y) = UnsafeHashRep (xorPackedBytes x y)
{-# INLINE xor #-}
