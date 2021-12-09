{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UnboxedTuples              #-}

-- for pinnedByteArrayFromListN
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Cardano.Crypto.PinnedSizedBytes
  (
    PinnedSizedBytes,
    -- * Initialization
    psbZero,
    -- * Conversions
    psbFromBytes,
    psbToBytes,
    psbFromByteString,
    psbFromByteStringCheck,
    psbToByteString,
    -- * C usage
    psbUseAsCPtr,
    psbUseAsSizedPtr,
    psbCreate,
    psbCreateSized,
    ptrPsbToSizedPtr,
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.ST (runST)
import Control.Monad.Primitive  (primitive_, touch)
import Data.Char (ord)
import Data.Primitive.ByteArray
          ( ByteArray (..)
          , MutableByteArray (..)
          , copyByteArrayToAddr
          , newPinnedByteArray
          , unsafeFreezeByteArray
          , foldrByteArray
          , byteArrayContents
          , writeByteArray
          , mutableByteArrayContents
          )
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Word (Word8)
import Foreign.C.Types (CSize)
import Foreign.Ptr (FunPtr, castPtr)
import Foreign.Storable (Storable (..))
import GHC.TypeLits (KnownNat, Nat, natVal)
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import Numeric (showHex)
import System.IO.Unsafe (unsafeDupablePerformIO)

import GHC.Exts (Int (..))
import GHC.Prim (copyAddrToByteArray#)
import GHC.Ptr (Ptr (..))

import qualified Data.Primitive as Prim
import qualified Data.ByteString as BS

import Cardano.Foreign
import Cardano.Crypto.Libsodium.C (c_sodium_compare)

{- HLINT ignore "Reduce duplication" -}

-- $setup
-- >>> :set -XDataKinds -XTypeApplications -XOverloadedStrings
-- >>> import Cardano.Crypto.PinnedSizedBytes

-- | @n@ bytes. 'Storable'.
--
-- We have two @*Bytes@ types:
--
-- * @PinnedSizedBytes@ is backed by pinned ByteArray.
-- * @MLockedSizedBytes@ is backed by ForeignPtr to @mlock@-ed memory region.
--
-- The 'ByteString' is pinned datatype, but it's represented by
-- 'ForeignPtr' + offset (and size).
--
-- I'm sorry for adding more types for bytes. :(
--
newtype PinnedSizedBytes (n :: Nat) = PSB ByteArray
  deriving NoThunks via OnlyCheckWhnfNamed "PinnedSizedBytes" (PinnedSizedBytes n)
  deriving NFData

instance Show (PinnedSizedBytes n) where
    showsPrec _ (PSB ba)
        = showChar '"'
        . foldrByteArray (\w acc -> show8 w . acc) id ba
        . showChar '"'
      where
        show8 :: Word8 -> ShowS
        show8 w | w < 16    = showChar '0' . showHex w
                | otherwise = showHex w

-- | The comparison is done in constant time for a given size @n@.
instance KnownNat n => Eq (PinnedSizedBytes n) where
    x == y = compare x y == EQ

instance KnownNat n => Ord (PinnedSizedBytes n) where
    compare x y =
        unsafeDupablePerformIO $
            psbUseAsCPtr x $ \xPtr ->
                psbUseAsCPtr y $ \yPtr -> do
                    res <- c_sodium_compare xPtr yPtr size
                    return (compare res 0)
      where
        size :: CSize
        size = fromInteger (natVal (Proxy :: Proxy n))

-- |
--
-- If given 'String' is too long, it is truncated,
-- If it is too short, it is padded with zeros.
--
-- Padding and truncation make it behave like an integer mod @n*8@.
--
-- >>> "abcdef" :: PinnedSizedBytes 4
-- "63646566"
--
-- >>> "foo" :: PinnedSizedBytes 8
-- "0000000000666f6f"
--
-- Non-ASCII codepoints are silently truncated to 0..255 range.
--
-- >>> "\x1234\x5678" :: PinnedSizedBytes 2
-- "3478"
--
-- 'PinnedSizedBytes' created with 'fromString' contains /unpinned/
-- 'ByteArray'.
--
instance KnownNat n => IsString (PinnedSizedBytes n) where
    fromString s = psbFromBytes (map (fromIntegral . ord) s)

-- | See 'psbFromBytes'.
psbToBytes :: PinnedSizedBytes n -> [Word8]
psbToBytes (PSB ba) = foldrByteArray (:) [] ba

psbToByteString :: PinnedSizedBytes n -> BS.ByteString
psbToByteString = BS.pack . psbToBytes

-- | See @'IsString' ('PinnedSizedBytes' n)@ instance.
--
-- >>> psbToBytes . (id @(PinnedSizedBytes 4)) . psbFromBytes $ [1,2,3,4]
-- [1,2,3,4]
--
-- >>> psbToBytes . (id @(PinnedSizedBytes 4)) . psbFromBytes $ [1,2]
-- [0,0,1,2]
--
-- >>> psbToBytes . (id @(PinnedSizedBytes 4)) . psbFromBytes $ [1,2,3,4,5,6]
-- [3,4,5,6]
-- 
psbFromBytes :: forall n. KnownNat n => [Word8] -> PinnedSizedBytes n
psbFromBytes ws0 = PSB (pinnedByteArrayFromListN size ws)
  where
    size :: Int
    size = fromInteger (natVal (Proxy :: Proxy n))

    ws :: [Word8]
    ws = reverse
        $ take size
        $ (++ repeat 0)
        $ reverse ws0

-- This is not efficient, but we don't use this in non-tests
psbFromByteString :: KnownNat n => BS.ByteString -> PinnedSizedBytes n
psbFromByteString = psbFromBytes . BS.unpack

psbFromByteStringCheck :: forall n. KnownNat n => BS.ByteString -> Maybe (PinnedSizedBytes n)
psbFromByteStringCheck bs 
    | BS.length bs == size = Just $ unsafeDupablePerformIO $
        BS.useAsCStringLen bs $ \(Ptr addr#, _) -> do
            marr@(MutableByteArray marr#) <- newPinnedByteArray size
            primitive_ $ copyAddrToByteArray# addr# marr# 0# (case size of I# s -> s)
            arr <- unsafeFreezeByteArray marr
            return (PSB arr)
    | otherwise            = Nothing
  where
    size :: Int
    size = fromInteger (natVal (Proxy :: Proxy n))

psbZero :: KnownNat n =>  PinnedSizedBytes n
psbZero = psbFromBytes []

instance KnownNat n => Storable (PinnedSizedBytes n) where
    sizeOf _          = fromInteger (natVal (Proxy :: Proxy n))
    alignment _       = alignment (undefined :: FunPtr (Int -> Int))

    peek (Ptr addr#) = do
        let size :: Int
            size = fromInteger (natVal (Proxy :: Proxy n))
        marr@(MutableByteArray marr#) <- newPinnedByteArray size
        primitive_ $ copyAddrToByteArray# addr# marr# 0# (case size of I# s -> s)
        arr <- unsafeFreezeByteArray marr
        return (PSB arr)

    poke p (PSB arr) = do
        let size :: Int
            size = fromInteger (natVal (Proxy :: Proxy n))
        copyByteArrayToAddr (castPtr p) arr 0 size

psbUseAsCPtr :: PinnedSizedBytes n -> (Ptr Word8 -> IO r) -> IO r
psbUseAsCPtr (PSB ba) k = do
    r <- k (byteArrayContents ba)
    r <$ touch ba

psbUseAsSizedPtr :: PinnedSizedBytes n -> (SizedPtr n -> IO r) -> IO r
psbUseAsSizedPtr (PSB ba) k = do
    r <- k (SizedPtr $ castPtr $ byteArrayContents ba)
    r <$ touch ba

psbCreate :: forall n. KnownNat n => (Ptr Word8 -> IO ()) -> IO (PinnedSizedBytes n)
psbCreate k = do
    let size :: Int
        size = fromInteger (natVal (Proxy :: Proxy n))
    mba <- newPinnedByteArray size
    k (mutableByteArrayContents mba)
    arr <- unsafeFreezeByteArray mba
    return (PSB arr)

psbCreateSized :: forall n. KnownNat n => (SizedPtr n -> IO ()) -> IO (PinnedSizedBytes n)
psbCreateSized k = psbCreate (k . SizedPtr . castPtr)

ptrPsbToSizedPtr :: Ptr (PinnedSizedBytes n) -> SizedPtr n
ptrPsbToSizedPtr = SizedPtr . castPtr

-------------------------------------------------------------------------------
-- derivative from primitive
-------------------------------------------------------------------------------

-- | Create a 'ByteArray' from a list of a known length. If the length
--   of the list does not match the given length, or if the length is zero,
--   then this throws an exception.
pinnedByteArrayFromListN :: forall a. Prim.Prim a => Int -> [a] -> ByteArray
pinnedByteArrayFromListN 0 _ =
    die "pinnedByteArrayFromListN" "list length zero"
pinnedByteArrayFromListN n ys = runST $ do
    marr <- newPinnedByteArray (n * Prim.sizeOf (head ys))
    let go !ix [] = if ix == n
          then return ()
          else die "pinnedByteArrayFromListN" "list length less than specified size"
        go !ix (x : xs) = if ix < n
          then do
            writeByteArray marr ix x
            go (ix + 1) xs
          else die "pinnedByteArrayFromListN" "list length greater than specified size"
    go 0 ys
    unsafeFreezeByteArray marr

die :: String -> String -> a
die fun problem = error $ "PinnedSizedBytes." ++ fun ++ ": " ++ problem
