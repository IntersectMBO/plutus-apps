{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

-- | Ed25519 digital signatures.
module Cardano.Crypto.DSIGN.Ed25519
  ( Ed25519DSIGN
  , SigDSIGN (..)
  , SignKeyDSIGN (..)
  , VerKeyDSIGN (..)
  )
where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import System.IO.Unsafe (unsafeDupablePerformIO)
import GHC.IO.Exception (ioException)
import Control.Monad (unless)
import Foreign.C.Error (errnoToIOError, getErrno)
import Foreign.Ptr (castPtr, nullPtr)
import qualified Data.ByteString as BS

import Cardano.Binary (FromCBOR (..), ToCBOR (..))

import Cardano.Foreign
import Cardano.Crypto.PinnedSizedBytes
import Cardano.Crypto.Libsodium.C

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.Seed
import Cardano.Crypto.Util (SignableRepresentation(..))
import Data.Proxy


data Ed25519DSIGN

instance NoThunks (VerKeyDSIGN Ed25519DSIGN)
instance NoThunks (SignKeyDSIGN Ed25519DSIGN)
instance NoThunks (SigDSIGN Ed25519DSIGN)

-- | Convert C-style return code / errno error reporting into Haskell
-- exceptions.
--
-- Runs an IO action (which should be some FFI call into C) that returns a
-- result code; if the result code returned is nonzero, fetch the errno, and
-- throw a suitable IO exception.
cOrError :: String -> String -> IO Int -> IO ()
cOrError contextDesc cFunName action = do
  res <- action
  unless (res == 0) $ do
      errno <- getErrno
      ioException $ errnoToIOError (contextDesc ++ ": " ++ cFunName) errno Nothing Nothing

instance DSIGNAlgorithm Ed25519DSIGN where
    -- | Seed size is 32 octets, the same as sign key size, because generating
    -- a sign key is literally just taking a chunk from the seed. We use
    -- SEEDBYTES to define both the seed size and the sign key size.
    type SeedSizeDSIGN Ed25519DSIGN = CRYPTO_SIGN_ED25519_SEEDBYTES
    -- | Ed25519 key size is 32 octets
    -- (per <https://tools.ietf.org/html/rfc8032#section-5.1.6>)
    type SizeVerKeyDSIGN  Ed25519DSIGN = CRYPTO_SIGN_ED25519_PUBLICKEYBYTES
    -- | Ed25519 secret key size is 32 octets; however, libsodium packs both
    -- the secret key and the public key into a 64-octet compound and exposes
    -- that as the secret key; the actual 32-octet secret key is called
    -- \"seed\" in libsodium. For backwards compatibility reasons and
    -- efficiency, we use the 64-octet compounds internally (this is what
    -- libsodium expects), but we only serialize the 32-octet secret key part
    -- (the libsodium \"seed\"). And because of this, we need to define the
    -- sign key size to be SEEDBYTES (which is 32), not PRIVATEKEYBYTES (which
    -- would be 64).
    type SizeSignKeyDSIGN Ed25519DSIGN = CRYPTO_SIGN_ED25519_SEEDBYTES
    -- | Ed25519 signature size is 64 octets
    type SizeSigDSIGN     Ed25519DSIGN = CRYPTO_SIGN_ED25519_BYTES

    --
    -- Key and signature types
    --

    newtype VerKeyDSIGN Ed25519DSIGN = VerKeyEd25519DSIGN (PinnedSizedBytes (SizeVerKeyDSIGN Ed25519DSIGN))
        deriving (Show, Eq, Generic)
        deriving newtype NFData

    -- Note that the size of the internal key data structure is the SECRET KEY
    -- bytes as per libsodium, while the declared key size (for serialization)
    -- is libsodium's SEED bytes. We expand 32-octet keys to 64-octet ones
    -- during deserialization, and we delete the 32 octets that contain the
    -- public key from the secret key before serializing.
    newtype SignKeyDSIGN Ed25519DSIGN = SignKeyEd25519DSIGN (PinnedSizedBytes CRYPTO_SIGN_ED25519_SECRETKEYBYTES)
        deriving (Show, Eq, Generic)
        deriving newtype NFData

    newtype SigDSIGN Ed25519DSIGN = SigEd25519DSIGN (PinnedSizedBytes (SizeSigDSIGN Ed25519DSIGN))
        deriving (Show, Eq, Generic)
        deriving newtype NFData

    --
    -- Metadata and basic key operations
    --

    algorithmNameDSIGN _ = "ed25519"

    deriveVerKeyDSIGN (SignKeyEd25519DSIGN sk) =
      VerKeyEd25519DSIGN $
        unsafeDupablePerformIO $
        psbUseAsSizedPtr sk $ \skPtr ->
        psbCreateSized $ \pkPtr ->
          cOrError "deriveVerKeyDSIGN @Ed25519DSIGN" "c_crypto_sign_ed25519_sk_to_pk"
            $ c_crypto_sign_ed25519_sk_to_pk pkPtr skPtr

    --
    -- Core algorithm operations
    --

    type Signable Ed25519DSIGN = SignableRepresentation

    signDSIGN () a (SignKeyEd25519DSIGN sk) =
      let bs = getSignableRepresentation a
      in SigEd25519DSIGN $ unsafeDupablePerformIO $
            BS.useAsCStringLen bs $ \(ptr, len) ->
            psbUseAsSizedPtr sk $ \skPtr ->
            allocaSized $ \pkPtr -> do
                cOrError "signDSIGN @Ed25519DSIGN" "c_crypto_sign_ed25519_sk_to_pk"
                  $ c_crypto_sign_ed25519_sk_to_pk pkPtr skPtr
                psbCreateSized $ \sigPtr -> do
                  cOrError "signDSIGN @Ed25519DSIGN" "c_crypto_sign_ed25519_detached"
                    $ c_crypto_sign_ed25519_detached sigPtr nullPtr (castPtr ptr) (fromIntegral len) skPtr

    verifyDSIGN () (VerKeyEd25519DSIGN vk) a (SigEd25519DSIGN sig) =
        let bs = getSignableRepresentation a
        in unsafeDupablePerformIO $
          BS.useAsCStringLen bs $ \(ptr, len) ->
          psbUseAsSizedPtr vk $ \vkPtr ->
          psbUseAsSizedPtr sig $ \sigPtr -> do
              res <- c_crypto_sign_ed25519_verify_detached sigPtr (castPtr ptr) (fromIntegral len) vkPtr
              if res == 0
              then return (Right ())
              else do
                  -- errno <- getErrno
                  return (Left  "Verification failed")

    --
    -- Key generation
    --

    genKeyDSIGN seed = SignKeyEd25519DSIGN $
      let (sb, _) = getBytesFromSeedT (seedSizeDSIGN (Proxy @Ed25519DSIGN)) seed
      in unsafeDupablePerformIO $ do
          psbCreateSized $ \skPtr ->
            BS.useAsCStringLen sb $ \(seedPtr, _) ->
            allocaSized $ \pkPtr -> do
                cOrError "genKeyDSIGN @Ed25519DSIGN" "c_crypto_sign_ed25519_seed_keypair"
                  $ c_crypto_sign_ed25519_seed_keypair pkPtr skPtr (SizedPtr . castPtr $ seedPtr)
    --
    -- raw serialise/deserialise
    --

    rawSerialiseVerKeyDSIGN   (VerKeyEd25519DSIGN vk) = psbToByteString vk
    rawSerialiseSignKeyDSIGN  (SignKeyEd25519DSIGN sk) =
        psbToByteString @(SeedSizeDSIGN Ed25519DSIGN) $ unsafeDupablePerformIO $ do
          let seed = psbZero
          psbUseAsSizedPtr sk $ \skPtr ->
            psbUseAsSizedPtr seed $ \seedPtr ->
              cOrError "deriveVerKeyDSIGN @Ed25519DSIGN" "c_crypto_sign_ed25519_sk_to_seed"
                $ c_crypto_sign_ed25519_sk_to_seed seedPtr skPtr
          return seed
    rawSerialiseSigDSIGN      (SigEd25519DSIGN sig) = psbToByteString sig

    rawDeserialiseVerKeyDSIGN  = fmap VerKeyEd25519DSIGN . psbFromByteStringCheck
    rawDeserialiseSignKeyDSIGN = Just . genKeyDSIGN . mkSeedFromBytes
    rawDeserialiseSigDSIGN     = fmap SigEd25519DSIGN . psbFromByteStringCheck


instance ToCBOR (VerKeyDSIGN Ed25519DSIGN) where
  toCBOR = encodeVerKeyDSIGN
  encodedSizeExpr _ = encodedVerKeyDSIGNSizeExpr

instance FromCBOR (VerKeyDSIGN Ed25519DSIGN) where
  fromCBOR = decodeVerKeyDSIGN

instance ToCBOR (SignKeyDSIGN Ed25519DSIGN) where
  toCBOR = encodeSignKeyDSIGN
  encodedSizeExpr _ = encodedSignKeyDESIGNSizeExpr

instance FromCBOR (SignKeyDSIGN Ed25519DSIGN) where
  fromCBOR = decodeSignKeyDSIGN

instance ToCBOR (SigDSIGN Ed25519DSIGN) where
  toCBOR = encodeSigDSIGN
  encodedSizeExpr _ = encodedSigDSIGNSizeExpr

instance FromCBOR (SigDSIGN Ed25519DSIGN) where
  fromCBOR = decodeSigDSIGN
