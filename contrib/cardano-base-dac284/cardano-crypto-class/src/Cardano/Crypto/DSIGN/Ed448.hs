{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Ed448 digital signatures.
module Cardano.Crypto.DSIGN.Ed448
  ( Ed448DSIGN
  , SigDSIGN (..)
  , SignKeyDSIGN (..)
  , VerKeyDSIGN (..)
  )
where

import Control.DeepSeq (NFData)
import Data.ByteArray as BA (ByteArrayAccess, convert)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks, InspectHeap(..))

import Cardano.Binary (FromCBOR (..), ToCBOR (..))

import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Ed448 as Ed448

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.Seed
import Cardano.Crypto.Util (SignableRepresentation(..))


data Ed448DSIGN

instance DSIGNAlgorithm Ed448DSIGN where
    type SeedSizeDSIGN Ed448DSIGN = 57
    -- | Goldilocks points are 448 bits long
    type SizeVerKeyDSIGN  Ed448DSIGN = 57
    type SizeSignKeyDSIGN Ed448DSIGN = 57
    type SizeSigDSIGN     Ed448DSIGN = 114

    --
    -- Key and signature types
    --

    newtype VerKeyDSIGN Ed448DSIGN = VerKeyEd448DSIGN PublicKey
        deriving (Show, Eq, Generic, ByteArrayAccess)
        deriving newtype NFData
        deriving NoThunks via InspectHeap PublicKey

    newtype SignKeyDSIGN Ed448DSIGN = SignKeyEd448DSIGN SecretKey
        deriving (Show, Eq, Generic, ByteArrayAccess)
        deriving newtype NFData
        deriving NoThunks via InspectHeap SecretKey

    newtype SigDSIGN Ed448DSIGN = SigEd448DSIGN Signature
        deriving (Show, Eq, Generic, ByteArrayAccess)
        deriving NoThunks via InspectHeap Signature

    --
    -- Metadata and basic key operations
    --

    algorithmNameDSIGN _ = "ed448"

    deriveVerKeyDSIGN (SignKeyEd448DSIGN sk) = VerKeyEd448DSIGN $ toPublic sk


    --
    -- Core algorithm operations
    --

    type Signable Ed448DSIGN = SignableRepresentation

    signDSIGN () a (SignKeyEd448DSIGN sk) =
        let vk = toPublic sk
            bs = getSignableRepresentation a
         in SigEd448DSIGN $ sign sk vk bs

    verifyDSIGN () (VerKeyEd448DSIGN vk) a (SigEd448DSIGN sig) =
        if verify vk (getSignableRepresentation a) sig
          then Right ()
          else Left "Verification failed"

    --
    -- Key generation
    --

    genKeyDSIGN seed =
        let sk = runMonadRandomWithSeed seed Ed448.generateSecretKey
         in SignKeyEd448DSIGN sk

    --
    -- raw serialise/deserialise
    --

    rawSerialiseVerKeyDSIGN   = BA.convert
    rawSerialiseSignKeyDSIGN  = BA.convert
    rawSerialiseSigDSIGN      = BA.convert

    rawDeserialiseVerKeyDSIGN  = fmap VerKeyEd448DSIGN
                               . cryptoFailableToMaybe . Ed448.publicKey
    rawDeserialiseSignKeyDSIGN = fmap SignKeyEd448DSIGN
                               . cryptoFailableToMaybe . Ed448.secretKey
    rawDeserialiseSigDSIGN     = fmap SigEd448DSIGN
                               . cryptoFailableToMaybe . Ed448.signature


instance ToCBOR (VerKeyDSIGN Ed448DSIGN) where
  toCBOR = encodeVerKeyDSIGN
  encodedSizeExpr _ = encodedVerKeyDSIGNSizeExpr

instance FromCBOR (VerKeyDSIGN Ed448DSIGN) where
  fromCBOR = decodeVerKeyDSIGN

instance ToCBOR (SignKeyDSIGN Ed448DSIGN) where
  toCBOR = encodeSignKeyDSIGN
  encodedSizeExpr _ = encodedSignKeyDESIGNSizeExpr

instance FromCBOR (SignKeyDSIGN Ed448DSIGN) where
  fromCBOR = decodeSignKeyDSIGN

instance ToCBOR (SigDSIGN Ed448DSIGN) where
  toCBOR = encodeSigDSIGN
  encodedSizeExpr _ = encodedSigDSIGNSizeExpr

instance FromCBOR (SigDSIGN Ed448DSIGN) where
  fromCBOR = decodeSigDSIGN


cryptoFailableToMaybe :: CryptoFailable a -> Maybe a
cryptoFailableToMaybe (CryptoPassed a) = Just a
cryptoFailableToMaybe (CryptoFailed _) = Nothing

