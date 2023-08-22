{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Orphans where

import Cardano.Api qualified as C
import Codec.Serialise.Class (Serialise (..))
import Data.Aeson qualified as JSON
import Data.Aeson.Extras qualified as JSON
import Data.Aeson.Types qualified as JSON
import Data.Bifunctor (bimap)
import Data.ByteArray qualified as BA
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Scientific (floatingOrInteger, scientific)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Ledger.Crypto (PrivateKey (PrivateKey, getPrivateKey))
import PlutusLedgerApi.V1 (LedgerBytes, POSIXTime (POSIXTime), TxId (TxId), fromBytes)
import PlutusLedgerApi.V1.Bytes (bytes)
import PlutusLedgerApi.V1.Scripts (ScriptError)
import Web.HttpApiData (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))

-- TODO: remove this dependency here once the instance of Ord for AddressInEra
-- can be obtained from upstream and removed from quickcheck-contractmodel.
import Test.QuickCheck.ContractModel.Internal.Common ()

instance ToHttpApiData PrivateKey where
    toUrlPiece = toUrlPiece . getPrivateKey

instance FromHttpApiData PrivateKey where
    parseUrlPiece a = PrivateKey <$> parseUrlPiece a

instance ToHttpApiData LedgerBytes where
    toUrlPiece = JSON.encodeByteString . bytes
instance FromHttpApiData LedgerBytes where
    parseUrlPiece = bimap Text.pack fromBytes . JSON.tryDecode

-- | ByteArrayAccess instance for signing support
instance BA.ByteArrayAccess TxId where
  length (TxId bis) = BA.length bis
  withByteArray (TxId bis) = BA.withByteArray bis

deriving instance Data C.NetworkMagic
deriving instance Data C.NetworkId
deriving instance Generic C.NetworkId

instance Serialise (C.AddressInEra C.BabbageEra) where
  encode = encode . C.serialiseToRawBytes
  decode = do
    bs <- decode
    maybe (fail "Can get back Address")
      pure
      $ C.deserialiseFromRawBytes (C.AsAddressInEra C.AsBabbageEra) bs

deriving instance Generic C.Lovelace
deriving instance Generic C.PolicyId
deriving instance Generic C.Quantity

-- 'POSIXTime' instances

-- | Custom `FromJSON` instance which allows to parse a JSON number to a
-- 'POSIXTime' value. The parsed JSON value MUST be an 'Integer' or else the
-- parsing fails.
instance JSON.FromJSON POSIXTime where
  parseJSON v@(JSON.Number n) =
      either (\_ -> JSON.prependFailure "parsing POSIXTime failed, " (JSON.typeMismatch "Integer" v))
             (return . POSIXTime)
             (floatingOrInteger n :: Either Double Integer)
  parseJSON invalid =
      JSON.prependFailure "parsing POSIXTime failed, " (JSON.typeMismatch "Number" invalid)

-- | Custom 'ToJSON' instance which allows to simply convert a 'POSIXTime'
-- value to a JSON number.
instance JSON.ToJSON POSIXTime where
  toJSON (POSIXTime n) = JSON.Number $ scientific n 0

deriving newtype instance Serialise POSIXTime
deriving newtype instance Hashable POSIXTime

deriving anyclass instance JSON.ToJSON ScriptError
deriving anyclass instance JSON.FromJSON ScriptError
