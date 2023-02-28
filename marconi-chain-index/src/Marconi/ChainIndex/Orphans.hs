{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.ChainIndex.Orphans where

import Cardano.Api qualified as C
import Cardano.Binary (fromCBOR, toCBOR)
import Codec.Serialise (Serialise (decode, encode))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy (toStrict)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text.Encoding qualified as Text
import Data.Typeable (Typeable)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField qualified as SQL
import Database.SQLite.Simple.Ok qualified as SQL
import Database.SQLite.Simple.ToField qualified as SQL
import Prettyprinter (Pretty (pretty), (<+>))

instance Pretty C.ChainTip where
  pretty C.ChainTipAtGenesis   = "ChainTipAtGenesis"
  pretty (C.ChainTip sn ha bn) = "ChainTip(" <> pretty sn <> "," <+> pretty ha <> "," <+> pretty bn <> ")"

instance Pretty C.ChainPoint where
  pretty C.ChainPointAtGenesis = "ChainPointAtGenesis"
  pretty (C.ChainPoint sn ha)  = "ChainPoint(" <> pretty sn <> "," <+> pretty ha <> ")"

instance Ord C.ChainPoint where
   C.ChainPointAtGenesis <= _                  = True
   _ <= C.ChainPointAtGenesis                  = False
   (C.ChainPoint sn _) <= (C.ChainPoint sn' _) = sn <= sn'

-- * C.Hash C.BlockHeader

instance Pretty (C.Hash C.BlockHeader) where
  pretty hash = "BlockHash" <+> pretty (C.serialiseToRawBytesHexText hash)

instance SQL.ToField (C.Hash C.BlockHeader) where
  toField f = SQL.toField $ C.serialiseToRawBytes f

instance SQL.FromField (C.Hash C.BlockHeader) where
   fromField f =
      SQL.fromField f <&>
        fromMaybe (error "Cannot deserialise C.Hash C.BlockHeader") .
          C.deserialiseFromRawBytes (C.proxyToAsType Proxy)

-- * C.SlotNo

instance Pretty C.SlotNo where
  pretty (C.SlotNo n) = "Slot" <+> pretty n

-- * C.BlockNo

instance Pretty C.BlockNo where
  pretty (C.BlockNo bn) = "BlockNo" <+> pretty bn

instance ToJSON C.BlockNo

instance FromJSON C.BlockNo

-- * C.AddressAny

instance SQL.FromField C.AddressAny where
  fromField f = SQL.fromField f >>= \b -> maybe
    cantDeserialise
    pure $ C.deserialiseFromRawBytes C.AsAddressAny
    b
    where
      cantDeserialise = SQL.returnError SQL.ConversionFailed f "Cannot deserialise address."

instance SQL.ToField C.AddressAny where
  toField = SQL.SQLBlob . C.serialiseToRawBytes

instance FromJSON C.AddressAny where
    parseJSON (Aeson.String v) =
        maybe mempty
              pure
              $ C.deserialiseAddress C.AsAddressAny v
    parseJSON _ = mempty

instance ToJSON C.AddressAny where
    toJSON = Aeson.String . C.serialiseAddress

-- * C.Hash C.ScriptData

instance SQL.FromField (C.Hash C.ScriptData) where
  fromField f = SQL.fromField f >>=
    maybe (SQL.returnError SQL.ConversionFailed f "Cannot deserialise C.Hash C.ScriptData.") pure
    . C.deserialiseFromRawBytes (C.AsHash C.AsScriptData)

instance SQL.ToField (C.Hash C.ScriptData) where
  toField = SQL.SQLBlob . C.serialiseToRawBytes

-- * C.ScriptData

instance Serialise C.ScriptData where
  encode = toCBOR
  decode = fromCBOR

instance SQL.FromField C.ScriptData where
  fromField f = SQL.fromField f >>=
    either (const $ SQL.returnError SQL.ConversionFailed f "Cannot deserialise C.ScriptData.") pure
    . C.deserialiseFromCBOR C.AsScriptData

instance SQL.ToField C.ScriptData where
  toField = SQL.SQLBlob . C.serialiseToCBOR

instance FromJSON C.ScriptData where
    parseJSON (Aeson.String v) =
        either (const mempty) pure $ do
            base16Val <- Base16.decode $ Text.encodeUtf8 v
            mapLeft show $ C.deserialiseFromCBOR C.AsScriptData base16Val
    parseJSON _ = mempty

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left v)  = Left $ f v
mapLeft _ (Right v) = Right v

instance ToJSON C.ScriptData where
    toJSON v = Aeson.String $ Text.decodeLatin1 $ Base16.encode $ C.serialiseToCBOR v

-- * C.TxIn

instance SQL.FromRow Int where
  fromRow = SQL.field

instance SQL.ToRow C.TxIn where
  toRow (C.TxIn txid txix) = SQL.toRow (txid, txix)

instance SQL.FromRow C.TxIn where
  fromRow = C.TxIn <$> SQL.field <*> SQL.field

instance SQL.FromField C.TxId where
  fromField f = SQL.fromField f >>= maybe
    (SQL.returnError SQL.ConversionFailed f "Cannot deserialise TxId.")
    pure . C.deserialiseFromRawBytes (C.proxyToAsType Proxy)

instance SQL.ToField C.TxId where
  toField = SQL.SQLBlob . C.serialiseToRawBytes

instance SQL.FromField C.TxIx where
  fromField = fmap C.TxIx . SQL.fromField

instance SQL.ToField C.TxIx where
  toField (C.TxIx i) = SQL.SQLInteger $ fromIntegral i

-- * C.Value

instance SQL.ToField C.Value where
  toField = SQL.SQLBlob . toStrict . Aeson.encode

instance SQL.FromField C.Value where
  fromField f = SQL.fromField f >>= either
    (const $ SQL.returnError SQL.ConversionFailed f "Cannot deserialise value.")
    pure . Aeson.eitherDecode

-- * C.ScriptInAnyLang

instance SQL.ToField C.ScriptInAnyLang where
  toField = SQL.SQLBlob . toStrict . Aeson.encode

instance SQL.FromField C.ScriptInAnyLang where
  fromField f = SQL.fromField f >>= either
    (const $ SQL.returnError SQL.ConversionFailed f "Cannot deserialise value.")
    pure . Aeson.eitherDecode

-- * C.ScriptHash

instance SQL.ToField C.ScriptHash where
  toField = SQL.SQLBlob . C.serialiseToRawBytesHex

instance SQL.FromField C.ScriptHash where
  fromField f = SQL.fromField f >>= either
    (const $ SQL.returnError SQL.ConversionFailed f "Cannot deserialise scriptDataHash.")
    pure . C.deserialiseFromRawBytesHex (C.proxyToAsType Proxy)

-- * ToField/FromField

deriving newtype instance SQL.ToField C.BlockNo
deriving newtype instance SQL.FromField C.BlockNo

deriving newtype instance SQL.ToField C.SlotNo
deriving newtype instance SQL.FromField C.SlotNo

deriving newtype instance SQL.ToField C.AssetName
deriving newtype instance SQL.FromField C.AssetName

deriving newtype instance SQL.ToField C.Quantity
deriving newtype instance SQL.FromField C.Quantity

instance SQL.ToField C.PolicyId where -- C.PolicyId is a newtype over C.ScriptHash but no ToField available for it.
  toField = SQL.toField . C.serialiseToRawBytes
instance SQL.FromField C.PolicyId where
  fromField = fromFieldViaRawBytes C.AsPolicyId

-- | Helper to deserialize via SerialiseAsRawBytes instance
fromFieldViaRawBytes :: (C.SerialiseAsRawBytes a, Typeable a) => C.AsType a -> SQL.Field -> SQL.Ok a
fromFieldViaRawBytes as f = maybe err pure . C.deserialiseFromRawBytes as =<< SQL.fromField f
  where err = SQL.returnError SQL.ConversionFailed f "can't deserialise via SerialiseAsRawBytes"
