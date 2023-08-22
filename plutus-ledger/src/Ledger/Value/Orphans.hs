{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ledger.Value.Orphans where

import Cardano.Api qualified as C
import Codec.Serialise (Serialise (decode))
import Codec.Serialise.Class (Serialise (encode))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), (.:))
import Data.Aeson qualified as JSON
import Data.Aeson.Extras qualified as JSON
import Data.ByteString qualified as BS
import Data.Hashable (Hashable)
import Data.String (IsString (fromString))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as E
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Bytes qualified as Bytes
import PlutusLedgerApi.V1.Value
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter (Pretty (pretty), (<+>))
import Prettyprinter.Extras (PrettyShow (PrettyShow))
import Prettyprinter.Util (reflow)


instance ToJSON CurrencySymbol where
  toJSON c =
    JSON.object
      [ ( "unCurrencySymbol"
        , JSON.String .
          JSON.encodeByteString .
          PlutusTx.fromBuiltin .
          unCurrencySymbol $
          c)
      ]

instance FromJSON CurrencySymbol where
  parseJSON =
    JSON.withObject "CurrencySymbol" $ \object -> do
      raw <- object .: "unCurrencySymbol"
      bytes <- JSON.decodeByteString raw
      pure $ CurrencySymbol $ PlutusTx.toBuiltin bytes

deriving anyclass instance Hashable CurrencySymbol
deriving newtype instance Serialise CurrencySymbol

deriving anyclass instance Hashable TokenName
deriving newtype instance Serialise TokenName

{- note [Roundtripping token names]
How to properly roundtrip a token name that is not valid UTF-8 through PureScript
without a big rewrite of the API?
We prefix it with a zero byte so we can recognize it when we get a bytestring value back,
and we serialize it base16 encoded, with 0x in front so it will look as a hex string.
(Browsers don't render the zero byte.)
-}

instance ToJSON TokenName where
    toJSON = JSON.object . pure . (,) "unTokenName" . JSON.toJSON .
        fromTokenName
            (\bs -> Text.cons '\NUL' (asBase16 bs))
            (\t -> case Text.take 1 t of "\NUL" -> Text.concat ["\NUL\NUL", t]; _ -> t)
      where
        -- copied from 'PlutusLedgerApi.V1.Value' because not exported
        asBase16 :: BS.ByteString -> Text.Text
        asBase16 bs = Text.concat ["0x", Bytes.encodeByteString bs]

        fromTokenName :: (BS.ByteString -> r) -> (Text.Text -> r) -> TokenName -> r
        fromTokenName handleBytestring handleText (TokenName bs) = either (\_ -> handleBytestring $ PlutusTx.fromBuiltin bs) handleText $ E.decodeUtf8' (PlutusTx.fromBuiltin bs)

instance FromJSON TokenName where
    parseJSON =
        JSON.withObject "TokenName" $ \object -> do
        raw <- object .: "unTokenName"
        fromJSONText raw
        where
            fromText = tokenName . E.encodeUtf8 . Text.pack . fromString . Text.unpack
            fromJSONText t = case Text.take 3 t of
                "\NUL0x"       -> either fail (pure . tokenName) . JSON.tryDecode . Text.drop 3 $ t
                "\NUL\NUL\NUL" -> pure . fromText . Text.drop 2 $ t
                _              -> pure . fromText $ t

deriving anyclass instance ToJSON AssetClass
deriving anyclass instance FromJSON AssetClass
deriving anyclass instance Hashable AssetClass
deriving newtype instance Serialise AssetClass

deriving anyclass instance ToJSON Value
deriving anyclass instance FromJSON Value
deriving anyclass instance Hashable Value
deriving newtype instance Serialise Value

-- Orphan instances for 'PlutusTx.Map' to make this work
instance (ToJSON v, ToJSON k) => ToJSON (Map.Map k v) where
    toJSON = JSON.toJSON . Map.toList

instance (FromJSON v, FromJSON k) => FromJSON (Map.Map k v) where
    parseJSON v = Map.fromList <$> JSON.parseJSON v

deriving anyclass instance (Hashable k, Hashable v) => Hashable (Map.Map k v)
deriving anyclass instance (Serialise k, Serialise v) => Serialise (Map.Map k v)


instance Pretty C.Lovelace where
  pretty (C.Lovelace l) = pretty l <+> "lovelace"
deriving newtype instance Serialise C.Lovelace

deriving newtype instance Serialise C.Quantity

instance Pretty C.Value where
  pretty = reflow . C.renderValuePretty
instance Serialise C.Value where
  decode = C.valueFromList <$> decode
  encode = encode . C.valueToList

deriving stock instance Generic C.AssetId
deriving anyclass instance FromJSON C.AssetId
deriving anyclass instance ToJSON C.AssetId
deriving anyclass instance Serialise C.AssetId
deriving via (PrettyShow C.AssetId) instance Pretty C.AssetId

instance Serialise C.PolicyId where
  encode = encode . C.serialiseToRawBytes
  decode = do
    bs <- decode
    maybe (fail "Can get back policy ID")
      pure
      $ C.deserialiseFromRawBytes C.AsPolicyId bs

instance Serialise C.AssetName where
  encode = encode . C.serialiseToRawBytes
  decode = do
    bs <- decode
    maybe (fail "Can get back asset name")
      pure
      $ C.deserialiseFromRawBytes C.AsAssetName bs
