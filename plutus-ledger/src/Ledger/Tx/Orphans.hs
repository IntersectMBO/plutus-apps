{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Ledger.Tx.Orphans where

import Codec.CBOR.Extras (SerialiseViaFlat (..))
import Codec.Serialise (Serialise (decode, encode))
import Data.Aeson (FromJSON (parseJSON), FromJSONKey, ToJSON (toJSON), ToJSONKey, (.:))
import Data.Aeson qualified as JSON
import Data.Aeson.Extras qualified as JSON
import Data.ByteString qualified as BS
import Data.Hashable (Hashable)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as E
import PlutusCore.Data qualified as PLC

import Data.String (IsString (fromString))
import PlutusTx qualified as PlutusTx
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude qualified as PlutusTx

import Control.Newtype.Generics (Newtype)

import Plutus.V1.Ledger.Api

import Plutus.V1.Ledger.Bytes qualified as Bytes
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Tx
import Plutus.V1.Ledger.Value

deriving newtype instance Serialise LedgerBytes
deriving anyclass instance FromJSONKey LedgerBytes
deriving anyclass instance ToJSONKey LedgerBytes

instance ToJSON LedgerBytes where
    toJSON = JSON.String . JSON.encodeByteString . Bytes.bytes

instance FromJSON LedgerBytes where
    parseJSON v = Bytes.fromBytes <$> JSON.decodeByteString v

deriving anyclass instance ToJSON DatumHash
deriving anyclass instance FromJSON DatumHash
deriving anyclass instance ToJSONKey DatumHash
deriving anyclass instance FromJSONKey DatumHash
deriving newtype instance Hashable DatumHash
deriving newtype instance Serialise DatumHash

deriving anyclass instance ToJSON ValidatorHash
deriving anyclass instance FromJSON ValidatorHash
deriving anyclass instance ToJSONKey ValidatorHash
deriving anyclass instance FromJSONKey ValidatorHash
deriving newtype instance Hashable ValidatorHash
deriving newtype instance Serialise ValidatorHash

deriving anyclass instance ToJSON MintingPolicyHash
deriving anyclass instance FromJSON MintingPolicyHash
deriving anyclass instance ToJSONKey MintingPolicyHash
deriving anyclass instance FromJSONKey MintingPolicyHash
deriving newtype instance Hashable MintingPolicyHash
deriving newtype instance Serialise MintingPolicyHash

deriving anyclass instance ToJSON RedeemerPtr
deriving anyclass instance FromJSON RedeemerPtr
deriving anyclass instance ToJSONKey RedeemerPtr
deriving anyclass instance FromJSONKey RedeemerPtr
deriving anyclass instance Serialise RedeemerPtr

deriving anyclass instance ToJSON ScriptTag
deriving anyclass instance FromJSON ScriptTag
deriving anyclass instance Serialise ScriptTag

deriving anyclass instance ToJSON TxIn
deriving anyclass instance FromJSON TxIn
deriving anyclass instance Serialise TxIn

deriving anyclass instance ToJSON PubKeyHash
deriving anyclass instance FromJSON PubKeyHash
deriving anyclass instance FromJSONKey PubKeyHash
deriving anyclass instance ToJSONKey PubKeyHash
deriving anyclass instance Newtype PubKeyHash
deriving newtype instance Serialise PubKeyHash
deriving newtype instance Hashable PubKeyHash

deriving anyclass instance ToJSON Credential
deriving anyclass instance FromJSON Credential
deriving anyclass instance Hashable Credential
deriving anyclass instance Serialise Credential

deriving anyclass instance ToJSON StakingCredential
deriving anyclass instance FromJSON StakingCredential
deriving anyclass instance Hashable StakingCredential
deriving anyclass instance Serialise StakingCredential

deriving anyclass instance ToJSON Address
deriving anyclass instance FromJSON Address
deriving anyclass instance Serialise Address

deriving anyclass instance ToJSON MintingPolicy
deriving anyclass instance FromJSON MintingPolicy

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
        -- copied from 'Plutus.V1.Ledger.Value' because not exported
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

deriving anyclass instance Hashable TokenName
deriving newtype instance Serialise TokenName

deriving anyclass instance ToJSON TxOut
deriving anyclass instance FromJSON TxOut
deriving anyclass instance Serialise TxOut

deriving anyclass instance ToJSON TxInType
deriving anyclass instance FromJSON TxInType
deriving anyclass instance Serialise TxInType

{- Note [JSON instances for Script]
The JSON instances for Script are partially hand-written rather than going via the Serialise
instance directly. The reason for this is to *avoid* the size checks that are in place in the
Serialise instance. These are only useful for deserialisation checks on-chain, whereas the
JSON instances are used for e.g. transmitting validation events, which often include scripts
with the data arguments applied (which can be very big!).
-}

instance ToJSON Script where
    -- See note [JSON instances for Script]
    toJSON (Script p) = JSON.String $ JSON.encodeSerialise (SerialiseViaFlat p)

instance FromJSON Script where
    -- See note [JSON instances for Script]
    parseJSON v = do
        (SerialiseViaFlat p) <- JSON.decodeSerialise v
        return $ Script p

deriving anyclass instance ToJSON Validator
deriving anyclass instance FromJSON Validator

deriving anyclass instance ToJSON Datum
deriving anyclass instance FromJSON Datum
deriving anyclass instance Serialise Datum

deriving anyclass instance ToJSON Redeemer
deriving anyclass instance FromJSON Redeemer
deriving anyclass instance Serialise Redeemer

deriving via (JSON.JSONViaSerialise PLC.Data) instance ToJSON PLC.Data
deriving via (JSON.JSONViaSerialise PLC.Data) instance FromJSON PLC.Data

instance ToJSON PlutusTx.BuiltinData where
  toJSON = toJSON . PlutusTx.builtinDataToData

instance FromJSON PlutusTx.BuiltinData where
  parseJSON v = parseJSON v >>= pure . PlutusTx.dataToBuiltinData

instance Serialise PlutusTx.BuiltinData where
  encode = encode . PlutusTx.builtinDataToData
  decode = PlutusTx.dataToBuiltinData <$> decode

deriving anyclass instance ToJSON TxOutRef
deriving anyclass instance FromJSON TxOutRef
deriving anyclass instance ToJSONKey TxOutRef
deriving anyclass instance FromJSONKey TxOutRef
deriving anyclass instance Serialise TxOutRef

deriving anyclass instance ToJSON TxId
deriving anyclass instance FromJSON TxId
deriving anyclass instance ToJSONKey TxId
deriving anyclass instance FromJSONKey TxId
deriving anyclass instance Serialise TxId

instance ToJSON PlutusTx.BuiltinByteString where
    toJSON = JSON.String . JSON.encodeByteString . PlutusTx.fromBuiltin

instance FromJSON PlutusTx.BuiltinByteString where
    parseJSON v = PlutusTx.toBuiltin <$> JSON.decodeByteString v
