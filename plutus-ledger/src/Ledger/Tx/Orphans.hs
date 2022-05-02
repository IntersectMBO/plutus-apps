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
import Data.Hashable (Hashable)
import PlutusCore.Data qualified as PLC

import PlutusTx qualified as PlutusTx
import PlutusTx.Prelude qualified as PlutusTx

import Plutus.V1.Ledger.Api (LedgerBytes)

import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Tx
import Plutus.V1.Ledger.Value

deriving anyclass instance ToJSON DatumHash
deriving anyclass instance FromJSON DatumHash
deriving anyclass instance ToJSONKey DatumHash
deriving anyclass instance FromJSONKey DatumHash
deriving anyclass instance Hashable DatumHash
deriving via LedgerBytes instance Serialise DatumHash

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

deriving anyclass instance ToJSON Address
deriving anyclass instance FromJSON Address
deriving anyclass instance Serialise Address

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

deriving newtype instance Serialise CurrencySymbol

deriving anyclass instance ToJSON Value
deriving anyclass instance FromJSON Value
deriving anyclass instance Hashable Value
deriving newtype instance Serialise Value

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
