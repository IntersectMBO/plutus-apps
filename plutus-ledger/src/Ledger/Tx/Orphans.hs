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

import Codec.Serialise (Serialise (decode, encode))
import Data.Aeson (FromJSON (parseJSON), FromJSONKey, ToJSON (toJSON), ToJSONKey)
import Data.Aeson qualified as JSON
import Data.Aeson.Extras qualified as JSON
import GHC.Generics (Generic)
import PlutusCore.Data qualified as PLC

import PlutusTx qualified as PlutusTx
import PlutusTx.Prelude qualified as PlutusTx

import Plutus.V1.Ledger.Interval
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Tx
import Plutus.V1.Ledger.Value


deriving anyclass instance ToJSON DatumHash
deriving anyclass instance FromJSON DatumHash
deriving anyclass instance ToJSONKey DatumHash
deriving anyclass instance FromJSONKey DatumHash

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

deriving anyclass instance ToJSON TxInType
deriving anyclass instance FromJSON TxInType
deriving anyclass instance Serialise TxInType

deriving anyclass instance ToJSON Redeemer
deriving anyclass instance FromJSON Redeemer
deriving anyclass instance Serialise Redeemer

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
