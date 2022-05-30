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


module Ledger.Tx.Orphans.V1 where

import Ledger.Scripts.Orphans ()

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON (parseJSON), FromJSONKey, ToJSON (toJSON), ToJSONKey)
import Data.Aeson qualified as JSON
import Data.Aeson.Extras qualified as JSON
import Ledger.Credential.Orphans ()

import Ledger.Address.Orphans ()
import Ledger.Builtins.Orphans ()
import Ledger.Value.Orphans ()

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Bytes qualified as Bytes
import Plutus.V1.Ledger.Tx

deriving newtype instance Serialise LedgerBytes
deriving anyclass instance FromJSONKey LedgerBytes
deriving anyclass instance ToJSONKey LedgerBytes

instance ToJSON LedgerBytes where
    toJSON = JSON.String . JSON.encodeByteString . Bytes.bytes

instance FromJSON LedgerBytes where
    parseJSON v = Bytes.fromBytes <$> JSON.decodeByteString v

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

deriving anyclass instance ToJSON TxOut
deriving anyclass instance FromJSON TxOut
deriving anyclass instance Serialise TxOut

deriving anyclass instance ToJSON TxInType
deriving anyclass instance FromJSON TxInType
deriving anyclass instance Serialise TxInType

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
