{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.Scripts.Orphans where

import Ledger.Builtins.Orphans ()

import Codec.CBOR.Extras (SerialiseViaFlat (..))
import Data.Aeson.Extras qualified as JSON
import Data.OpenApi qualified as OpenApi
import Plutus.V1.Ledger.Scripts

import Data.Aeson (FromJSON (parseJSON), FromJSONKey, ToJSON (toJSON), ToJSONKey)
import Data.Aeson qualified as JSON

import Codec.Serialise (Serialise)
import Data.Hashable (Hashable)

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

deriving anyclass instance ToJSON DatumHash
deriving anyclass instance FromJSON DatumHash
deriving anyclass instance ToJSONKey DatumHash
deriving anyclass instance FromJSONKey DatumHash
deriving newtype instance Hashable DatumHash
deriving newtype instance Serialise DatumHash
deriving newtype instance OpenApi.ToSchema DatumHash

deriving anyclass instance ToJSON RedeemerHash
deriving anyclass instance FromJSON RedeemerHash
deriving anyclass instance ToJSONKey RedeemerHash
deriving anyclass instance FromJSONKey RedeemerHash
deriving newtype instance Hashable RedeemerHash
deriving newtype instance Serialise RedeemerHash

deriving anyclass instance ToJSON MintingPolicyHash
deriving anyclass instance FromJSON MintingPolicyHash
deriving anyclass instance ToJSONKey MintingPolicyHash
deriving anyclass instance FromJSONKey MintingPolicyHash
deriving newtype instance Hashable MintingPolicyHash
deriving newtype instance Serialise MintingPolicyHash

deriving anyclass instance ToJSON StakeValidatorHash
deriving anyclass instance FromJSON StakeValidatorHash
deriving anyclass instance ToJSONKey StakeValidatorHash
deriving anyclass instance FromJSONKey StakeValidatorHash
deriving newtype instance Hashable StakeValidatorHash
deriving newtype instance Serialise StakeValidatorHash

deriving anyclass instance ToJSON ScriptHash
deriving anyclass instance FromJSON ScriptHash
deriving anyclass instance ToJSONKey ScriptHash
deriving anyclass instance FromJSONKey ScriptHash
deriving newtype instance Hashable ScriptHash
deriving newtype instance Serialise ScriptHash

deriving anyclass instance ToJSON ValidatorHash
deriving anyclass instance FromJSON ValidatorHash
deriving anyclass instance ToJSONKey ValidatorHash
deriving anyclass instance FromJSONKey ValidatorHash
deriving newtype instance Hashable ValidatorHash
deriving newtype instance Serialise ValidatorHash

deriving newtype instance ToJSON Context
deriving newtype instance FromJSON Context

deriving anyclass instance ToJSON StakeValidator
deriving anyclass instance FromJSON StakeValidator

deriving anyclass instance ToJSON MintingPolicy
deriving anyclass instance FromJSON MintingPolicy

deriving anyclass instance ToJSON Validator
deriving anyclass instance FromJSON Validator

deriving anyclass instance ToJSON Redeemer
deriving anyclass instance FromJSON Redeemer
deriving anyclass instance Serialise Redeemer

deriving anyclass instance ToJSON Datum
deriving anyclass instance FromJSON Datum
deriving anyclass instance Serialise Datum
deriving newtype instance OpenApi.ToSchema Datum
