{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Orphans where

import Cardano.Api qualified as C
import Cardano.Crypto.Hash qualified as Hash
import Cardano.Crypto.Wallet qualified as Crypto
import Cardano.Ledger.Crypto qualified as C
import Cardano.Ledger.Hashes qualified as Hashes
import Cardano.Ledger.SafeHash qualified as C
import Control.Lens ((&), (.~), (?~))
import Control.Monad.Freer.Extras.Log (LogLevel, LogMessage)
import Crypto.Hash qualified as Crypto
import Data.Aeson qualified as JSON
import Data.Aeson.Extras qualified as JSON
import Data.Bifunctor (bimap)
import Data.ByteArray qualified as BA
import Data.OpenApi qualified as OpenApi
import Data.Text qualified as Text
import Data.Typeable (Proxy (Proxy), Typeable)
import GHC.Exts (IsList (fromList))
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Ada (Ada (Lovelace))
import Plutus.V1.Ledger.Api (Address, BuiltinByteString, BuiltinData (BuiltinData), Credential,
                             CurrencySymbol (CurrencySymbol), Data, Datum (Datum), DatumHash (DatumHash), Extended,
                             Interval, LedgerBytes (LedgerBytes), LowerBound, MintingPolicy (MintingPolicy),
                             MintingPolicyHash (MintingPolicyHash), POSIXTime (POSIXTime), PubKeyHash (PubKeyHash),
                             Redeemer (Redeemer), RedeemerHash (RedeemerHash), Script, StakeValidator (StakeValidator),
                             StakeValidatorHash (StakeValidatorHash), StakingCredential, TokenName (TokenName),
                             TxId (TxId), TxOut, TxOutRef, UpperBound, Validator (Validator),
                             ValidatorHash (ValidatorHash), Value (Value), fromBytes)
import Plutus.V1.Ledger.Bytes (bytes)
import Plutus.V1.Ledger.Crypto (PrivateKey (PrivateKey, getPrivateKey), PubKey (PubKey), Signature (Signature))
import Plutus.V1.Ledger.Scripts (ScriptHash (..))
import Plutus.V1.Ledger.Slot (Slot (Slot))
import Plutus.V1.Ledger.Time (DiffMilliSeconds (DiffMilliSeconds))
import Plutus.V1.Ledger.Tx (RedeemerPtr, ScriptTag, Tx, TxIn, TxInType)
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))
import PlutusCore (Kind, Some, Term, Type, ValueOf, Version)
import PlutusTx.AssocMap qualified as AssocMap
import Web.HttpApiData (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))

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

-- | OpenApi instances for swagger support

instance OpenApi.ToSchema C.ScriptHash where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "ScriptHash") mempty
instance OpenApi.ToSchema (C.AddressInEra C.AlonzoEra) where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "AddressInAlonzoEra") mempty
deriving instance Generic C.ScriptData
instance OpenApi.ToSchema C.ScriptData where
    declareNamedSchema _ =
        pure $ OpenApi.NamedSchema (Just "ScriptData") OpenApi.byteSchema
instance OpenApi.ToSchema (C.Hash C.ScriptData) where
    declareNamedSchema _ =
        pure $ OpenApi.NamedSchema (Just "HashScriptData") OpenApi.byteSchema
deriving instance Generic C.TxId
deriving anyclass instance OpenApi.ToSchema C.TxId
deriving instance Generic C.TxIx
deriving anyclass instance OpenApi.ToSchema C.TxIx
deriving instance Generic C.Lovelace
deriving anyclass instance OpenApi.ToSchema C.Lovelace
deriving instance Generic C.PolicyId
deriving anyclass instance OpenApi.ToSchema C.PolicyId
instance OpenApi.ToSchema C.AssetName where
    declareNamedSchema _ =
        pure $ OpenApi.NamedSchema (Just "AssetName") OpenApi.byteSchema
deriving instance Generic C.Quantity
deriving anyclass instance OpenApi.ToSchema C.Quantity

deriving anyclass instance (OpenApi.ToSchema k, OpenApi.ToSchema v) => OpenApi.ToSchema (AssocMap.Map k v)
instance OpenApi.ToSchema BuiltinByteString where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "Bytes") mempty
instance OpenApi.ToSchema Crypto.XPub where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "PubKey") mempty
instance OpenApi.ToSchema Crypto.XPrv where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "PrvKey") mempty
instance OpenApi.ToSchema (Crypto.Digest Crypto.Blake2b_160) where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "Digest") mempty
instance OpenApi.ToSchema (Hash.Hash Hash.Blake2b_256 Hashes.EraIndependentTxBody) where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "Hash") mempty
instance OpenApi.ToSchema (C.SafeHash C.StandardCrypto Hashes.EraIndependentData) where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "Hash") mempty
deriving instance OpenApi.ToSchema (LogMessage JSON.Value)
deriving instance OpenApi.ToSchema LogLevel
instance OpenApi.ToSchema JSON.Value where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "JSON") mempty
instance OpenApi.ToSchema Data where
  declareNamedSchema _ = do
    integerSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy Integer)
    constrArgsSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy (Integer, [Data]))
    mapArgsSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy [(Data, Data)])
    listArgsSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy [Data])
    bytestringSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy String)
    return $ OpenApi.NamedSchema (Just "Data") $ mempty
      & OpenApi.type_ ?~ OpenApi.OpenApiObject
      & OpenApi.properties .~
          fromList
          [ ("Constr", constrArgsSchema)
          , ("Map", mapArgsSchema)
          , ("List", listArgsSchema)
          , ("I", integerSchema)
          , ("B", bytestringSchema)
          ]
deriving instance OpenApi.ToSchema ann => OpenApi.ToSchema (Kind ann)
deriving newtype instance OpenApi.ToSchema Ada
deriving instance OpenApi.ToSchema Tx
deriving instance OpenApi.ToSchema ScriptTag
deriving instance OpenApi.ToSchema RedeemerPtr
deriving instance OpenApi.ToSchema TxOutRef
deriving instance OpenApi.ToSchema TxInType
deriving instance OpenApi.ToSchema TxIn
deriving instance OpenApi.ToSchema TxOut
deriving newtype instance OpenApi.ToSchema Validator
deriving newtype instance OpenApi.ToSchema TxId
deriving newtype instance OpenApi.ToSchema Slot
deriving instance OpenApi.ToSchema a => OpenApi.ToSchema (Interval a)
deriving instance OpenApi.ToSchema a => OpenApi.ToSchema (LowerBound a)
deriving instance OpenApi.ToSchema a => OpenApi.ToSchema (UpperBound a)
deriving newtype instance OpenApi.ToSchema Redeemer
deriving newtype instance OpenApi.ToSchema RedeemerHash
deriving newtype instance OpenApi.ToSchema Datum
deriving newtype instance OpenApi.ToSchema Value
deriving instance OpenApi.ToSchema Address
deriving newtype instance OpenApi.ToSchema MintingPolicy
deriving newtype instance OpenApi.ToSchema MintingPolicyHash
deriving newtype instance OpenApi.ToSchema DatumHash
deriving newtype instance OpenApi.ToSchema CurrencySymbol
deriving instance OpenApi.ToSchema Credential
deriving newtype instance OpenApi.ToSchema PubKey
deriving newtype instance OpenApi.ToSchema TokenName
deriving instance OpenApi.ToSchema StakingCredential
deriving newtype instance OpenApi.ToSchema StakeValidator
deriving newtype instance OpenApi.ToSchema StakeValidatorHash
deriving newtype instance OpenApi.ToSchema PubKeyHash
deriving newtype instance OpenApi.ToSchema LedgerBytes
deriving newtype instance OpenApi.ToSchema ValidatorHash
deriving newtype instance OpenApi.ToSchema Signature
deriving newtype instance OpenApi.ToSchema POSIXTime
deriving newtype instance OpenApi.ToSchema DiffMilliSeconds
deriving newtype instance OpenApi.ToSchema BuiltinData
deriving newtype instance OpenApi.ToSchema AssetClass
deriving instance OpenApi.ToSchema a => OpenApi.ToSchema (Extended a)
deriving instance
    ( OpenApi.ToSchema tyname
    , OpenApi.ToSchema name
    , OpenApi.ToSchema (uni ann)
    , OpenApi.ToSchema fun
    , OpenApi.ToSchema ann
    , OpenApi.ToSchema (Type tyname uni ann)
    , OpenApi.ToSchema (Some (ValueOf uni))
    , Typeable uni
    ) => OpenApi.ToSchema (Term tyname name uni fun ann)
deriving instance OpenApi.ToSchema ann => OpenApi.ToSchema (Version ann)
instance OpenApi.ToSchema Script where
    declareNamedSchema _ =
        pure $ OpenApi.NamedSchema (Just "Script") (OpenApi.toSchema (Proxy :: Proxy String))
deriving newtype instance OpenApi.ToSchema ScriptHash
