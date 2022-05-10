{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.Crypto
    ( module Export
    , PubKey(..)
    , PrivateKey(..)
    , Signature(..)
    , Passphrase(..)
    , pubKeyHash
    , signedBy
    , sign
    , signTx
    , generateFromSeed
    , toPublicKey
    , xPubToPublicKey
    -- * Signing and generation with no passphrase
    , sign'
    , signTx'
    , generateFromSeed'
    ) where

import Cardano.Crypto.Wallet qualified as Crypto
import Codec.Serialise.Class (Serialise)
import Control.DeepSeq (NFData)
import Control.Newtype.Generics (Newtype)
import Crypto.Hash qualified as Crypto
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, (.:))
import Data.Aeson qualified as JSON
import Data.Aeson.Extras qualified as JSON
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.Hashable (Hashable)
import Data.String
import GHC.Generics (Generic)
import Ledger.Tx.Orphans ()
import Plutus.V1.Ledger.Api (LedgerBytes (LedgerBytes), TxId (TxId), fromBuiltin, toBuiltin)
import Plutus.V1.Ledger.Bytes qualified as KB
import Plutus.V1.Ledger.Crypto as Export
import PlutusTx qualified as PlutusTx
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter (Pretty)

-- | Passphrase newtype to mark intent
newtype Passphrase =
  Passphrase { unPassphrase :: BS.ByteString }
  deriving newtype (IsString)

instance Show Passphrase where
  show _ = "<passphrase>"

-- | A message with a cryptographic signature.
newtype Signature = Signature { getSignature :: PlutusTx.BuiltinByteString }
    deriving stock (Eq, Ord, Generic)
    deriving newtype (PlutusTx.Eq, PlutusTx.Ord, Serialise, NFData, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving (Show, Pretty) via LedgerBytes
makeLift ''Signature

instance ToJSON Signature where
  toJSON signature =
    JSON.object
      [ ( "getSignature"
        , JSON.String .
          JSON.encodeByteString .
          PlutusTx.fromBuiltin .
          getSignature $
          signature)
      ]

instance FromJSON Signature where
  parseJSON =
    JSON.withObject "Signature" $ \object -> do
      raw <- object .: "getSignature"
      bytes <- JSON.decodeByteString raw
      pure . Signature $ PlutusTx.toBuiltin $ bytes

newtype PubKey = PubKey { getPubKey :: LedgerBytes }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (Newtype, ToJSON, FromJSON, NFData)
    deriving newtype (PlutusTx.Eq, PlutusTx.Ord, Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving IsString via LedgerBytes
    deriving (Show, Pretty) via LedgerBytes
makeLift ''PubKey

instance ToJSONKey PubKey where
  toJSONKey = JSON.ToJSONKeyValue (JSON.genericToJSON JSON.defaultOptions) JSON.toEncoding

instance FromJSONKey PubKey where
  fromJSONKey = JSON.FromJSONKeyValue (JSON.genericParseJSON JSON.defaultOptions)

-- | A cryptographic private key.
newtype PrivateKey = PrivateKey { getPrivateKey :: LedgerBytes }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Newtype, ToJSONKey, FromJSONKey)
    deriving newtype (PlutusTx.Eq, PlutusTx.Ord, Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving (Show, Pretty) via LedgerBytes
    deriving Hashable via PlutusTx.BuiltinByteString
makeLift ''PrivateKey

-- | Compute the hash of a public key.
pubKeyHash :: PubKey -> PubKeyHash
pubKeyHash (PubKey (LedgerBytes bs)) =
    PubKeyHash
      $ toBuiltin
      $ BA.convert @_ @BS.ByteString
      $ Crypto.hashWith Crypto.Blake2b_224 (fromBuiltin bs)

-- | Check whether the given 'Signature' was signed by the private key corresponding to the given public key.
signedBy :: BA.ByteArrayAccess a => Signature -> PubKey -> a -> Bool
signedBy (Signature s) (PubKey k) payload =
    let xpub = Crypto.XPub (KB.bytes k) (Crypto.ChainCode "" {- value is ignored -})
        xsig = either error id $ Crypto.xsignature (PlutusTx.fromBuiltin s)
    in Crypto.verify xpub payload xsig

-- | Sign the hash of a transaction using a private key and passphrase.
signTx :: TxId -> Crypto.XPrv -> Passphrase -> Signature
signTx (TxId txId) = sign txId

-- | Sign the hash of a transaction using a private key that has no passphrase.
signTx' :: TxId -> Crypto.XPrv -> Signature
signTx' txId xprv = signTx txId xprv noPassphrase

-- | Sign a message using a private key and passphrase.
sign :: BA.ByteArrayAccess a => a -> Crypto.XPrv -> Passphrase -> Signature
sign msg privKey (Passphrase passPhrase) = Signature . toBuiltin . Crypto.unXSignature $ Crypto.sign passPhrase privKey msg

-- | Sign a message using a private key with no passphrase.
sign' :: BA.ByteArrayAccess a => a -> Crypto.XPrv -> Signature
sign' msg privKey = sign msg privKey noPassphrase

-- | Generate a private key from a seed phrase and passphrase
generateFromSeed :: BS.ByteString -> Passphrase -> Crypto.XPrv
generateFromSeed seed (Passphrase passPhrase) = Crypto.generate seed passPhrase

-- | Generate a private key from a seed phrase without a passphrase.
generateFromSeed' :: BS.ByteString -> Crypto.XPrv
generateFromSeed' seed = generateFromSeed seed noPassphrase

noPassphrase :: Passphrase
noPassphrase = Passphrase ""

xPubToPublicKey :: Crypto.XPub -> PubKey
xPubToPublicKey = PubKey . KB.fromBytes . Crypto.xpubPublicKey

toPublicKey :: Crypto.XPrv -> PubKey
toPublicKey = xPubToPublicKey . Crypto.toXPub
