{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Ledger.Crypto
    ( module Export
    , PrivateKey
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
import Crypto.Hash qualified as Crypto
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.String
import Plutus.V1.Ledger.Api (LedgerBytes (LedgerBytes), TxId (TxId), fromBuiltin, toBuiltin)
import Plutus.V1.Ledger.Bytes qualified as KB
import Plutus.V1.Ledger.Crypto as Export hiding (PrivateKey)
import PlutusTx.Prelude qualified as P

type PrivateKey = Crypto.XPrv

-- | Passphrase newtype to mark intent
newtype Passphrase =
  Passphrase { unPassphrase :: BS.ByteString }
  deriving newtype (IsString)

instance Show Passphrase where
  show _ = "<passphrase>"

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
        xsig = either error id $ Crypto.xsignature (P.fromBuiltin s)
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
