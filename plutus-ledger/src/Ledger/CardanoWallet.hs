{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

{- Cardano wallet implementation for the emulator.
-}
module Ledger.CardanoWallet(
    MockWallet(..),
    -- * Enumerating wallets
    WalletNumber(..),
    fromWalletNumber,
    toWalletNumber,
    knownMockWallets,
    knownMockWallet,
    fromSeed,
    fromSeed',
    -- ** Keys
    mockWalletAddress,
    paymentPrivateKey,
    paymentPubKeyHash,
    paymentPubKey,
    stakePubKeyHash,
    stakePubKey
    ) where

import Cardano.Crypto.Wallet qualified as Crypto
import Codec.Serialise (serialise)
import Crypto.Hash qualified as Crypto
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Extras (encodeByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Hashable (Hashable (..))
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Ledger (PaymentPrivateKey (PaymentPrivateKey), PaymentPubKey (PaymentPubKey, unPaymentPubKey),
               PaymentPubKeyHash (PaymentPubKeyHash), StakePubKeyHash)
import Ledger.Address (PaymentPubKeyHash (unPaymentPubKeyHash), StakePubKey (StakePubKey, unStakePubKey),
                       StakePubKeyHash (StakePubKeyHash, unStakePubKeyHash))
import Ledger.Crypto (PubKey (..))
import Ledger.Crypto qualified as Crypto
import Plutus.V1.Ledger.Api (Address (Address), Credential (PubKeyCredential), StakingCredential (StakingHash))
import Plutus.V1.Ledger.Bytes (LedgerBytes (getLedgerBytes))
import Servant.API (FromHttpApiData, ToHttpApiData)

newtype MockPrivateKey = MockPrivateKey { unMockPrivateKey :: Crypto.XPrv }

instance Show MockPrivateKey where
    show = T.unpack . encodeByteString . Crypto.unXPrv . unMockPrivateKey
instance Eq MockPrivateKey where
    (MockPrivateKey l) == (MockPrivateKey r) = Crypto.unXPrv l == Crypto.unXPrv r
instance Ord MockPrivateKey where
    compare (MockPrivateKey l) (MockPrivateKey r) = compare (Crypto.unXPrv l) (Crypto.unXPrv r)
instance Hashable MockPrivateKey where
    hashWithSalt i = hashWithSalt i . Crypto.unXPrv . unMockPrivateKey

-- | Emulated wallet with a key and a passphrase
data MockWallet =
    MockWallet
        { mwWalletId   :: Crypto.Digest Crypto.Blake2b_160
        , mwPaymentKey :: MockPrivateKey
        , mwStakeKey   :: Maybe MockPrivateKey
        , mwPrintAs    :: Maybe String
        } deriving Show

-- | Wrapper for config files and APIs
newtype WalletNumber = WalletNumber { getWallet :: Integer }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToHttpApiData, FromHttpApiData)
    deriving anyclass (FromJSON, ToJSON)

fromWalletNumber :: WalletNumber -> MockWallet
fromWalletNumber (WalletNumber i) = (fromSeed' (BSL.toStrict $ serialise i)) { mwPrintAs = Just (show i) }

fromSeed :: BS.ByteString -> Crypto.Passphrase -> MockWallet
fromSeed bs passPhrase = fromSeedInternal (flip Crypto.generateFromSeed passPhrase) bs

fromSeed' :: BS.ByteString -> MockWallet
fromSeed' = fromSeedInternal Crypto.generateFromSeed'

fromSeedInternal :: (BS.ByteString -> Crypto.XPrv) -> BS.ByteString -> MockWallet
fromSeedInternal seedGen bs = MockWallet{mwWalletId, mwPaymentKey, mwStakeKey, mwPrintAs = Nothing} where
    missing = max 0 (32 - BS.length bs)
    bs' = bs <> BS.replicate missing 0
    k = seedGen bs'
    mwWalletId =
        fromMaybe (error "Ledger.CardanoWallet.fromSeed: digestFromByteString")
        $ Crypto.digestFromByteString
        $ Crypto.hashWith Crypto.Blake2b_160
        $ getLedgerBytes
        $ getPubKey
        $ Crypto.toPublicKey k
    mwPaymentKey = MockPrivateKey k
    mwStakeKey = Nothing

toWalletNumber :: MockWallet -> WalletNumber
toWalletNumber MockWallet{mwWalletId=w} =
    maybe (error "Ledger.CardanoWallet.toWalletNumber: not a known wallet")
          (WalletNumber . toInteger . succ)
          $ findIndex ((==) w . mwWalletId) knownMockWallets

-- | The wallets used in mockchain simulations by default. There are
--   ten wallets by default.
knownMockWallets :: [MockWallet]
knownMockWallets = fromWalletNumber . WalletNumber <$> [1..10]

-- | Get a known wallet from an @Integer@ indexed from 1 to 10.
knownMockWallet :: Integer -> MockWallet
knownMockWallet = (knownMockWallets !!) . pred . fromInteger

mockWalletAddress :: MockWallet -> Address
mockWalletAddress mw =
    Address (PubKeyCredential $ unPaymentPubKeyHash $ paymentPubKeyHash mw)
            (StakingHash . PubKeyCredential . unStakePubKeyHash <$> stakePubKeyHash mw)

-- | Mock wallet's private key
paymentPrivateKey :: MockWallet -> PaymentPrivateKey
paymentPrivateKey = PaymentPrivateKey . unMockPrivateKey . mwPaymentKey

-- | The mock wallet's public key hash
paymentPubKeyHash :: MockWallet -> PaymentPubKeyHash
paymentPubKeyHash = PaymentPubKeyHash . Crypto.pubKeyHash . unPaymentPubKey . paymentPubKey

-- | The mock wallet's payment public key
paymentPubKey :: MockWallet -> PaymentPubKey
paymentPubKey = PaymentPubKey . Crypto.toPublicKey . unMockPrivateKey . mwPaymentKey

-- | The mock wallet's stake public key hash
stakePubKeyHash :: MockWallet -> Maybe StakePubKeyHash
stakePubKeyHash w = StakePubKeyHash . Crypto.pubKeyHash . unStakePubKey <$> stakePubKey w

-- | The mock wallet's stake public key
stakePubKey :: MockWallet -> Maybe StakePubKey
stakePubKey w = StakePubKey . Crypto.toPublicKey . unMockPrivateKey <$> mwStakeKey w
