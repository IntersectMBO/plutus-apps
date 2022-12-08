{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}

module Ledger.Address
    ( module Export
    , CardanoAddress
    , PaymentPrivateKey(..)
    , PaymentPubKey(..)
    , PaymentPubKeyHash(..)
    , StakePubKey(..)
    , StakePubKeyHash(..)
    , paymentPubKeyHash
    , pubKeyHashAddress
    , pubKeyAddress
    , scriptValidatorHashAddress
    , stakePubKeyHashCredential
    , stakeValidatorHashCredential
    , xprvToPaymentPubKey
    , xprvToPaymentPubKeyHash
    , xprvToStakingCredential
    , xprvToStakePubKey
    , xprvToStakePubKeyHash
    ) where

import Cardano.Api qualified as C
import Cardano.Crypto.Wallet qualified as Crypto
import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Data.OpenApi qualified as OpenApi
import GHC.Generics (Generic)
import Ledger.Address.Orphans as Export ()
import Ledger.Crypto (PubKey (PubKey), PubKeyHash (PubKeyHash), pubKeyHash, toPublicKey)
import Ledger.Orphans ()
import Ledger.Scripts (StakeValidatorHash (..), ValidatorHash (..))
import Plutus.V1.Ledger.Address as Export hiding (pubKeyHashAddress)
import Plutus.V1.Ledger.Credential (Credential (PubKeyCredential, ScriptCredential), StakingCredential (StakingHash))
import PlutusTx qualified
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter (Pretty)

type CardanoAddress = C.AddressInEra C.BabbageEra

newtype PaymentPrivateKey = PaymentPrivateKey { unPaymentPrivateKey :: Crypto.XPrv }

newtype PaymentPubKey = PaymentPubKey { unPaymentPubKey :: PubKey }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, OpenApi.ToSchema)
    deriving newtype (PlutusTx.Eq, PlutusTx.Ord, Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving (Show, Pretty) via PubKey
makeLift ''PaymentPubKey

xprvToPaymentPubKey :: Crypto.XPrv -> PaymentPubKey
xprvToPaymentPubKey = PaymentPubKey . toPublicKey

newtype PaymentPubKeyHash = PaymentPubKeyHash { unPaymentPubKeyHash :: PubKeyHash }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, OpenApi.ToSchema)
    deriving newtype (PlutusTx.Eq, PlutusTx.Ord, Serialise, Hashable, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving (Show, Pretty) via PubKeyHash
makeLift ''PaymentPubKeyHash

xprvToPaymentPubKeyHash :: Crypto.XPrv -> PaymentPubKeyHash
xprvToPaymentPubKeyHash = PaymentPubKeyHash . pubKeyHash . toPublicKey

newtype StakePubKey = StakePubKey { unStakePubKey :: PubKey }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, OpenApi.ToSchema)
    deriving newtype (PlutusTx.Eq, PlutusTx.Ord, Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving (Show, Pretty) via PubKey
makeLift ''StakePubKey

xprvToStakePubKey :: Crypto.XPrv -> StakePubKey
xprvToStakePubKey = StakePubKey . toPublicKey

newtype StakePubKeyHash = StakePubKeyHash { unStakePubKeyHash :: PubKeyHash }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, OpenApi.ToSchema)
    deriving newtype (PlutusTx.Eq, PlutusTx.Ord, Serialise, Hashable, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving (Show, Pretty) via PubKeyHash
makeLift ''StakePubKeyHash

xprvToStakePubKeyHash :: Crypto.XPrv -> StakePubKeyHash
xprvToStakePubKeyHash = StakePubKeyHash . pubKeyHash . toPublicKey

xprvToStakingCredential :: Crypto.XPrv -> StakingCredential
xprvToStakingCredential = stakePubKeyHashCredential . xprvToStakePubKeyHash

{-# INLINABLE paymentPubKeyHash #-}
paymentPubKeyHash :: PaymentPubKey -> PaymentPubKeyHash
paymentPubKeyHash (PaymentPubKey pk) = PaymentPubKeyHash (pubKeyHash pk)

{-# INLINABLE pubKeyHashAddress #-}
-- | The address that should be targeted by a transaction output locked by the
-- given public payment key (with its staking credentials).
pubKeyHashAddress :: PaymentPubKeyHash -> Maybe StakingCredential -> Address
pubKeyHashAddress (PaymentPubKeyHash pkh) = Address (PubKeyCredential pkh)

{-# INLINABLE pubKeyAddress #-}
-- | The address that should be targeted by a transaction output locked by the given public key.
-- (with its staking credentials).
pubKeyAddress :: PaymentPubKey -> Maybe StakingCredential -> Address
pubKeyAddress (PaymentPubKey pk) = Address (PubKeyCredential (pubKeyHash pk))

{-# INLINABLE scriptValidatorHashAddress #-}
-- | The address that should be used by a transaction output locked by the given validator script
-- (with its staking credentials).
scriptValidatorHashAddress :: ValidatorHash -> Maybe StakingCredential -> Address
scriptValidatorHashAddress vh = Address (ScriptCredential vh)

{-# INLINABLE stakePubKeyHashCredential #-}
-- | Construct a `StakingCredential` from a public key hash.
stakePubKeyHashCredential :: StakePubKeyHash -> StakingCredential
stakePubKeyHashCredential = StakingHash . PubKeyCredential . unStakePubKeyHash

{-# INLINEABLE stakeValidatorHashCredential #-}
-- | Construct a `StakingCredential` from a validator script hash.
stakeValidatorHashCredential :: StakeValidatorHash -> StakingCredential
stakeValidatorHashCredential (StakeValidatorHash h) = StakingHash . ScriptCredential . ValidatorHash $ h

