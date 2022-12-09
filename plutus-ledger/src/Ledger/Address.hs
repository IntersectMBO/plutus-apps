{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Ledger.Address
    ( module Export
    , CardanoAddress
    , PaymentPrivateKey(..)
    , PaymentPubKey(..)
    , PaymentPubKeyHash(..)
    , StakePubKey(..)
    , StakePubKeyHash(..)
    , toPlutusAddress
    , toPlutusPubKeyHash
    , cardanoAddressCredential
    , cardanoPubKeyHash
    , cardanoStakingCredential
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
    , mkValidatorCardanoAddress
    ) where

import Cardano.Api qualified as C
import Cardano.Api.Byron qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Chain.Common (addrToBase58)
import Cardano.Crypto.Wallet qualified as Crypto
import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Data.OpenApi qualified as OpenApi
import GHC.Generics (Generic)
import Ledger.Address.Orphans as Export ()
import Ledger.Crypto (PubKey (PubKey), PubKeyHash (PubKeyHash), pubKeyHash, toPublicKey)
import Ledger.Orphans ()
import Ledger.Scripts (Language (..), StakeValidatorHash (..), Validator, ValidatorHash (..), Versioned (..))
import Plutus.Script.Utils.V1.Address qualified as PV1
import Plutus.Script.Utils.V2.Address qualified as PV2
import Plutus.V1.Ledger.Address as Export hiding (pubKeyHashAddress)
import Plutus.V1.Ledger.Credential (Credential (PubKeyCredential, ScriptCredential), StakingCredential (StakingHash))
import PlutusTx qualified
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter (Pretty)

type CardanoAddress = C.AddressInEra C.BabbageEra

cardanoAddressCredential :: C.AddressInEra era -> Credential
cardanoAddressCredential (C.AddressInEra C.ByronAddressInAnyEra (C.ByronAddress address))
  = PubKeyCredential
  $ PubKeyHash
  $ PlutusTx.toBuiltin
  $ addrToBase58 address
cardanoAddressCredential (C.AddressInEra _ (C.ShelleyAddress _ paymentCredential _))
  = case C.fromShelleyPaymentCredential paymentCredential of
      C.PaymentCredentialByKey paymentKeyHash ->
          PubKeyCredential
          $ PubKeyHash
          $ PlutusTx.toBuiltin
          $ C.serialiseToRawBytes paymentKeyHash
      C.PaymentCredentialByScript scriptHash ->
          ScriptCredential $ scriptToValidatorHash scriptHash

cardanoStakingCredential :: C.AddressInEra era -> Maybe StakingCredential
cardanoStakingCredential (C.AddressInEra C.ByronAddressInAnyEra _) = Nothing
cardanoStakingCredential (C.AddressInEra _ (C.ShelleyAddress _ _ stakeAddressReference))
  = case C.fromShelleyStakeReference stakeAddressReference of
         C.NoStakeAddress -> Nothing
         (C.StakeAddressByValue stakeCredential) ->
             Just (StakingHash $ fromCardanoStakeCredential stakeCredential)
         C.StakeAddressByPointer{} -> Nothing -- Not supported
  where
    fromCardanoStakeCredential :: C.StakeCredential -> Credential
    fromCardanoStakeCredential (C.StakeCredentialByKey stakeKeyHash)
      = PubKeyCredential
      $ PubKeyHash
      $ PlutusTx.toBuiltin
      $ C.serialiseToRawBytes stakeKeyHash
    fromCardanoStakeCredential (C.StakeCredentialByScript scriptHash) = ScriptCredential (scriptToValidatorHash scriptHash)

cardanoPubKeyHash :: C.AddressInEra era -> Maybe PubKeyHash
cardanoPubKeyHash addr = case cardanoAddressCredential addr of
  PubKeyCredential x -> Just x
  _                  -> Nothing

toPlutusAddress :: C.AddressInEra era -> Address
toPlutusAddress address = Address (cardanoAddressCredential address) (cardanoStakingCredential address)

toPlutusPubKeyHash :: C.Hash C.PaymentKey -> PubKeyHash
toPlutusPubKeyHash paymentKeyHash = PubKeyHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes paymentKeyHash

scriptToValidatorHash :: C.ScriptHash -> ValidatorHash
scriptToValidatorHash = ValidatorHash . PlutusTx.toBuiltin . C.serialiseToRawBytes

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

-- | Cardano address of a versioned 'Validator' script.
mkValidatorCardanoAddress :: C.NetworkId -> Versioned Validator -> C.AddressInEra C.BabbageEra
mkValidatorCardanoAddress networkId (Versioned val PlutusV1) = PV1.mkValidatorCardanoAddress networkId val
mkValidatorCardanoAddress networkId (Versioned val PlutusV2) = PV2.mkValidatorCardanoAddress networkId val
