{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}

module Ledger.Address
    ( module Export
    , PaymentPrivateKey(..)
    , PaymentPubKey(..)
    , PaymentPubKeyHash(..)
    , StakePubKey(..)
    , StakePubKeyHash(..)
    , paymentPubKeyHash
    , pubKeyHashAddress
    , pubKeyAddress
    , scriptAddress
    ) where

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.OpenApi qualified as OpenApi
import GHC.Generics (Generic)
import Ledger.Crypto (PrivateKey, PubKey (PubKey), PubKeyHash (PubKeyHash), pubKeyHash)
import Ledger.Orphans ()
import Ledger.Scripts (Validator, validatorHash)
import Plutus.V1.Ledger.Address as Export hiding (pubKeyHashAddress)
import Plutus.V1.Ledger.Credential (Credential (PubKeyCredential, ScriptCredential), StakingCredential (StakingHash))
import PlutusTx qualified
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter (Pretty)

newtype PaymentPrivateKey = PaymentPrivateKey { unPaymentPrivateKey :: PrivateKey }

newtype PaymentPubKey = PaymentPubKey { unPaymentPubKey :: PubKey }
    deriving stock (Eq, Ord, Generic, Data)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, OpenApi.ToSchema)
    deriving newtype (PlutusTx.Eq, PlutusTx.Ord, Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving (Show, Pretty) via PubKey
makeLift ''PaymentPubKey

newtype PaymentPubKeyHash = PaymentPubKeyHash { unPaymentPubKeyHash :: PubKeyHash }
    deriving stock (Eq, Ord, Generic, Data)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, OpenApi.ToSchema)
    deriving newtype (PlutusTx.Eq, PlutusTx.Ord, Serialise, Hashable, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving (Show, Pretty) via PubKeyHash
makeLift ''PaymentPubKeyHash

newtype StakePubKey = StakePubKey { unStakePubKey :: PubKey }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, OpenApi.ToSchema)
    deriving newtype (PlutusTx.Eq, PlutusTx.Ord, Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving (Show, Pretty) via PubKey
makeLift ''StakePubKey

newtype StakePubKeyHash = StakePubKeyHash { unStakePubKeyHash :: PubKeyHash }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, OpenApi.ToSchema)
    deriving newtype (PlutusTx.Eq, PlutusTx.Ord, Serialise, Hashable, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving (Show, Pretty) via PubKeyHash
makeLift ''StakePubKeyHash

{-# INLINABLE paymentPubKeyHash #-}
paymentPubKeyHash :: PaymentPubKey -> PaymentPubKeyHash
paymentPubKeyHash (PaymentPubKey pk) = PaymentPubKeyHash (pubKeyHash pk)

{-# INLINABLE pubKeyHashAddress #-}
-- | The address that should be targeted by a transaction output locked by the
-- given public payment key (with it's public stake key).
--
-- TODO: This should be moved to Plutus.V1(or V2).Ledger.Address with the newtypes.
pubKeyHashAddress :: PaymentPubKeyHash -> Maybe StakePubKeyHash -> Address
pubKeyHashAddress (PaymentPubKeyHash pkh) skh =
    Address (PubKeyCredential pkh)
            (fmap (StakingHash . PubKeyCredential . unStakePubKeyHash) skh)

{-# INLINABLE pubKeyAddress #-}
-- | The address that should be targeted by a transaction output locked by the given public key.
pubKeyAddress :: PaymentPubKey -> Maybe StakePubKey -> Address
pubKeyAddress (PaymentPubKey pk) skh =
    Address (PubKeyCredential (pubKeyHash pk))
            (fmap (StakingHash . PubKeyCredential . pubKeyHash . unStakePubKey) skh)

{-# INLINABLE scriptAddress #-}
-- | The address that should be used by a transaction output locked by the given validator script.
scriptAddress :: Validator -> Address
scriptAddress validator = Address (ScriptCredential (validatorHash validator)) Nothing
