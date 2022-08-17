{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TypeFamilies       #-}
module Plutus.Script.Utils.Typed (
  UntypedValidator
  ---
  , ValidatorTypes (..)
  , TypedValidator (..)
  , validatorHash
  , validatorAddress
  , validatorScript
  , forwardingMintingPolicy
  , forwardingMintingPolicyHash
  , generalise
  ---
  , Any
  , Language (PlutusV1, PlutusV2)
) where

import Cardano.Ledger.Alonzo.Language (Language (PlutusV1, PlutusV2))
import Data.Aeson (ToJSON)
import Data.Kind (Type)
import Data.Void (Void)
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Address qualified as PV1
import Plutus.V1.Ledger.Api qualified as PV1
import PlutusTx.Builtins (BuiltinData)

type UntypedValidator = BuiltinData -> BuiltinData -> BuiltinData -> ()

data Any
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | A class that associates a type standing for a connection type with two types, the type of the
-- redeemer and the data script for that connection type.
class ValidatorTypes (a :: Type) where
  -- | The type of the redeemers of this connection type.
  type RedeemerType a :: Type

  -- | The type of the data of this connection type.
  type DatumType a :: Type

  -- Defaults
  type RedeemerType a = ()
  type DatumType a = ()

instance ValidatorTypes Void where
  type RedeemerType Void = Void
  type DatumType Void = Void

instance ValidatorTypes Any where
  type RedeemerType Any = BuiltinData
  type DatumType Any = BuiltinData

-- | A typed validator script with its 'ValidatorScript' and 'Address'.
data TypedValidator (a :: Type) = TypedValidator
  { tvValidator         :: PV1.Validator
  , tvValidatorHash     :: PV1.ValidatorHash
  , tvForwardingMPS     :: PV1.MintingPolicy
    -- | The hash of the minting policy that checks whether the validator
    --   is run in this transaction
  , tvForwardingMPSHash :: PV1.MintingPolicyHash
  , tvLanguage          :: Language
  }
  deriving stock (Show, Eq, Generic)

-- | The hash of the validator.
validatorHash :: TypedValidator a -> PV1.ValidatorHash
validatorHash = tvValidatorHash

-- | The address of the validator.
validatorAddress :: TypedValidator a -> PV1.Address
validatorAddress = PV1.scriptHashAddress . tvValidatorHash

-- | The validator script itself.
validatorScript :: TypedValidator a -> PV1.Validator
validatorScript = tvValidator

-- | Generalise the typed validator to one that works with the 'Data' type.
generalise :: forall a. TypedValidator a -> TypedValidator Any
generalise TypedValidator {tvValidator, tvValidatorHash, tvForwardingMPS, tvForwardingMPSHash, tvLanguage} =
  -- we can do this safely because the on-chain validators are untyped, so they always
  -- take 'BuiltinData' arguments. The validator script stays the same, so the conversion
  -- from 'BuiltinData' to 'a' still takes place, even if it's not reflected in the type
  -- signature anymore.
  TypedValidator {tvValidator, tvValidatorHash, tvForwardingMPS, tvForwardingMPSHash, tvLanguage}

-- | The minting policy that forwards all checks to the instance's
--   validator
forwardingMintingPolicy :: TypedValidator a -> PV1.MintingPolicy
forwardingMintingPolicy = tvForwardingMPS

-- | Hash of the minting policy that forwards all checks to the instance's
--   validator
forwardingMintingPolicyHash :: TypedValidator a -> PV1.MintingPolicyHash
forwardingMintingPolicyHash = tvForwardingMPSHash
