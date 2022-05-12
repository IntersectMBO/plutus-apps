{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE NamedFieldPuns     #-}

module Ledger.Typed.Scripts.Validators
    ( ValidatorTypes(..)
    , ValidatorType
    , TypedValidator
    , mkTypedValidator
    , mkTypedValidatorParam
    , validatorHash
    , validatorAddress
    , validatorScript
    , unsafeMkTypedValidator
    , forwardingMintingPolicy
    , forwardingMintingPolicyHash
    , generalise

    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import Data.Void (Void)
import GHC.Generics (Generic)

import Plutus.V1.Ledger.Address (Address, scriptHashAddress)
import Plutus.V1.Ledger.Contexts qualified as Validation
import Plutus.V1.Ledger.Scripts qualified as Scripts
import PlutusCore.Default (DefaultUni)
import PlutusTx (BuiltinData, CompiledCode, Lift, applyCode, liftCode)

import Ledger.Typed.TypeUtils (Any)
import Plutus.Script.Utils.V1.Scripts qualified as ScriptUtils
import Plutus.Script.Utils.V1.Typed.Scripts (UntypedValidator)
import Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies qualified as MPS

-- | A class that associates a type standing for a connection type with two types, the type of the
-- redeemer and the data script for that connection type.
class ValidatorTypes (a :: Type) where
    -- | The type of the redeemers of this connection type.
    type RedeemerType a :: Type
    -- | The type of the data of this connection type.
    type DatumType a :: Type

    -- Defaults
    type instance RedeemerType a = ()
    type instance DatumType  a = ()

-- | The type of validators for the given connection type.
type ValidatorType (a :: Type) = DatumType a -> RedeemerType a -> Validation.ScriptContext -> Bool

instance ValidatorTypes Void where
    type RedeemerType Void = Void
    type DatumType Void = Void

instance ValidatorTypes Any where
    type RedeemerType Any = BuiltinData
    type DatumType Any = BuiltinData

-- | A typed validator script with its 'ValidatorScript' and 'Address'.
data TypedValidator (a :: Type) =
    TypedValidator
        { tvValidator         :: Scripts.Validator
        , tvValidatorHash     :: Scripts.ValidatorHash
        , tvForwardingMPS     :: Scripts.MintingPolicy
        , tvForwardingMPSHash :: Scripts.MintingPolicyHash
        -- ^ The hash of the minting policy that checks whether the validator
        --   is run in this transaction
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

{-| Generalise the typed validator to one that works with the 'Data' type.
-}
generalise :: forall a. TypedValidator a -> TypedValidator Any
generalise TypedValidator{tvValidator, tvValidatorHash, tvForwardingMPS, tvForwardingMPSHash} =
    -- we can do this safely because the on-chain validators are untyped, so they always
    -- take 'BuiltinData' arguments. The validator script stays the same, so the conversion
    -- from 'BuiltinData' to 'a' still takes place, even if it's not reflected in the type
    -- signature anymore.
    TypedValidator{tvValidator, tvValidatorHash, tvForwardingMPS, tvForwardingMPSHash}

-- | Make a 'TypedValidator' from the 'CompiledCode' of a validator script and its wrapper.
mkTypedValidator ::
    CompiledCode (ValidatorType a)
    -- ^ Validator script (compiled)
    -> CompiledCode (ValidatorType a -> UntypedValidator)
    -- ^ A wrapper for the compiled validator
    -> TypedValidator a
mkTypedValidator vc wrapper =
    let val = Scripts.mkValidatorScript $ wrapper `applyCode` vc
        hsh = ScriptUtils.validatorHash val
        mps = MPS.mkForwardingMintingPolicy hsh
    in TypedValidator
        { tvValidator         = val
        , tvValidatorHash     = hsh
        , tvForwardingMPS     = mps
        , tvForwardingMPSHash = ScriptUtils.mintingPolicyHash mps
        }

-- | Make a 'TypedValidator' from the 'CompiledCode' of a parameterized validator script and its wrapper.
mkTypedValidatorParam
    :: forall a param. Lift DefaultUni param
    => CompiledCode (param -> ValidatorType a)
    -- ^ Validator script (compiled)
    -> CompiledCode (ValidatorType a -> UntypedValidator)
    -- ^ A wrapper for the compiled validator
    -> param
    -- ^ The extra paramater for the validator script
    -> TypedValidator a
mkTypedValidatorParam vc wrapper param =
    mkTypedValidator (vc `PlutusTx.applyCode` PlutusTx.liftCode param) wrapper

-- | The hash of the validator.
validatorHash :: TypedValidator a -> Scripts.ValidatorHash
validatorHash = tvValidatorHash

-- | The address of the validator.
validatorAddress :: TypedValidator a -> Address
validatorAddress = scriptHashAddress . tvValidatorHash

-- | The validator script itself.
validatorScript :: TypedValidator a -> Scripts.Validator
validatorScript = tvValidator

-- | Make a 'TypedValidator' (with no type constraints) from an untyped 'Validator' script.
unsafeMkTypedValidator :: Scripts.Validator -> TypedValidator Any
unsafeMkTypedValidator vl =
    let vh = ScriptUtils.validatorHash vl
        mps = MPS.mkForwardingMintingPolicy vh
    in
    TypedValidator
        { tvValidator         = vl
        , tvValidatorHash     = vh
        , tvForwardingMPS     = mps
        , tvForwardingMPSHash = ScriptUtils.mintingPolicyHash mps
        }

-- | The minting policy that forwards all checks to the instance's
--   validator
forwardingMintingPolicy :: TypedValidator a -> Scripts.MintingPolicy
forwardingMintingPolicy = tvForwardingMPS

-- | Hash of the minting policy that forwards all checks to the instance's
--   validator
forwardingMintingPolicyHash :: TypedValidator a -> Scripts.MintingPolicyHash
forwardingMintingPolicyHash = tvForwardingMPSHash
