{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Plutus.Script.Utils.V2.Typed.Scripts.Validators
    ( UntypedValidator
    , mkUntypedValidator
    ---
    , ValidatorTypes (..)
    , ValidatorType
    , TypedValidator
    , mkTypedValidator
    , mkTypedValidatorParam
    , validatorHash
    , validatorAddress
    , validatorScript
    , vValidatorScript
    , forwardingMintingPolicy
    , vForwardingMintingPolicy
    , forwardingMintingPolicyHash
    , generalise
    )
where

import Data.Kind (Type)
import Plutus.Script.Utils.Scripts (Language (PlutusV2), Versioned (Versioned))
import Plutus.Script.Utils.Typed (DatumType, RedeemerType,
                                  TypedValidator (TypedValidator, tvForwardingMPS, tvForwardingMPSHash, tvValidator, tvValidatorHash),
                                  UntypedValidator, ValidatorTypes, forwardingMintingPolicy,
                                  forwardingMintingPolicyHash, generalise, mkUntypedValidator, vForwardingMintingPolicy,
                                  vValidatorScript, validatorAddress, validatorHash, validatorScript)
import Plutus.Script.Utils.V2.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies qualified as MPS
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusCore.Default (DefaultUni)
import PlutusTx (CompiledCode, Lift, applyCode, liftCode)

-- | The type of validators for the given connection type.
type ValidatorType (a :: Type) = DatumType a -> RedeemerType a -> PV2.ScriptContext -> Bool

-- | Make a 'TypedValidator' from the 'CompiledCode' of a validator script and its wrapper.
mkTypedValidator ::
  -- | Validator script (compiled)
  CompiledCode (ValidatorType a) ->
  -- | A wrapper for the compiled validator
  CompiledCode (ValidatorType a -> UntypedValidator) ->
  TypedValidator a
mkTypedValidator vc wrapper =
  TypedValidator
    { tvValidator = Versioned val PlutusV2
    , tvValidatorHash = hsh
    , tvForwardingMPS = Versioned mps PlutusV2
    , tvForwardingMPSHash = Scripts.mintingPolicyHash mps
    }
  where
    val = PV2.mkValidatorScript $ wrapper `applyCode` vc
    hsh = Scripts.validatorHash val
    mps = MPS.mkForwardingMintingPolicy hsh

-- | Make a 'TypedValidator' from the 'CompiledCode' of a parameterized validator script and its wrapper.
mkTypedValidatorParam ::
  forall a param.
  Lift DefaultUni param =>
  -- | Validator script (compiled)
  CompiledCode (param -> ValidatorType a) ->
  -- | A wrapper for the compiled validator
  CompiledCode (ValidatorType a -> UntypedValidator) ->
  -- | The extra paramater for the validator script
  param ->
  TypedValidator a
mkTypedValidatorParam vc wrapper param =
  mkTypedValidator (vc `applyCode` liftCode param) wrapper
