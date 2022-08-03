{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
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
    , unsafeMkTypedValidator
    , forwardingMintingPolicy
    , forwardingMintingPolicyHash
    , generalise
    )
where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType, UntypedValidator, ValidatorTypes)
import Plutus.Script.Utils.V1.Typed.TypeUtils (Any)
import Plutus.Script.Utils.V2.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies qualified as MPS
import Plutus.V1.Ledger.Address qualified as PV1
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusCore.Default (DefaultUni)
import PlutusTx (CompiledCode, Lift, UnsafeFromData (unsafeFromBuiltinData), applyCode, liftCode)
import PlutusTx.Prelude (check)

{-# INLINABLE mkUntypedValidator #-}
-- | Converts a custom datum and redeemer from a validator function to an
-- untyped validator function. See Note [Scripts returning Bool].
--
-- Here's an example of how this function can be used:
--
-- @
--   import PlutusTx qualified
--   import Plutus.V2.Ledger.Scripts qualified as Plutus
--   import Plutus.Script.Utils.V2.Scripts (mkUntypedValidator)
--
--   newtype MyCustomDatum = MyCustomDatum Integer
--   PlutusTx.unstableMakeIsData ''MyCustomDatum
--   newtype MyCustomRedeemer = MyCustomRedeemer Integer
--   PlutusTx.unstableMakeIsData ''MyCustomRedeemer
--
--   mkValidator :: MyCustomDatum -> MyCustomRedeemer -> Plutus.ScriptContext -> Bool
--   mkValidator _ _ _ = True
--
--   validator :: Plutus.Validator
--   validator = Plutus.mkValidatorScript
--       $$(PlutusTx.compile [|| wrap ||])
--    where
--       wrap = mkUntypedValidator mkValidator
-- @
--
-- Here's an example using a parameterized validator:
--
-- @
--   import PlutusTx qualified
--   import Plutus.V2.Ledger.Scripts qualified as Plutus
--   import Plutus.Script.Utils.V2.Scripts (mkUntypedValidator)
--
--   newtype MyCustomDatum = MyCustomDatum Integer
--   PlutusTx.unstableMakeIsData ''MyCustomDatum
--   newtype MyCustomRedeemer = MyCustomRedeemer Integer
--   PlutusTx.unstableMakeIsData ''MyCustomRedeemer
--
--   mkValidator :: Int -> MyCustomDatum -> MyCustomRedeemer -> Plutus.ScriptContext -> Bool
--   mkValidator _ _ _ _ = True
--
--   validator :: Int -> Plutus.Validator
--   validator i = Plutus.mkValidatorScript
--       $$(PlutusTx.compile [|| wrap . mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode i
--    where
--       wrap = mkUntypedValidator
-- @
mkUntypedValidator
    :: forall d r
    . (UnsafeFromData d, UnsafeFromData r)
    => (d -> r -> PV2.ScriptContext -> Bool)
    -> UntypedValidator
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
mkUntypedValidator f d r p =
    check $ f (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)

-- | The type of validators for the given connection type.
type ValidatorType (a :: Type) = DatumType a -> RedeemerType a -> PV2.ScriptContext -> Bool

-- | A typed validator script with its 'ValidatorScript' and 'Address'.
data TypedValidator (a :: Type) = TypedValidator
  { tvValidator         :: PV2.Validator,
    tvValidatorHash     :: PV2.ValidatorHash,
    tvForwardingMPS     :: PV2.MintingPolicy,
    -- | The hash of the minting policy that checks whether the validator
    --   is run in this transaction
    tvForwardingMPSHash :: PV2.MintingPolicyHash
  }
  deriving stock (Show, Eq, Generic)

-- | Generalise the typed validator to one that works with the 'Data' type.
generalise :: forall a. TypedValidator a -> TypedValidator Any
generalise TypedValidator {tvValidator, tvValidatorHash, tvForwardingMPS, tvForwardingMPSHash} =
  -- we can do this safely because the on-chain validators are untyped, so they always
  -- take 'BuiltinData' arguments. The validator script stays the same, so the conversion
  -- from 'BuiltinData' to 'a' still takes place, even if it's not reflected in the type
  -- signature anymore.
  TypedValidator {tvValidator, tvValidatorHash, tvForwardingMPS, tvForwardingMPSHash}

-- | Make a 'TypedValidator' from the 'CompiledCode' of a validator script and its wrapper.
mkTypedValidator ::
  -- | Validator script (compiled)
  CompiledCode (ValidatorType a) ->
  -- | A wrapper for the compiled validator
  CompiledCode (ValidatorType a -> UntypedValidator) ->
  TypedValidator a
mkTypedValidator vc wrapper =
  TypedValidator
    { tvValidator = val,
      tvValidatorHash = hsh,
      tvForwardingMPS = mps,
      tvForwardingMPSHash = Scripts.mintingPolicyHash mps
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

-- | The hash of the validator.
validatorHash :: TypedValidator a -> PV2.ValidatorHash
validatorHash = tvValidatorHash

-- | The address of the validator.
validatorAddress :: TypedValidator a -> PV2.Address
validatorAddress = PV1.scriptHashAddress . tvValidatorHash

-- | The validator script itself.
validatorScript :: TypedValidator a -> PV2.Validator
validatorScript = tvValidator

-- | Make a 'TypedValidator' (with no type constraints) from an untyped 'Validator' script.
unsafeMkTypedValidator :: PV2.Validator -> TypedValidator Any
unsafeMkTypedValidator vl =
  TypedValidator
    { tvValidator = vl,
      tvValidatorHash = vh,
      tvForwardingMPS = mps,
      tvForwardingMPSHash = Scripts.mintingPolicyHash mps
    }
  where
    vh = Scripts.validatorHash vl
    mps = MPS.mkForwardingMintingPolicy vh

-- | The minting policy that forwards all checks to the instance's
--   validator
forwardingMintingPolicy :: TypedValidator a -> PV2.MintingPolicy
forwardingMintingPolicy = tvForwardingMPS

-- | Hash of the minting policy that forwards all checks to the instance's
--   validator
forwardingMintingPolicyHash :: TypedValidator a -> PV2.MintingPolicyHash
forwardingMintingPolicyHash = tvForwardingMPSHash
