{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TypeFamilies       #-}
module Plutus.Script.Utils.Typed (
  UntypedValidator
  , UntypedMintingPolicy
  , UntypedStakeValidator
  ---
  , ValidatorTypes (..)
  , TypedValidator (..)
  , validatorHash
  , validatorAddress
  , validatorScript
  , vValidatorScript
  , forwardingMintingPolicy
  , vForwardingMintingPolicy
  , forwardingMintingPolicyHash
  , generalise
  ---
  , Any
  , Language (PlutusV1, PlutusV2)
  , Versioned (Versioned, unversioned, version)
  , ScriptContext(mkUntypedValidator, mkUntypedStakeValidator, mkUntypedMintingPolicy)
  , ScriptContextV1
  , ScriptContextV2
) where

import Cardano.Ledger.Alonzo.Language (Language (PlutusV1, PlutusV2))
import Data.Aeson (ToJSON)
import Data.Kind (Type)
import Data.Void (Void)
import GHC.Generics (Generic)
import Plutus.Script.Utils.Scripts (Versioned (Versioned, unversioned, version))
import Plutus.V1.Ledger.Address qualified as PV1
import Plutus.V1.Ledger.Api qualified as PV1
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.Prelude (check)

type UntypedValidator = BuiltinData -> BuiltinData -> BuiltinData -> ()
type UntypedMintingPolicy = BuiltinData -> BuiltinData -> ()
type UntypedStakeValidator = BuiltinData -> BuiltinData -> ()

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
  { tvValidator         :: Versioned PV1.Validator
  , tvValidatorHash     :: PV1.ValidatorHash
  , tvForwardingMPS     :: Versioned PV1.MintingPolicy
    -- | The hash of the minting policy that checks whether the validator
    --   is run in this transaction
  , tvForwardingMPSHash :: PV1.MintingPolicyHash
  }
  deriving stock (Show, Eq, Generic)

-- | The hash of the validator.
validatorHash :: TypedValidator a -> PV1.ValidatorHash
validatorHash = tvValidatorHash

-- | The address of the validator.
validatorAddress :: TypedValidator a -> PV1.Address
validatorAddress = PV1.scriptHashAddress . tvValidatorHash

-- | The unversioned validator script itself.
validatorScript :: TypedValidator a -> PV1.Validator
validatorScript = unversioned . vValidatorScript

-- | The validator script itself.
vValidatorScript :: TypedValidator a -> Versioned PV1.Validator
vValidatorScript = tvValidator

-- | Generalise the typed validator to one that works with the 'Data' type.
generalise :: forall a. TypedValidator a -> TypedValidator Any
generalise TypedValidator {tvValidator, tvValidatorHash, tvForwardingMPS, tvForwardingMPSHash} =
  -- we can do this safely because the on-chain validators are untyped, so they always
  -- take 'BuiltinData' arguments. The validator script stays the same, so the conversion
  -- from 'BuiltinData' to 'a' still takes place, even if it's not reflected in the type
  -- signature anymore.
  TypedValidator {tvValidator, tvValidatorHash, tvForwardingMPS, tvForwardingMPSHash}

-- | The unversioned minting policy that forwards all checks to the instance's
--   validator
forwardingMintingPolicy :: TypedValidator a -> PV1.MintingPolicy
forwardingMintingPolicy = unversioned . tvForwardingMPS

-- | The minting policy that forwards all checks to the instance's
--   validator
vForwardingMintingPolicy :: TypedValidator a -> Versioned PV1.MintingPolicy
vForwardingMintingPolicy = tvForwardingMPS


-- | Hash of the minting policy that forwards all checks to the instance's
--   validator
forwardingMintingPolicyHash :: TypedValidator a -> PV1.MintingPolicyHash
forwardingMintingPolicyHash = tvForwardingMPSHash

class PV1.UnsafeFromData a => ScriptContext a where
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
        . (PV1.UnsafeFromData d, PV1.UnsafeFromData r)
        => (d -> r -> a -> Bool)
        -> UntypedValidator
    -- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
    mkUntypedValidator f d r p =
      check $ f (PV1.unsafeFromBuiltinData d) (PV1.unsafeFromBuiltinData r) (PV1.unsafeFromBuiltinData p)

    {-# INLINABLE mkUntypedStakeValidator #-}
    -- | Converts a custom redeemer from a stake validator function to an
    -- untyped stake validator function. See Note [Scripts returning Bool].
    --
    -- Here's an example of how this function can be used:
    --
    -- @
    --   import PlutusTx qualified
    --   import Plutus.V1.Ledger.Scripts qualified as Plutus
    --   import Plutus.Script.Utils.V1.Scripts (mkUntypedStakeValidator)
    --
    --   newtype MyCustomRedeemer = MyCustomRedeemer Integer
    --   PlutusTx.unstableMakeIsData ''MyCustomRedeemer
    --
    --   mkStakeValidator :: MyCustomRedeemer -> ScriptContext -> Bool
    --   mkStakeValidator _ _ = True
    --
    --   validator :: Plutus.Validator
    --   validator = Plutus.mkStakeValidatorScript
    --       $$(PlutusTx.compile [|| wrap ||])
    --    where
    --       wrap = mkUntypedStakeValidator mkStakeValidator
    -- @
    mkUntypedStakeValidator
        :: PV1.UnsafeFromData r
        => (r -> a -> Bool)
        -> UntypedStakeValidator
    mkUntypedStakeValidator f r p =
        check $ f (PV1.unsafeFromBuiltinData r) (PV1.unsafeFromBuiltinData p)

    {-# INLINABLE mkUntypedMintingPolicy #-}
    -- | Converts a custom redeemer from a minting policy function to an
    -- untyped minting policy function. See Note [Scripts returning Bool].
    --
    -- Here's an example of how this function can be used:
    --
    -- @
    --   import PlutusTx qualified
    --   import Plutus.V1.Ledger.Scripts qualified as Plutus
    --   import Plutus.Script.Utils.V1.Scripts (mkUntypedMintingPolicy)
    --
    --   newtype MyCustomRedeemer = MyCustomRedeemer Integer
    --   PlutusTx.unstableMakeIsData ''MyCustomRedeemer
    --
    --   mkMintingPolicy :: MyCustomRedeemer -> ScriptContext -> Bool
    --   mkMintingPolicy _ _ = True
    --
    --   validator :: Plutus.Validator
    --   validator = Plutus.mkMintingPolicyScript
    --       $$(PlutusTx.compile [|| wrap ||])
    --    where
    --       wrap = mkUntypedMintingPolicy mkMintingPolicy
    -- @
    mkUntypedMintingPolicy
        :: PV1.UnsafeFromData r
        => (r -> a -> Bool)
        -> UntypedMintingPolicy
    -- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
    mkUntypedMintingPolicy f r p =
        check $ f (PV1.unsafeFromBuiltinData r) (PV1.unsafeFromBuiltinData p)


type ScriptContextV1 = PV1.ScriptContext
type ScriptContextV2 = PV2.ScriptContext

instance ScriptContext PV1.ScriptContext where

instance ScriptContext PV2.ScriptContext where
