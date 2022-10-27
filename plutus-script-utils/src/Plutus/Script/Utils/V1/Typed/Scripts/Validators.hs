{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Plutus.Script.Utils.V1.Typed.Scripts.Validators
  ( UntypedValidator,
    mkUntypedValidator,
    ---
    ValidatorTypes (..),
    ValidatorType,
    TypedValidator,
    mkTypedValidator,
    mkTypedValidatorParam,
    validatorHash,
    validatorAddress,
    validatorScript,
    vValidatorScript,
    forwardingMintingPolicy,
    vForwardingMintingPolicy,
    forwardingMintingPolicyHash,
    generalise,
    ---
    WrongOutTypeError (..),
    ConnectionError (..),
    checkValidatorAddress,
    checkDatum,
    checkRedeemer,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Plutus.Script.Utils.Scripts (Datum, Language (PlutusV1), Versioned (Versioned))
import Plutus.Script.Utils.Typed (DatumType, RedeemerType,
                                  TypedValidator (TypedValidator, tvForwardingMPS, tvForwardingMPSHash, tvValidator, tvValidatorHash),
                                  UntypedValidator, ValidatorTypes, forwardingMintingPolicy,
                                  forwardingMintingPolicyHash, generalise, vForwardingMintingPolicy, vValidatorScript,
                                  validatorAddress, validatorHash, validatorScript)
import Plutus.Script.Utils.V1.Scripts qualified as Scripts
import Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies qualified as MPS
import Plutus.V1.Ledger.Address qualified as PV1
import Plutus.V1.Ledger.Api qualified as PV1
import PlutusCore.Default (DefaultUni)
import PlutusTx (CompiledCode, Lift, applyCode, liftCode)
import PlutusTx.Prelude (check)
import Prettyprinter (Pretty (pretty), viaShow, (<+>))

{-# INLINEABLE mkUntypedValidator #-}

-- | Converts a custom datum and redeemer from a validator function to an
-- untyped validator function. See Note [Scripts returning Bool].
--
-- Here's an example of how this function can be used:
--
-- @
--   import PlutusTx qualified
--   import Plutus.V1.Ledger.Scripts qualified as Plutus
--   import Plutus.Script.Utils.V1.Scripts (mkUntypedValidator)
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
--   import Plutus.V1.Ledger.Scripts qualified as Plutus
--   import Plutus.Script.Utils.V1.Scripts (mkUntypedValidator)
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
mkUntypedValidator ::
  forall d r.
  (PV1.UnsafeFromData d, PV1.UnsafeFromData r) =>
  (d -> r -> PV1.ScriptContext -> Bool) ->
  UntypedValidator
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
mkUntypedValidator f d r p =
  check $ f (PV1.unsafeFromBuiltinData d) (PV1.unsafeFromBuiltinData r) (PV1.unsafeFromBuiltinData p)

-- | The type of validators for the given connection type.
type ValidatorType (a :: Type) = DatumType a -> RedeemerType a -> PV1.ScriptContext -> Bool

-- | Make a 'TypedValidator' from the 'CompiledCode' of a validator script and its wrapper.
mkTypedValidator ::
  -- | Validator script (compiled)
  CompiledCode (ValidatorType a) ->
  -- | A wrapper for the compiled validator
  CompiledCode (ValidatorType a -> UntypedValidator) ->
  TypedValidator a
mkTypedValidator vc wrapper =
  TypedValidator
    { tvValidator = Versioned val PlutusV1
    , tvValidatorHash = hsh
    , tvForwardingMPS = Versioned mps PlutusV1
    , tvForwardingMPSHash = Scripts.mintingPolicyHash mps
    }
  where
    val = PV1.mkValidatorScript $ wrapper `applyCode` vc
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

data WrongOutTypeError
  = ExpectedScriptGotPubkey
  | ExpectedPubkeyGotScript
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | An error we can get while trying to type an existing transaction part.
data ConnectionError
  = WrongValidatorAddress PV1.Address PV1.Address
  | WrongOutType WrongOutTypeError
  | WrongValidatorType String
  | WrongRedeemerType PV1.BuiltinData
  | WrongDatumType PV1.BuiltinData
  | NoDatum PV1.TxOutRef PV1.DatumHash
  | UnknownRef PV1.TxOutRef
  deriving stock (Show, Eq, Ord, Generic)

instance Pretty ConnectionError where
  pretty (WrongValidatorAddress a1 a2) = "Wrong validator address. Expected:" <+> pretty a1 <+> "Actual:" <+> pretty a2
  pretty (WrongOutType t)              = "Wrong out type:" <+> viaShow t
  pretty (WrongValidatorType t)        = "Wrong validator type:" <+> pretty t
  pretty (WrongRedeemerType d)         = "Wrong redeemer type" <+> pretty (PV1.builtinDataToData d)
  pretty (WrongDatumType d)            = "Wrong datum type" <+> pretty (PV1.builtinDataToData d)
  pretty (NoDatum t d)                 = "No datum with hash " <+> pretty d <+> "for tx output" <+> pretty t
  pretty (UnknownRef d)                = "Unknown reference" <+> pretty d

-- | Checks that the given validator hash is consistent with the actual validator.
checkValidatorAddress :: forall a m. (MonadError ConnectionError m) => TypedValidator a -> PV1.Address -> m ()
checkValidatorAddress ct actualAddr = do
  let expectedAddr = validatorAddress ct
  unless (expectedAddr == actualAddr) $ throwError $ WrongValidatorAddress expectedAddr actualAddr

-- | Checks that the given redeemer script has the right type.
checkRedeemer ::
  forall inn m.
  (PV1.FromData (RedeemerType inn), MonadError ConnectionError m) =>
  TypedValidator inn ->
  PV1.Redeemer ->
  m (RedeemerType inn)
checkRedeemer _ (PV1.Redeemer d) =
  case PV1.fromBuiltinData d of
    Just v  -> pure v
    Nothing -> throwError $ WrongRedeemerType d

-- | Checks that the given datum has the right type.
checkDatum ::
  forall a m.
  (PV1.FromData (DatumType a), MonadError ConnectionError m) =>
  TypedValidator a ->
  Datum ->
  m (DatumType a)
checkDatum _ (PV1.Datum d) =
  case PV1.fromBuiltinData d of
    Just v  -> pure v
    Nothing -> throwError $ WrongDatumType d

