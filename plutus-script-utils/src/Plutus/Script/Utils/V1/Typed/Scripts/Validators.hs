{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
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
    unsafeMkTypedValidator,
    forwardingMintingPolicy,
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

import Codec.Serialise (deserialise, serialise)
import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.ByteString.Lazy qualified as BSL
import Data.Kind (Type)
import Data.Void (Void)
import GHC.Generics (Generic)
import Plutus.Script.Utils.V1.Scripts qualified as Scripts
import Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies qualified as MPS
import Plutus.Script.Utils.V1.Typed.TypeUtils (Any)
import Plutus.V1.Ledger.Address qualified as PV1
import Plutus.V1.Ledger.Api qualified as PV1
import Plutus.V1.Ledger.Tx qualified as PV1
import PlutusCore.Default (DefaultUni)
import PlutusTx (CompiledCode, Lift, applyCode, dataToBuiltinData, liftCode)
import PlutusTx.Prelude (check)
import Prettyprinter (Pretty (pretty), viaShow, (<+>))

type UntypedValidator = PV1.BuiltinData -> PV1.BuiltinData -> PV1.BuiltinData -> ()

{-# INLINEABLE mkUntypedValidator #-}

-- | Converts a custom datum and redeemer from a validator function to an
-- untyped validator function. See Note [Scripts returning Bool].
--
-- Here's an example of how this function can be used:
--
-- @
--   import PlutusTx qualified
--   import Plutus.V1.Ledger.Scripts qualified as Plutus
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
mkUntypedValidator ::
  forall d r.
  (PV1.UnsafeFromData d, PV1.UnsafeFromData r) =>
  (d -> r -> PV1.ScriptContext -> Bool) ->
  UntypedValidator
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
mkUntypedValidator f d r p =
  check $ f (PV1.unsafeFromBuiltinData d) (PV1.unsafeFromBuiltinData r) (PV1.unsafeFromBuiltinData p)

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

-- | The type of validators for the given connection type.
type ValidatorType (a :: Type) = DatumType a -> RedeemerType a -> PV1.ScriptContext -> Bool

instance ValidatorTypes Void where
  type RedeemerType Void = Void
  type DatumType Void = Void

instance ValidatorTypes Any where
  type RedeemerType Any = PV1.BuiltinData
  type DatumType Any = PV1.BuiltinData

-- | A typed validator script with its 'ValidatorScript' and 'Address'.
data TypedValidator (a :: Type) = TypedValidator
  { tvValidator         :: PV1.Validator,
    tvValidatorHash     :: PV1.ValidatorHash,
    tvForwardingMPS     :: PV1.MintingPolicy,
    -- | The hash of the minting policy that checks whether the validator
    --   is run in this transaction
    tvForwardingMPSHash :: PV1.MintingPolicyHash
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

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

-- | The hash of the validator.
validatorHash :: TypedValidator a -> PV1.ValidatorHash
validatorHash = tvValidatorHash

-- | The address of the validator.
validatorAddress :: TypedValidator a -> PV1.Address
validatorAddress = PV1.scriptHashAddress . tvValidatorHash

-- | The validator script itself.
validatorScript :: TypedValidator a -> PV1.Validator
validatorScript = tvValidator

-- | Make a 'TypedValidator' (with no type constraints) from an untyped 'Validator' script.
unsafeMkTypedValidator :: PV1.Validator -> TypedValidator Any
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
forwardingMintingPolicy :: TypedValidator a -> PV1.MintingPolicy
forwardingMintingPolicy = tvForwardingMPS

-- | Hash of the minting policy that forwards all checks to the instance's
--   validator
forwardingMintingPolicyHash :: TypedValidator a -> PV1.MintingPolicyHash
forwardingMintingPolicyHash = tvForwardingMPSHash

-- TODO: these should probably live somewhere else
instance ToJSON PV1.BuiltinData where
  toJSON d = toJSON (BSL.toStrict (serialise (PV1.builtinDataToData d)))

instance FromJSON PV1.BuiltinData where
  parseJSON v = dataToBuiltinData . deserialise . BSL.fromStrict <$> parseJSON v

data WrongOutTypeError
  = ExpectedScriptGotPubkey
  | ExpectedPubkeyGotScript
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | An error we can get while trying to type an existing transaction part.
data ConnectionError
  = WrongValidatorAddress PV1.Address PV1.Address
  | WrongOutType WrongOutTypeError
  | WrongInType PV1.TxInType
  | MissingInType
  | WrongValidatorType String
  | WrongRedeemerType PV1.BuiltinData
  | WrongDatumType PV1.BuiltinData
  | NoDatum PV1.TxOutRef PV1.DatumHash
  | UnknownRef
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty ConnectionError where
  pretty (WrongValidatorAddress a1 a2) = "Wrong validator address. Expected:" <+> pretty a1 <+> "Actual:" <+> pretty a2
  pretty (WrongOutType t)              = "Wrong out type:" <+> viaShow t
  pretty (WrongInType t)               = "Wrong in type:" <+> viaShow t
  pretty MissingInType                 = "Missing in type"
  pretty (WrongValidatorType t)        = "Wrong validator type:" <+> pretty t
  pretty (WrongRedeemerType d)         = "Wrong redeemer type" <+> pretty (PV1.builtinDataToData d)
  pretty (WrongDatumType d)            = "Wrong datum type" <+> pretty (PV1.builtinDataToData d)
  pretty (NoDatum t d)                 = "No datum with hash " <+> pretty d <+> "for tx output" <+> pretty t
  pretty UnknownRef                    = "Unknown reference"

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
  Scripts.Datum ->
  m (DatumType a)
checkDatum _ (PV1.Datum d) =
  case PV1.fromBuiltinData d of
    Just v  -> pure v
    Nothing -> throwError $ WrongDatumType d
