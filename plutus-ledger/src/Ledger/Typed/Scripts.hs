{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Ledger.Typed.Scripts
  ( module Export
  , MintingPolicy
  , Validator
  , PV1.ConnectionError (..)
  , mkForwardingMintingPolicy
  , unsafeMkTypedValidator
  -- TODO: Don't export Plutus V1 specific code from a module that doesn't mention a plutus version
  , PV1.ValidatorType
  , PV1.mkTypedValidator
  , PV1.mkTypedValidatorParam
  , PV1.mkUntypedMintingPolicy
  , PV1.mkUntypedValidator
  ) where

import Ledger.Typed.Scripts.Orphans as Export ()
import Plutus.Script.Utils.Scripts qualified as Untyped
import Plutus.Script.Utils.Typed as Export
import Plutus.Script.Utils.V1.Typed.Scripts qualified as PV1
import Plutus.Script.Utils.V2.Typed.Scripts qualified as PV2
import Plutus.V1.Ledger.Api (MintingPolicy, Validator)

mkForwardingMintingPolicy :: Versioned Validator -> Versioned MintingPolicy
mkForwardingMintingPolicy vl@(Versioned _ PlutusV1) = Versioned (PV1.mkForwardingMintingPolicy (Untyped.validatorHash vl)) PlutusV1
mkForwardingMintingPolicy vl@(Versioned _ PlutusV2) = Versioned (PV2.mkForwardingMintingPolicy (Untyped.validatorHash vl)) PlutusV2

-- | Make a 'TypedValidator' (with no type constraints) from an untyped 'Validator' script.
unsafeMkTypedValidator :: Versioned Validator -> TypedValidator Any
unsafeMkTypedValidator vl =
  TypedValidator
    { tvValidator = vl
    , tvValidatorHash = vh
    , tvForwardingMPS = mps
    , tvForwardingMPSHash = Untyped.mintingPolicyHash mps
    }
  where
    vh = Untyped.validatorHash vl
    mps = mkForwardingMintingPolicy vl

