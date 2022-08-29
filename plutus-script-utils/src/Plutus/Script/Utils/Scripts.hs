{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-|
This module contains functions related to versioning scripts and BuiltinData, or more specifially,
'Datum's and 'Redeemer's. These functions do not depend on a particular version of Plutus.
-}
module Plutus.Script.Utils.Scripts
    ( -- * Plutus language versioning
      Language (..)
    , Versioned (..)
      -- * Script data hashes
    , PV1.Datum
    , PV1.DatumHash
    , PV1.Redeemer
    , PV1.RedeemerHash
    , datumHash
    , redeemerHash
    , dataHash
    ) where

import Cardano.Api qualified as Script
import Cardano.Api.Shelley qualified as Script
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1, PlutusV2))
import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Api qualified as PV1
import PlutusTx.Builtins qualified as Builtins
import Prettyprinter (Pretty (pretty))

deriving instance Serialise Language

instance Pretty Language where
  pretty PlutusV1 = "Plutus V1"
  pretty PlutusV2 = "Plutus V2"

-- | A script of some kind with its Plutus language version
data Versioned script = Versioned { unversioned :: script, version :: Language }
    deriving stock (Show, Eq, Ord, Functor, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise, NFData)

instance Pretty script => Pretty (Versioned script) where
    pretty Versioned{unversioned,version} = pretty unversioned <> " (" <> pretty version <> ")"

-- | Hash a 'PV1.Datum builtin data.
datumHash :: PV1.Datum -> PV1.DatumHash
datumHash = PV1.DatumHash . dataHash . PV1.getDatum

-- | Hash a 'PV1.Redeemer' builtin data.
redeemerHash :: PV1.Redeemer -> PV1.RedeemerHash
redeemerHash = PV1.RedeemerHash . dataHash . PV1.getRedeemer

-- | Hash a 'Builtins.BuiltinData'
dataHash :: Builtins.BuiltinData -> Builtins.BuiltinByteString
dataHash =
    Builtins.toBuiltin
    . Script.serialiseToRawBytes
    . Script.hashScriptData
    . toCardanoAPIData

-- | Convert a 'Builtins.BuiltinsData' value to a 'cardano-api' script
--   data value.
--
-- For why we depend on `cardano-api`,
-- see note [Hash computation of datums, redeemers and scripts]
toCardanoAPIData :: Builtins.BuiltinData -> Script.ScriptData
toCardanoAPIData = Script.fromPlutusData . Builtins.builtinDataToData

{- Note [Hash computation of datums, redeemers and scripts]

We have three options for computing the hash (each with advantages and drawbacks):

1- Depend on `cardano-api` and use it's `Scripts.hashScriptData` and `Scripts.hashScript`
functions.
The good: most simplest way to compute the hashes.
The bad: this package has an additional pretty large dependency.

2- Depend on `cardano-ledger` instead and use their `hashScriptData` and `hashScript`.
The good: smaller footprint than `cardano-api`.
The bad: a lower-lever library than `cardano-api`.

3- Depend on `cardano-crypto-class`, and reimplement ourselves the hashing functions
from `cardano-ledger`.
The good: the lowest dependency footprint.
The bad: code duplication.

However, we expect that most Plutus script devs depending on this package will
also probably depend on `cardano-api`, so the dependency on `cardano-api` should
(probably) be an non-issue.

If this becomes an issue, we'll change the implementation.
-}

