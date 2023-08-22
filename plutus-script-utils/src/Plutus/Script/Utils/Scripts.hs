{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
This module contains functions related to versioning scripts and BuiltinData, or more specifially,
'Datum's and 'Redeemer's. These functions do not depend on a particular version of Plutus.
-}
module Plutus.Script.Utils.Scripts
    ( -- * Plutus language versioning
      Language (..)
    , Versioned (..)
    , Script (..)
    , Validator (..)
    , mkValidatorScript
    , unValidatorScript
    , MintingPolicy (..)
    , mkMintingPolicyScript
    , unMintingPolicyScript
    , StakeValidator (..)
    , mkStakeValidatorScript
    , unStakeValidatorScript
      -- * Script hashing
    , ValidatorHash (..)
    , MintingPolicyHash (..)
    , StakeValidatorHash (..)
    , scriptHash
    , validatorHash
    , mintingPolicyHash
    , stakeValidatorHash
      -- * Script utilities
    , scriptCurrencySymbol
      -- * Script data hashes
    , PV1.Datum
    , PV1.DatumHash
    , PV1.Redeemer
    , PV1.RedeemerHash
    , datumHash
    , redeemerHash
    , dataHash
      -- * Address utilities
    , toScriptAddress
    , fromCardanoHash
    , mkValidatorCardanoAddress
    ) where

import Cardano.Api qualified as C.Api
import Cardano.Api.Shelley qualified as C.Api
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1, PlutusV2, PlutusV3))
import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import GHC.Generics (Generic)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Bytes (LedgerBytes (LedgerBytes))
import PlutusTx (CompiledCode, makeLift)
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.Builtins qualified as Builtins
import Prettyprinter (Pretty (pretty))
import Prettyprinter.Extras (PrettyShow (PrettyShow))

deriving instance Serialise Language

instance Pretty Language where
  pretty PlutusV1 = "Plutus V1"
  pretty PlutusV2 = "Plutus V2"
  pretty PlutusV3 = "Plutus V3"

-- | A script of some kind with its Plutus language version
data Versioned script = Versioned { unversioned :: script, version :: Language }
    deriving stock (Show, Eq, Ord, Functor, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise)

instance Pretty script => Pretty (Versioned script) where
    pretty Versioned{unversioned,version} = pretty unversioned <> " (" <> pretty version <> ")"

-- | Hash a 'Versioned' 'Script'
scriptHash :: Versioned Script -> PV1.ScriptHash
scriptHash = fromCardanoHash . cardanoScriptHash

-- | Transform a Cardano Script hash in a Plutus Script hash
fromCardanoHash :: C.Api.ScriptHash -> PV1.ScriptHash
fromCardanoHash =
    PV1.ScriptHash
    . Builtins.toBuiltin
    . C.Api.serialiseToRawBytes

cardanoScriptHash :: Versioned Script -> C.Api.ScriptHash
cardanoScriptHash (Versioned (Script script) lang) = case lang of
  PlutusV1 -> C.Api.hashScript . C.Api.PlutusScript C.Api.PlutusScriptV1 $ C.Api.PlutusScriptSerialised script
  PlutusV2 -> C.Api.hashScript . C.Api.PlutusScript C.Api.PlutusScriptV2 $ C.Api.PlutusScriptSerialised script
  PlutusV3 -> C.Api.hashScript . C.Api.PlutusScript C.Api.PlutusScriptV3 $ C.Api.PlutusScriptSerialised script

-- | Hash a 'Versioned' 'Validator' script.
validatorHash :: Versioned Validator -> ValidatorHash
validatorHash =
    ValidatorHash
  . PV1.getScriptHash
  . scriptHash
  . fmap getValidator

-- | Hash a 'Versioned' 'MintingPolicy' script.
mintingPolicyHash :: Versioned MintingPolicy -> MintingPolicyHash
mintingPolicyHash =
    MintingPolicyHash
  . PV1.getScriptHash
  . scriptHash
  . fmap getMintingPolicy

-- | Hash a 'Versioned' 'StakeValidator' script.
stakeValidatorHash :: Versioned StakeValidator -> StakeValidatorHash
stakeValidatorHash =
    StakeValidatorHash
  . PV1.getScriptHash
  . scriptHash
  . fmap getStakeValidator

{-# INLINABLE scriptCurrencySymbol #-}
-- | The 'CurrencySymbol' of a 'MintingPolicy'.
scriptCurrencySymbol :: Versioned MintingPolicy -> PV1.CurrencySymbol
scriptCurrencySymbol scrpt =
    let (MintingPolicyHash hsh) = mintingPolicyHash scrpt in PV1.CurrencySymbol hsh

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
    . C.Api.serialiseToRawBytes
    . C.Api.hashScriptDataBytes
    . toCardanoAPIData

-- | Convert a 'Builtins.BuiltinsData' value to a 'cardano-api' script
--   data value.
--
-- For why we depend on `cardano-api`,
-- see note [Hash computation of datums, redeemers and scripts]
toCardanoAPIData :: Builtins.BuiltinData -> C.Api.HashableScriptData
toCardanoAPIData = C.Api.unsafeHashableScriptData . C.Api.fromPlutusData . Builtins.builtinDataToData

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

-- | 'Validator' is a wrapper around 'Script's which are used as validators in transaction outputs.
newtype Validator = Validator { getValidator :: Script }
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Serialise)
  deriving Pretty via (PrettyShow Validator)

instance Show Validator where
    show = const "Validator { <script> }"

-- | 'MintingPolicy' is a wrapper around 'Script's which are used as validators for minting constraints.
newtype MintingPolicy = MintingPolicy { getMintingPolicy :: Script }
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Serialise)
  deriving Pretty via (PrettyShow MintingPolicy)

instance Show MintingPolicy where
    show = const "MintingPolicy { <script> }"

-- | 'StakeValidator' is a wrapper around 'Script's which are used as validators for withdrawals and stake address certificates.
newtype StakeValidator = StakeValidator { getStakeValidator :: Script }
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Serialise)
  deriving Pretty via (PrettyShow MintingPolicy)

instance Show StakeValidator where
    show = const "StakeValidator { <script> }"

newtype Script = Script { unScript :: PV1.SerialisedScript }
  deriving stock (Eq, Ord, Generic)
  deriving Serialise via PV1.SerialisedScript

instance Show Script where
    showsPrec _ _ = showString "<Script>"

-- | Script runtime representation of a @Digest SHA256@.
newtype ValidatorHash =
    ValidatorHash { getValidatorHash :: Builtins.BuiltinByteString }
    deriving (IsString, Show, Pretty) via LedgerBytes
    deriving stock (Generic)
    deriving newtype (Eq, Ord)

-- | Script runtime representation of a @Digest SHA256@.
newtype MintingPolicyHash =
    MintingPolicyHash { getMintingPolicyHash :: Builtins.BuiltinByteString }
    deriving (IsString, Show, Pretty) via LedgerBytes
    deriving stock (Generic)
    deriving newtype (Eq, Ord)

-- | Script runtime representation of a @Digest SHA256@.
newtype StakeValidatorHash =
    StakeValidatorHash { getStakeValidatorHash :: Builtins.BuiltinByteString }
    deriving (IsString, Show, Pretty) via LedgerBytes
    deriving stock (Generic)
    deriving newtype (Eq, Ord)

mkValidatorScript :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> Validator
mkValidatorScript = Validator . Script . serialiseCompiledCode

unValidatorScript :: Validator -> Script
unValidatorScript = getValidator

mkMintingPolicyScript :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> MintingPolicy
mkMintingPolicyScript = MintingPolicy . Script . serialiseCompiledCode

unMintingPolicyScript :: MintingPolicy -> Script
unMintingPolicyScript = getMintingPolicy

mkStakeValidatorScript :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> StakeValidator
mkStakeValidatorScript = StakeValidator . Script . serialiseCompiledCode

unStakeValidatorScript :: StakeValidator -> Script
unStakeValidatorScript = getStakeValidator


toScriptAddress :: C.Api.NetworkId -> Versioned Script -> C.Api.AddressInEra C.Api.BabbageEra
toScriptAddress networkId script = C.Api.makeShelleyAddressInEra
  networkId
  (C.Api.PaymentCredentialByScript (cardanoScriptHash script))
  C.Api.NoStakeAddress

mkValidatorCardanoAddress :: C.Api.NetworkId -> Versioned Validator -> C.Api.AddressInEra C.Api.BabbageEra
mkValidatorCardanoAddress networkId = toScriptAddress networkId . fmap getValidator


makeLift ''ValidatorHash
makeLift ''MintingPolicyHash
makeLift ''StakeValidatorHash
