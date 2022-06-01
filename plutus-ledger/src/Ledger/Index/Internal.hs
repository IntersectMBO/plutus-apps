{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ledger.Index.Internal where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Crypto (StandardCrypto)

import Prelude hiding (lookup)

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Map qualified as Map
import Data.OpenApi.Schema qualified as OpenApi
import GHC.Generics (Generic)
import Ledger.Crypto
import Ledger.Orphans ()
import Plutus.V1.Ledger.Scripts qualified as Scripts
import Plutus.V1.Ledger.Slot qualified as Slot
import Plutus.V1.Ledger.Tx
import Plutus.V1.Ledger.Value qualified as V
import Prettyprinter (Pretty)
import Prettyprinter.Extras (PrettyShow (..))

-- | The UTxOs of a blockchain indexed by their references.
newtype UtxoIndex = UtxoIndex { getIndex :: Map.Map TxOutRef TxOut }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Semigroup, OpenApi.ToSchema, Monoid, Serialise)
    deriving anyclass (FromJSON, ToJSON, NFData)

type EmulatorEra = AlonzoEra StandardCrypto


-- | A reason why a transaction is invalid.
data ValidationError =
    InOutTypeMismatch TxIn TxOut
    -- ^ A pay-to-pubkey output was consumed by a pay-to-script input or vice versa, or the 'TxIn' refers to a different public key than the 'TxOut'.
    | TxOutRefNotFound TxOutRef
    -- ^ The transaction output consumed by a transaction input could not be found (either because it was already spent, or because
    -- there was no transaction with the given hash on the blockchain).
    | InvalidScriptHash Scripts.Validator Scripts.ValidatorHash
    -- ^ For pay-to-script outputs: the validator script provided in the transaction input does not match the hash specified in the transaction output.
    | InvalidDatumHash Scripts.Datum Scripts.DatumHash
    -- ^ For pay-to-script outputs: the datum provided in the transaction input does not match the hash specified in the transaction output.
    | MissingRedeemer RedeemerPtr
    -- ^ For scripts that take redeemers: no redeemer was provided for this script.
    | InvalidSignature PubKey Signature
    -- ^ For pay-to-pubkey outputs: the signature of the transaction input does not match the public key of the transaction output.
    | ValueNotPreserved V.Value V.Value
    -- ^ The amount spent by the transaction differs from the amount consumed by it.
    | NegativeValue Tx
    -- ^ The transaction produces an output with a negative value.
    | ValueContainsLessThanMinAda Tx TxOut V.Value
    -- ^ The transaction produces an output with a value containing less than the minimum required Ada.
    | NonAdaFees Tx
    -- ^ The fee is not denominated entirely in Ada.
    | ScriptFailure Scripts.ScriptError
    -- ^ For pay-to-script outputs: evaluation of the validator script failed.
    | CurrentSlotOutOfRange Slot.Slot
    -- ^ The current slot is not covered by the transaction's validity slot range.
    | SignatureMissing PubKeyHash
    -- ^ The transaction is missing a signature
    | MintWithoutScript Scripts.MintingPolicyHash
    -- ^ The transaction attempts to mint value of a currency without running
    --   the currency's minting policy.
    | TransactionFeeTooLow V.Value V.Value
    -- ^ The transaction fee is lower than the minimum acceptable fee.
    | CardanoLedgerValidationError String
    -- ^ An error from Cardano.Ledger validation
    deriving (Eq, Show, Generic)

instance FromJSON ValidationError
instance ToJSON ValidationError
deriving via (PrettyShow ValidationError) instance Pretty ValidationError

data ValidationPhase = Phase1 | Phase2 deriving (Eq, Show, Generic, FromJSON, ToJSON)
deriving via (PrettyShow ValidationPhase) instance Pretty ValidationPhase
type ValidationErrorInPhase = (ValidationPhase, ValidationError)
