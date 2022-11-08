{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.Index.Internal where

import Prelude hiding (lookup)

import Cardano.Ledger.Alonzo.Scripts (ExUnits)
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr)
import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Control.Lens (makeClassyPrisms)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Map qualified as Map
import Data.OpenApi.Schema qualified as OpenApi
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger.Blockchain
import Ledger.Orphans ()
import Ledger.Tx.Internal (TxOut)
import Plutus.V1.Ledger.Scripts qualified as Scripts
import Plutus.V1.Ledger.Tx qualified as PV1
import Prettyprinter (Pretty)
import Prettyprinter.Extras (PrettyShow (..))

-- | The UTxOs of a blockchain indexed by their references.
newtype UtxoIndex = UtxoIndex { getIndex :: Map.Map PV1.TxOutRef TxOut }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Semigroup, OpenApi.ToSchema, Monoid, Serialise)
    deriving anyclass (FromJSON, ToJSON, NFData)

-- | Create an index of all UTxOs on the chain.
initialise :: Blockchain -> UtxoIndex
initialise = UtxoIndex . unspentOutputs

-- | A reason why a transaction is invalid.
data ValidationError =
    TxOutRefNotFound PV1.TxOutRef
    -- ^ The transaction output consumed by a transaction input could not be found (either because it was already spent, or because
    -- there was no transaction with the given hash on the blockchain).
    | ScriptFailure Scripts.ScriptError
    -- ^ For pay-to-script outputs: evaluation of the validator script failed.
    | CardanoLedgerValidationError Text
    -- ^ An error from Cardano.Ledger validation
    deriving (Eq, Show, Generic)
makeClassyPrisms ''ValidationError

instance FromJSON ValidationError
instance ToJSON ValidationError
deriving via (PrettyShow ValidationError) instance Pretty ValidationError

data ValidationPhase = Phase1 | Phase2 deriving (Eq, Show, Generic, FromJSON, ToJSON)
deriving via (PrettyShow ValidationPhase) instance Pretty ValidationPhase
type ValidationErrorInPhase = (ValidationPhase, ValidationError)
type ValidationSuccess = Map.Map RdmrPtr ([Text], ExUnits)
