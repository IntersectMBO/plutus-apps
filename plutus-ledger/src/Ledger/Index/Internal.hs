{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.Index.Internal where

import Prelude hiding (lookup)

import Cardano.Api qualified as C
import Cardano.Ledger.Alonzo.Scripts (ExUnits)
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr)
import Control.Lens (makeClassyPrisms)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger.Orphans ()
import Plutus.V1.Ledger.Scripts qualified as Scripts
import Prettyprinter (Pretty)
import Prettyprinter.Extras (PrettyShow (..))

-- | The UTxOs of a blockchain indexed by their references.
type UtxoIndex = C.UTxO C.BabbageEra

deriving newtype instance Semigroup (C.UTxO era)
deriving newtype instance Monoid (C.UTxO era)

-- | A reason why a transaction is invalid.
data ValidationError =
    TxOutRefNotFound C.TxIn
    -- ^ The transaction output consumed by a transaction input could not be found (either because it was already spent, or because
    -- there was no transaction with the given hash on the blockchain).
    | ScriptFailure Scripts.ScriptError
    -- ^ For pay-to-script outputs: evaluation of the validator script failed.
    | CardanoLedgerValidationError Text
    -- ^ An error from Cardano.Ledger validation
    | MaxCollateralInputsExceeded
    -- ^ Balancing failed, it needed more than the maximum number of collateral inputs
    deriving (Eq, Show, Generic)
makeClassyPrisms ''ValidationError

instance FromJSON ValidationError
instance ToJSON ValidationError
deriving via (PrettyShow ValidationError) instance Pretty ValidationError

data ValidationPhase = Phase1 | Phase2 deriving (Eq, Show, Generic, FromJSON, ToJSON)
deriving via (PrettyShow ValidationPhase) instance Pretty ValidationPhase
type ValidationErrorInPhase = (ValidationPhase, ValidationError)
type ValidationSuccess = Map.Map RdmrPtr ([Text], ExUnits)
