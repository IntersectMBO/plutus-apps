{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.Index.Internal where

import Prelude hiding (lookup)

import Cardano.Ledger.Alonzo.Rules.Utxos qualified as C.Ledger
import Cardano.Ledger.Alonzo.Tools qualified as C.Ledger
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Map qualified as Map
import Data.OpenApi.Schema qualified as OpenApi
import GHC.Generics (Generic)
import Ledger.Blockchain
import Ledger.Orphans ()
import Ledger.Params (EmulatorEra)
import Plutus.V1.Ledger.Scripts qualified as Scripts
import Plutus.V1.Ledger.Tx
import Prettyprinter (Pretty)
import Prettyprinter.Extras (PrettyShow (..))

-- | The UTxOs of a blockchain indexed by their references.
newtype UtxoIndex = UtxoIndex { getIndex :: Map.Map TxOutRef TxOut }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Semigroup, OpenApi.ToSchema, Monoid, Serialise)
    deriving anyclass (FromJSON, ToJSON, NFData)

-- | Create an index of all UTxOs on the chain.
initialise :: Blockchain -> UtxoIndex
initialise = UtxoIndex . unspentOutputs

instance Eq (C.Ledger.BasicFailure c) where
    _ == _ = True
instance Eq (C.Ledger.ScriptFailure c) where
    _ == _ = True

instance ToJSON (C.Ledger.BasicFailure c) where
    toJSON _ = toJSON "BasicFailure"
instance FromJSON (C.Ledger.BasicFailure c) where
    parseJSON = error "not implemented"
instance ToJSON (C.Ledger.ScriptFailure c) where
    toJSON _ = toJSON "ScriptFailure"
instance FromJSON (C.Ledger.ScriptFailure c) where
    parseJSON = error "not implemented"

instance ToJSON (C.Ledger.ApplyTxError EmulatorEra) where
    toJSON _ = toJSON "ApplyTxError"
instance FromJSON (C.Ledger.ApplyTxError EmulatorEra) where
    parseJSON = error "not implemented"

instance ToJSON (C.Ledger.UtxosPredicateFailure EmulatorEra) where
    toJSON _ = toJSON "UtxosPredicateFailure"
instance FromJSON (C.Ledger.UtxosPredicateFailure EmulatorEra) where
    parseJSON = error "not implemented"

-- | A reason why a transaction is invalid.
data ValidationError =
    TxOutRefNotFound TxOutRef
    -- ^ The transaction output consumed by a transaction input could not be found (either because it was already spent, or because
    -- there was no transaction with the given hash on the blockchain).
    | ScriptError Scripts.ScriptError
    -- ^ For pay-to-script outputs: evaluation of the validator script failed.
    | ApplyTxError (C.Ledger.ApplyTxError EmulatorEra)
    | UtxosPredicateFailure [C.Ledger.UtxosPredicateFailure EmulatorEra]
    | ScriptFailure (C.Ledger.ScriptFailure StandardCrypto)
    | BasicFailure (C.Ledger.BasicFailure StandardCrypto)
    deriving (Eq, Show, Generic)

instance FromJSON ValidationError
instance ToJSON ValidationError
deriving via (PrettyShow ValidationError) instance Pretty ValidationError

data ValidationPhase = Phase1 | Phase2 deriving (Eq, Show, Generic, FromJSON, ToJSON)
deriving via (PrettyShow ValidationPhase) instance Pretty ValidationPhase
type ValidationErrorInPhase = (ValidationPhase, ValidationError)
