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

-- | An index of unspent transaction outputs, and some functions for validating
--   transactions using the index.
module Ledger.Index(
    -- * Types for transaction validation based on UTXO index
    ValidationMonad,
    ValidationCtx(..),
    UtxoIndex(..),
    insert,
    insertCollateral,
    insertBlock,
    initialise,
    lookup,
    ValidationError(..),
    ValidationErrorInPhase,
    ValidationPhase(..),
    InOutMatch(..),
    minFee,
    maxFee,
    minAdaTxOut,
    minLovelaceTxOut,
    maxMinAdaTxOut,
    pubKeyTxIns,
    scriptTxIns,
    -- * Script validation events
    ScriptType(..),
    ScriptValidationEvent(..),
    Api.ExBudget(..),
    Api.ExCPU(..),
    Api.ExMemory(..),
    Api.SatInt,
    ValidatorMode(..),
    getScript
    ) where

import Cardano.Api (Lovelace (..))
import Prelude hiding (lookup)

import Control.Lens (Fold, folding)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Writer (MonadWriter)
import Data.Foldable (foldl')
import Data.Map qualified as Map
import Ledger.Blockchain
import Ledger.Crypto
import Ledger.Index.Internal
import Ledger.Orphans ()
import Ledger.Params (Params)
import Ledger.Tx (CardanoTx (..), updateUtxoCollateral)
import Plutus.V1.Ledger.Ada (Ada)
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Api qualified as Api
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Tx hiding (pubKeyTxIns, scriptTxIns, updateUtxoCollateral)
import Plutus.V1.Ledger.TxId
import Plutus.V1.Ledger.Value qualified as V

-- | Context for validating transactions. We need access to the unspent
--   transaction outputs of the blockchain, and we can throw 'ValidationError's.
type ValidationMonad m = (MonadReader ValidationCtx m, MonadError ValidationError m, MonadWriter [ScriptValidationEvent] m)

data ValidationCtx = ValidationCtx { vctxIndex :: UtxoIndex, vctxParams :: Params }

-- | Update the index for the addition of a transaction.
insert :: CardanoTx -> UtxoIndex -> UtxoIndex
insert tx = UtxoIndex . updateUtxo tx . getIndex

-- | Update the index for the addition of only the collateral inputs of a failed transaction.
insertCollateral :: CardanoTx -> UtxoIndex -> UtxoIndex
insertCollateral tx = UtxoIndex . updateUtxoCollateral tx . getIndex

-- | Update the index for the addition of a block.
insertBlock :: Block -> UtxoIndex -> UtxoIndex
insertBlock blck i = foldl' (flip (eitherTx insertCollateral insert)) i blck

-- | Find an unspent transaction output by the 'TxOutRef' that spends it.
lookup :: MonadError ValidationError m => TxOutRef -> UtxoIndex -> m TxOut
lookup i index = case Map.lookup i $ getIndex index of
    Just t  -> pure t
    Nothing -> throwError $ TxOutRefNotFound i

-- | Filter to get only the script inputs.
scriptTxIns :: Fold [TxIn] TxIn
scriptTxIns = (\x -> folding x) . filter $ \case
    TxIn{ txInType = Just ConsumeScriptAddress{} } -> True
    _                                              -> False

-- | Filter to get only the pubkey inputs.
pubKeyTxIns :: Fold [TxIn] TxIn
pubKeyTxIns = folding (filter (\TxIn{ txInType = t } -> t == Just ConsumePublicKeyAddress))

{- note [Minting of Ada]

'checkMintingAuthorised' will never allow a transaction that mints Ada.
Ada's currency symbol is the empty bytestring, and it can never be matched by a
validator script whose hash is its symbol.

Therefore 'checkMintingAuthorised' should not be applied to the first transaction in
the blockchain.

-}

-- | A matching pair of transaction input and transaction output, ensuring that they are of matching types also.
data InOutMatch =
    ScriptMatch
        TxOutRef
        Validator
        Redeemer
        Datum
    | PubKeyMatch TxId PubKey Signature
    deriving (Eq, Ord, Show)

{-# INLINABLE minAdaTxOut #-}
-- An estimate of the minimum required Ada for each tx output.
--
-- TODO: Should be removed.
minAdaTxOut :: Ada
minAdaTxOut = Ada.lovelaceOf minTxOut

{-# INLINABLE minTxOut #-}
minTxOut :: Integer
minTxOut = 2_000_000

{-# INLINABLE maxMinAdaTxOut #-}
{-
maxMinAdaTxOut = maxTxOutSize * coinsPerUTxOWord
coinsPerUTxOWord = 34_482
maxTxOutSize = utxoEntrySizeWithoutVal + maxValSizeInWords + dataHashSize
utxoEntrySizeWithoutVal = 27
maxValSizeInWords = 500
dataHashSize = 10

These values are partly protocol parameters-based, but since this is used in on-chain code
we want a constant to reduce code size.
-}
maxMinAdaTxOut :: Ada
maxMinAdaTxOut = Ada.lovelaceOf 18_516_834

-- Minimum required Lovelace for each tx output.
--
minLovelaceTxOut :: Lovelace
minLovelaceTxOut = Lovelace minTxOut

-- | Minimum transaction fee.
minFee :: Tx -> V.Value
minFee = const (Ada.lovelaceValueOf 10)

-- | TODO Should be calculated based on the maximum script size permitted on
-- the Cardano blockchain.
maxFee :: Ada
maxFee = Ada.lovelaceOf 1_000_000

data ValidatorMode = FullyAppliedValidators | UnappliedValidators
    deriving (Eq, Ord, Show)

-- | Get the script from a @ScriptValidationEvent@ in either fully applied or unapplied form.
getScript :: ValidatorMode -> ScriptValidationEvent -> Script
getScript FullyAppliedValidators ScriptValidationEvent{sveScript} = sveScript
getScript UnappliedValidators ScriptValidationEvent{sveType} =
    case sveType of
        ValidatorScript (Validator script) _    -> script
        MintingPolicyScript (MintingPolicy mps) -> mps
getScript _ ScriptValidationResultOnlyEvent{} = error "getScript: unexpected ScriptValidationResultOnlyEvent"
