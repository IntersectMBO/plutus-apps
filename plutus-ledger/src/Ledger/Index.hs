{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An index of unspent transaction outputs, and some functions for validating
--   transactions using the index.
module Ledger.Index(
    -- * Types for transaction validation based on UTXO index
    UtxoIndex(..),
    insert,
    insertCollateral,
    insertBlock,
    initialise,
    lookup,
    ValidationError(..),
    _TxOutRefNotFound,
    _ScriptFailure,
    _CardanoLedgerValidationError,
    ValidationSuccess,
    ValidationErrorInPhase,
    ValidationPhase(..),
    minFee,
    maxFee,
    minAdaTxOut,
    maxMinAdaTxOut,
    pubKeyTxIns,
    scriptTxIns,
    PV1.ExBudget(..),
    PV1.ExCPU(..),
    PV1.ExMemory(..),
    PV1.SatInt,
    ) where

import Prelude hiding (lookup)

import Control.Lens (Fold, folding)
import Control.Monad.Except (MonadError (..))
import Data.Foldable (foldl')
import Data.Map qualified as Map
import Ledger.Ada (Ada)
import Ledger.Ada qualified as Ada
import Ledger.Blockchain
import Ledger.Index.Internal
import Ledger.Orphans ()
import Ledger.Tx (CardanoTx (..), Tx, TxIn (TxIn, txInType), TxInType (ConsumePublicKeyAddress, ScriptAddress), TxOut,
                  TxOutRef, updateUtxoCollateral)
import Plutus.V1.Ledger.Api qualified as PV1
import Plutus.V1.Ledger.Value qualified as V

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
    TxIn{ txInType = Just ScriptAddress{} } -> True
    _                                       -> False

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

-- | Minimum transaction fee.
minFee :: Tx -> V.Value
minFee = const (Ada.lovelaceValueOf 10)

-- | TODO Should be calculated based on the maximum script size permitted on
-- the Cardano blockchain.
maxFee :: Ada
maxFee = Ada.lovelaceOf 1_000_000
