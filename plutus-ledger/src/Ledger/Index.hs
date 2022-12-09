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
    maxFee,
    adjustTxOut,
    minAdaTxOut,
    minAdaTxOutEstimated,
    minLovelaceTxOutEstimated,
    maxMinAdaTxOut,
    pubKeyTxIns,
    scriptTxIns,
    PV1.ExBudget(..),
    PV1.ExCPU(..),
    PV1.ExMemory(..),
    PV1.SatInt,
    ) where

import Prelude hiding (lookup)

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C.Api
import Cardano.Ledger.Babbage qualified as Babbage
import Cardano.Ledger.Babbage.PParams qualified as Babbage
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Control.Lens (Fold, folding, (&), (.~))
import Control.Monad.Except (MonadError (..))
import Data.Foldable (foldl')
import Data.Map qualified as Map
import Ledger.Blockchain
import Ledger.Index.Internal
import Ledger.Orphans ()
import Ledger.Tx (CardanoTx (..), ToCardanoError, TxIn (TxIn, txInType),
                  TxInType (ConsumePublicKeyAddress, ScriptAddress), TxOut (getTxOut), TxOutRef, outValue, txOutValue,
                  updateUtxoCollateral)
import Ledger.Tx.CardanoAPI (toCardanoTxOutValue)
import Ledger.Value.CardanoAPI ()
import Plutus.Script.Utils.Ada (Ada)
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.V1.Ledger.Api qualified as PV1
import PlutusTx.Lattice ((\/))

-- | Create an index of all UTxOs on the chain.
initialise :: Blockchain -> UtxoIndex
initialise = UtxoIndex . unspentOutputs

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

-- | Adjust a single transaction output so it contains at least the minimum amount of Ada
-- and return the adjustment (if any) and the updated TxOut.
adjustTxOut :: (Babbage.PParams (Babbage.BabbageEra StandardCrypto)) -> TxOut -> Either ToCardanoError ([C.Lovelace], TxOut)
adjustTxOut params txOut = do
    -- Increasing the ada amount can also increase the size in bytes, so start with a rough estimated amount of ada
    let withMinAdaValue = toCardanoTxOutValue $ txOutValue txOut \/ C.lovelaceToValue (minAdaTxOut params txOut)
    let txOutEstimate = txOut & outValue .~ withMinAdaValue
        minAdaTxOutEstimated' = minAdaTxOut params txOutEstimate
        missingLovelace = minAdaTxOutEstimated' - C.selectLovelace (txOutValue txOut)
    if missingLovelace > 0
    then
      let adjustedLovelace = toCardanoTxOutValue $ txOutValue txOut <> C.lovelaceToValue missingLovelace
      in pure ([missingLovelace], txOut & outValue .~ adjustedLovelace)
    else pure ([], txOut)

-- | Exact computation of the mimimum Ada required for a given TxOut.
-- TODO: Should be moved to cardano-api-extended once created
minAdaTxOut :: (Babbage.PParams (Babbage.BabbageEra StandardCrypto)) -> TxOut -> C.Lovelace
minAdaTxOut params txOut = let
  toLovelace = C.Lovelace . C.Ledger.unCoin
  initialValue = txOutValue txOut
  fromPlutusTxOut = C.Api.toShelleyTxOut C.Api.ShelleyBasedEraBabbage . C.Api.toCtxUTxOTxOut . getTxOut
  firstEstimate = toLovelace . C.Ledger.evaluateMinLovelaceOutput params $ fromPlutusTxOut txOut
  in -- if the estimate is above the initialValue, we run minAdaAgain, just to be sure that the
     -- new amount didn't change the TxOut size and requires more ada.
     if firstEstimate > C.selectLovelace initialValue
     then minAdaTxOut params . flip (outValue .~) txOut
            $ toCardanoTxOutValue $ C.lovelaceToValue firstEstimate \/ initialValue
     else firstEstimate

-- minAdaTxOutParams

{-# INLINABLE minAdaTxOutEstimated #-}
{- | Provide a reasonable estimate of the mimimum of Ada required for a TxOut.

   An exact estimate of the the mimimum of Ada in a TxOut is determined by two things:
     - the `PParams`, more precisely its 'coinPerUTxOWord' parameter.
     - the size of the 'TxOut'.
 In many situations though, we need to determine a plausible value for the minimum of Ada needed for a TxOut
 without knowing much of the 'TxOut'.
 This function provides a value big enough to balance UTxOs without
 a large inlined data (larger than a hash) nor a complex val with a lot of minted values.
 It's superior to the lowest minimum needed for an UTxO, as the lowest value require no datum.
 An estimate of the minimum required Ada for each tx output.
-}
minAdaTxOutEstimated :: Ada
minAdaTxOutEstimated = Ada.lovelaceOf minTxOut

minLovelaceTxOutEstimated :: C.Lovelace
minLovelaceTxOutEstimated = C.Lovelace minTxOut

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

-- | TODO Should be calculated based on the maximum script size permitted on
-- the Cardano blockchain.
maxFee :: Ada
maxFee = Ada.lovelaceOf 1_000_000
