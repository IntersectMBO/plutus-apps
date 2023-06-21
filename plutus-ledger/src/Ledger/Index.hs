{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An index of unspent transaction outputs, and some functions for validating
--   transactions using the index.
module Ledger.Index(
    -- * Types for transaction validation based on UTXO index
    UtxoIndex,
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
    createGenesisTransaction,
    genesisTxIn,
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
import Control.Lens ((&), (.~), (<&>))
import Data.Foldable (foldl')
import Data.Map qualified as Map
import Data.Set qualified as Set
import Ledger.Address (CardanoAddress)
import Ledger.Blockchain
import Ledger.Index.Internal
import Ledger.Orphans ()
import Ledger.Tx (CardanoTx (..), TxOut (..), getCardanoTxCollateralInputs, getCardanoTxProducedOutputs,
                  getCardanoTxProducedReturnCollateral, getCardanoTxSpentOutputs, outValue, txOutValue)
import Ledger.Tx.CardanoAPI (fromPlutusTxOut, toCardanoTxOutValue)
import Ledger.Tx.Internal qualified as Tx
import Ledger.Value.CardanoAPI (Value, lovelaceToValue)
import Plutus.Script.Utils.Ada (Ada)
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.V1.Ledger.Api qualified as PV1
import PlutusTx.Lattice ((\/))

-- | Create an index of all UTxOs on the chain.
initialise :: Blockchain -> UtxoIndex
initialise = (`insertBlock` mempty) . concat

-- | Update the index for the addition of a transaction.
insert :: CardanoTx -> UtxoIndex -> UtxoIndex
insert tx (C.UTxO unspent) = C.UTxO $
  (unspent `Map.withoutKeys` getCardanoTxSpentOutputs tx)
  `Map.union` (Tx.toCtxUTxOTxOut <$> getCardanoTxProducedOutputs tx)

-- | Update the index for the addition of only the collateral inputs of a failed transaction.
insertCollateral :: CardanoTx -> UtxoIndex -> UtxoIndex
insertCollateral tx (C.UTxO unspent) = C.UTxO $
    (unspent `Map.withoutKeys` (Set.fromList $ getCardanoTxCollateralInputs tx))
    `Map.union` (Tx.toCtxUTxOTxOut <$> getCardanoTxProducedReturnCollateral tx)

-- | Update the index for the addition of a block.
insertBlock :: Block -> UtxoIndex -> UtxoIndex
insertBlock blck i = foldl' (flip (eitherTx insertCollateral insert)) i blck

-- | Find an unspent transaction output by the 'TxOutRef' that spends it.
lookup :: C.TxIn -> UtxoIndex -> Maybe TxOut
lookup i index = case Map.lookup i $ C.unUTxO index of
    Just (C.TxOut aie tov tod rs) ->
        let tod' = case tod of
                    C.TxOutDatumNone                    -> C.TxOutDatumNone
                    C.TxOutDatumHash era scriptDataHash -> C.TxOutDatumHash era scriptDataHash
                    C.TxOutDatumInline era scriptData   -> C.TxOutDatumInline era scriptData
        in Just $ TxOut (C.TxOut aie tov tod' rs)
    Nothing -> Nothing

{- note [Minting of Ada]

'checkMintingAuthorised' will never allow a transaction that mints Ada.
Ada's currency symbol is the empty bytestring, and it can never be matched by a
validator script whose hash is its symbol.

Therefore 'checkMintingAuthorised' should not be applied to the first transaction in
the blockchain.

-}

-- | Adjust a single transaction output so it contains at least the minimum amount of Ada
-- and return the adjustment (if any) and the updated TxOut.
adjustTxOut :: Babbage.PParams (Babbage.BabbageEra StandardCrypto) -> TxOut -> ([C.Lovelace], Tx.TxOut)
adjustTxOut params txOut = do
    -- Increasing the ada amount can also increase the size in bytes, so start with a rough estimated amount of ada
    let withMinAdaValue = toCardanoTxOutValue $ txOutValue txOut \/ lovelaceToValue (minAdaTxOut params txOut)
    let txOutEstimate = txOut & outValue .~ withMinAdaValue
        minAdaTxOutEstimated' = minAdaTxOut params txOutEstimate
        missingLovelace = minAdaTxOutEstimated' - C.selectLovelace (txOutValue txOut)
    if missingLovelace > 0
    then
      let adjustedLovelace = toCardanoTxOutValue $ txOutValue txOut <> lovelaceToValue missingLovelace
      in ([missingLovelace], txOut & outValue .~ adjustedLovelace)
    else ([], txOut)

-- | Exact computation of the mimimum Ada required for a given TxOut.
-- TODO: Should be moved to cardano-api-extended once created
minAdaTxOut :: Babbage.PParams (Babbage.BabbageEra StandardCrypto) -> TxOut -> C.Lovelace
minAdaTxOut params txOut = let
  toLovelace = C.Lovelace . C.Ledger.unCoin
  initialValue = txOutValue txOut
  firstEstimate = toLovelace . C.Ledger.evaluateMinLovelaceOutput params $ fromPlutusTxOut txOut
  in -- if the estimate is above the initialValue, we run minAdaAgain, just to be sure that the
     -- new amount didn't change the TxOut size and requires more ada.
     if firstEstimate > C.selectLovelace initialValue
     then minAdaTxOut params . flip (outValue .~) txOut
            $ toCardanoTxOutValue $ lovelaceToValue firstEstimate \/ initialValue
     else firstEstimate

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

-- | cardano-ledger validation rules require the presence of inputs and
-- we have to provide a stub TxIn for the genesis transaction.
genesisTxIn :: C.TxIn
genesisTxIn = C.TxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53" (C.TxIx 40214)

createGenesisTransaction :: Map.Map CardanoAddress Value -> CardanoTx
createGenesisTransaction vals =
    let
        txBodyContent = Tx.emptyTxBodyContent
           { C.txIns = [ (genesisTxIn, C.BuildTxWith (C.KeyWitness C.KeyWitnessForSpending)) ]
           , C.txOuts = Map.toList vals <&> \(changeAddr, v) ->
                C.TxOut changeAddr (toCardanoTxOutValue v) C.TxOutDatumNone C.Api.ReferenceScriptNone
           }
        txBody = either (error . ("createGenesisTransaction: Can't create TxBody: " <>) . show) id $ C.makeTransactionBody txBodyContent
    in CardanoEmulatorEraTx $ C.Tx txBody []

