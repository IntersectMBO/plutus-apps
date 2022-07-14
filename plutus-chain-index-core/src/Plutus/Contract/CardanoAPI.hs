{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-|

Interface to the transaction types from 'cardano-api'

-}
module Plutus.Contract.CardanoAPI(
    fromCardanoBlock
  , fromCardanoTx
  , setValidity
  , module Export
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Data.List (sort)
import Ledger qualified as P
import Ledger.Tx.CardanoAPI as Export
import Plutus.ChainIndex.Types (ChainIndexTx (..), ChainIndexTxOutputs (..))

fromCardanoBlock :: C.BlockInMode C.CardanoMode -> Either FromCardanoError [ChainIndexTx]
fromCardanoBlock (C.BlockInMode (C.Block C.BlockHeader {} txs) eraInMode) =
  Export.withIsCardanoEra eraInMode (traverse (fromCardanoTx eraInMode) txs)

-- | Convert a Cardano API tx of any given era to a Plutus chain index tx.
fromCardanoTx
  :: C.IsCardanoEra era
  => C.EraInMode era C.CardanoMode
  -> C.Tx era
  -> Either FromCardanoError ChainIndexTx
fromCardanoTx eraInMode tx@(C.Tx txBody@(C.TxBody C.TxBodyContent{..}) _) = do
    txOutputs <- traverse fromCardanoTxOut txOuts
    let scriptMap = plutusScriptsFromTxBody txBody
        isTxScriptValid = fromTxScriptValidity txScriptValidity
        (datums, redeemers) = scriptDataFromCardanoTxBody txBody
        -- We need to sort the inputs as the order is important
        -- to find the corresponding redeemers
        inputs = sort $
          if isTxScriptValid
            then fst <$> txIns
            else case txInsCollateral of
                   C.TxInsCollateralNone     -> []
                   C.TxInsCollateral _ txins -> txins

    pure ChainIndexTx
            { _citxTxId = fromCardanoTxId (C.getTxId txBody)
            , _citxValidRange = fromCardanoValidityRange txValidityRange
            -- If the transaction is invalid, we use collateral inputs
            , _citxInputs = fmap ((`P.TxIn` Nothing) . fromCardanoTxIn) inputs
            -- No outputs if the one of scripts failed
            , _citxOutputs = if isTxScriptValid then ValidTx txOutputs
                                                else InvalidTx
            , _citxData = datums
            , _citxRedeemers = redeemers
            , _citxScripts = scriptMap
            , _citxCardanoTx = Just $ SomeTx tx eraInMode
            }

setValidity :: Bool -> C.Tx era -> C.Tx era
setValidity validity (C.Tx (C.ShelleyTxBody C.ShelleyBasedEraAlonzo txBody scripts dat aux _) era) =
  C.Tx (C.ShelleyTxBody C.ShelleyBasedEraAlonzo txBody scripts dat aux (toTxScriptValidity validity)) era
setValidity _ tx = tx -- @setValidity@ only applies in Alonzo era (and newer)
