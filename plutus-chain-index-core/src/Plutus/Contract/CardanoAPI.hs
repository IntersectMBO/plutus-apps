{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-|

Interface to the transaction types from 'cardano-api'

-}
module Plutus.Contract.CardanoAPI(
    fromCardanoBlock
  , fromCardanoTx
  , setValidity
  , fromCardanoTxOut
  , fromCardanoTxOutRefScript
  , module Export
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Data.Either (fromRight)
import Data.List (sort)
import Ledger qualified as P
import Ledger.Tx.CardanoAPI as Export
import Plutus.ChainIndex.Types (ChainIndexTx (..), ChainIndexTxOut (..), ChainIndexTxOutputs (..), ReferenceScript (..))

fromCardanoBlock :: C.BlockInMode C.CardanoMode -> [ChainIndexTx]
fromCardanoBlock (C.BlockInMode (C.Block C.BlockHeader {} txs) eraInMode) =
  map (fromCardanoTx eraInMode) txs

-- | Convert a Cardano API tx of any given era to a Plutus chain index tx.
fromCardanoTx
  :: C.IsCardanoEra era
  => C.EraInMode era C.CardanoMode
  -> C.Tx era
  -> ChainIndexTx
fromCardanoTx eraInMode tx@(C.Tx txBody@(C.TxBody C.TxBodyContent{..}) _) =
    let txOutputs = map fromCardanoTxOut txOuts
        scriptMap = plutusScriptsFromTxBody txBody
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
        collateralOut = case txReturnCollateral of
          C.TxReturnCollateralNone     -> Nothing
          C.TxReturnCollateral _ txOut -> Just $ fromCardanoTxOut txOut

    in ChainIndexTx
            { _citxTxId = fromCardanoTxId (C.getTxId txBody)
            , _citxValidRange = fromCardanoValidityRange txValidityRange
            -- If the transaction is invalid, we use collateral inputs
            , _citxInputs = fmap ((`P.TxIn` Nothing) . fromCardanoTxIn) inputs
            -- No outputs if the one of scripts failed
            , _citxOutputs = if isTxScriptValid then ValidTx txOutputs
                                                else InvalidTx collateralOut
            , _citxData = datums
            , _citxRedeemers = redeemers
            , _citxScripts = scriptMap
            , _citxCardanoTx = Just $ CardanoTx tx eraInMode
            }

fromCardanoTxOut :: C.IsCardanoEra era => C.TxOut C.CtxTx era -> ChainIndexTxOut
fromCardanoTxOut (C.TxOut addr val datum refScript) =
    ChainIndexTxOut
        (fromRight (error "BabbageEra should be the latest era") $ C.eraCast C.BabbageEra addr)
        (C.txOutValueToValue val)
        (fromCardanoTxOutDatum datum)
        (fromCardanoTxOutRefScript refScript)

setValidity :: Bool -> C.Tx era -> C.Tx era
setValidity validity (C.Tx (C.ShelleyTxBody shelleyBasedEra txBody scripts dat aux _) era) =
  C.Tx (C.ShelleyTxBody shelleyBasedEra txBody scripts dat aux (toTxScriptValidity shelleyBasedEra validity)) era
setValidity _ tx = tx -- @setValidity@ only applies in Alonzo era (and newer)

fromCardanoTxOutRefScript :: C.ReferenceScript era -> ReferenceScript
fromCardanoTxOutRefScript = \case
    C.ReferenceScriptNone      -> ReferenceScriptNone
    C.ReferenceScript _ script -> ReferenceScriptInAnyLang script
