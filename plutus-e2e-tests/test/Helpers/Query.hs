{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Helpers.Query where

import Cardano.Api qualified as C
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (isInfixOf, sortBy)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Hedgehog (MonadTest)
import Hedgehog.Extras.Test qualified as HE
import Hedgehog.Extras.Test.Base qualified as H
import Helpers.Common (cardanoEraToShelleyBasedEra, toEraInCardanoMode)

-- | Find the first UTxO at address and return as TxIn. Used for txbody's txIns.
firstTxIn ::
  (MonadIO m, MonadTest m) =>
  C.CardanoEra era ->
  C.LocalNodeConnectInfo C.CardanoMode ->
  C.Address C.ShelleyAddr ->
  m C.TxIn
firstTxIn era = txInAtAddressByIndex era 0

-- | Find UTxO at address by index and return as TxIn. Used for txbody's txIns.
txInAtAddressByIndex ::
  (MonadIO m, MonadTest m) =>
  C.CardanoEra era ->
  Int ->
  C.LocalNodeConnectInfo C.CardanoMode ->
  C.Address C.ShelleyAddr ->
  m C.TxIn
txInAtAddressByIndex era idx localNodeConnectInfo address = do
  atM idx =<< txInsFromUtxo =<< findUTxOByAddress era localNodeConnectInfo address
  where
    atM :: (MonadTest m) => Int -> [a] -> m a
    atM i' l = return $ l !! i'

-- | Find the TxIn at address which is ada-only and has the most ada
adaOnlyTxInAtAddress ::
  (MonadIO m, MonadTest m) =>
  C.CardanoEra era ->
  C.LocalNodeConnectInfo C.CardanoMode ->
  C.Address C.ShelleyAddr ->
  m C.TxIn
adaOnlyTxInAtAddress era localNodeConnectInfo address = do
  utxo <- findUTxOByAddress era localNodeConnectInfo address
  return $ fst $ head $ sortByMostAda $ adaOnly $ Map.toList $ C.unUTxO utxo
  where
    adaOnly =
      filter
        ( \(_, C.TxOut _ (C.TxOutValue _ v) _ _) ->
            ((length $ C.valueToList v) == 1)
              && ((fst $ head $ C.valueToList v) == C.AdaAssetId)
        )
    sortByMostAda =
      sortBy
        ( \(_, C.TxOut _ (C.TxOutValue _ v1) _ _)
           (_, C.TxOut _ (C.TxOutValue _ v2) _ _) ->
              compare (snd $ head $ C.valueToList v2) (snd $ head $ C.valueToList v1)
        )

-- | Get TxIns from all UTxOs
txInsFromUtxo :: (MonadIO m) => C.UTxO era -> m [C.TxIn]
txInsFromUtxo utxos = do
  let (txIns, _) = unzip $ Map.toList $ C.unUTxO utxos
  return txIns

-- | Query ledger for UTxOs at address
findUTxOByAddress ::
  (MonadIO m, MonadTest m) =>
  C.CardanoEra era ->
  C.LocalNodeConnectInfo C.CardanoMode ->
  C.Address a ->
  m (C.UTxO era)
findUTxOByAddress era localNodeConnectInfo address =
  let query =
        C.QueryInShelleyBasedEra (cardanoEraToShelleyBasedEra era) $
          C.QueryUTxO $
            C.QueryUTxOByAddress $ Set.singleton (C.toAddressAny address)
   in H.leftFailM . H.leftFailM . liftIO $
        C.queryNodeLocalState localNodeConnectInfo Nothing $
          C.QueryInEra (toEraInCardanoMode era) query

-- | Get [TxIn] and total lovelace value for an address.
getAddressTxInsLovelaceValue ::
  (MonadIO m, MonadTest m) =>
  C.CardanoEra era ->
  C.LocalNodeConnectInfo C.CardanoMode ->
  C.Address a ->
  m ([C.TxIn], C.Lovelace)
getAddressTxInsLovelaceValue era con address = do
  utxo <- findUTxOByAddress era con address
  let (txIns, txOuts) = unzip $ Map.toList $ C.unUTxO utxo
      values = map (\case C.TxOut _ v _ _ -> C.txOutValueToLovelace v) txOuts
  pure (txIns, sum values)

-- | Get [TxIn] and value for an address (including assets).
getAddressTxInsValue ::
  (MonadIO m, MonadTest m) =>
  C.CardanoEra era ->
  C.LocalNodeConnectInfo C.CardanoMode ->
  C.Address a ->
  m ([C.TxIn], C.Value)
getAddressTxInsValue era con address = do
  utxo <- findUTxOByAddress era con address
  let (txIns, txOuts) = unzip $ Map.toList $ C.unUTxO utxo
      values = map (\case C.TxOut _ v _ _ -> C.txOutValueToValue v) txOuts
  pure (txIns, (mconcat values))

--TODO: loop timeout
waitForTxIdAtAddress ::
  (MonadIO m, MonadTest m) =>
  C.CardanoEra era ->
  C.LocalNodeConnectInfo C.CardanoMode ->
  C.Address C.ShelleyAddr ->
  C.TxId ->
  m ()
waitForTxIdAtAddress era localNodeConnectInfo address txId = do
  let loop = do
        txIns <- txInsFromUtxo =<< findUTxOByAddress era localNodeConnectInfo address
        let txIds = map (\(C.TxIn txId _) -> txId) txIns
        when (not $ txId `elem` txIds) loop
  loop

waitForTxInAtAddress :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo C.CardanoMode
  -> C.Address C.ShelleyAddr
  -> C.TxIn
  -> String -- temp debug text for intermittent timeout failure
  -> m ()
waitForTxInAtAddress era localNodeConnectInfo address txIn debugStr = do
  let timeoutSeconds = 90 :: Int
      loop i prevUtxo = do
        if i == 0
          then error ("waitForTxInAtAddress timeout. \n-- Debug --\nTest function: " ++ debugStr
                    ++ "\nAddress: " ++ show address ++ "\nTxIn: " ++ show txIn ++ "\nPrev UTxO: " ++ show prevUtxo)
          else HE.threadDelay 1000000
        utxos <- findUTxOByAddress era localNodeConnectInfo address
        when (Map.notMember txIn $ C.unUTxO utxos) (loop (pred i) (show utxos))
  loop timeoutSeconds ""

-- | Get tx out at address is for general use when txo is expected
getTxOutAtAddress :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo C.CardanoMode
  -> C.Address C.ShelleyAddr
  -> C.TxIn
  -> String -- temp debug text for intermittent timeout failure (waitForTxInAtAddress)
  -> m (C.TxOut C.CtxUTxO era)
getTxOutAtAddress era localNodeConnectInfo address txIn debugStr = do
    maybeTxOut <- getTxOutAtAddress' era localNodeConnectInfo address txIn debugStr
    return $ fromMaybe maybeTxOut
    where
      fromMaybe Nothing    = error $ "txIn " ++ show txIn ++ " is not at address " ++ show address
      fromMaybe (Just txo) = txo

-- | Maybe get tx out at address for asserting when it is not expected to be present
getTxOutAtAddress' :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo C.CardanoMode
  -> C.Address C.ShelleyAddr
  -> C.TxIn
  -> String -- temp debug text for intermittent timeout failure (waitForTxInAtAddress)
  -> m (Maybe (C.TxOut C.CtxUTxO era))
getTxOutAtAddress' era localNodeConnectInfo address txIn debugStr = do
  waitForTxInAtAddress era localNodeConnectInfo address txIn debugStr
  utxos <- findUTxOByAddress era localNodeConnectInfo address
  return $ Map.lookup txIn $ C.unUTxO utxos

txOutHasValue :: (MonadIO m)
  => C.TxOut C.CtxUTxO era
  -> C.Value
  -> m Bool
txOutHasValue (C.TxOut _ txOutValue _ _) tokenValue = do
  let value = C.txOutValueToValue txOutValue
  return $ isInfixOf (C.valueToList tokenValue) (C.valueToList value)
