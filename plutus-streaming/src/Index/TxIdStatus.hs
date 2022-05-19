{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Index.TxIdStatus where

import Cardano.Api (Block (Block), BlockHeader (BlockHeader), BlockInMode (BlockInMode), BlockNo (BlockNo), CardanoMode)
import Cardano.Api qualified as C
import Control.Lens.Operators
import Control.Monad (forM_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid (Last (Last), Sum (Sum, getSum))
import Data.Sequence (Seq, ViewL (..))
import Data.Sequence qualified as Seq
import Database.SQLite.Simple (execute, execute_)
import Index.VSplit (SplitIndex, getBuffer, handle, storage)
import Index.VSqlite (SqliteIndex)
import Index.VSqlite qualified as Ix
import Ledger.TxId (TxId)
import Plutus.ChainIndex.Tx (ChainIndexTx (_citxTxId))
import Plutus.ChainIndex.Types (BlockNumber (BlockNumber),
                                TxConfirmedState (TxConfirmedState, blockAdded, timesConfirmed, validity),
                                TxValidity (TxValid))
import Plutus.Contract.CardanoAPI (fromCardanoTx)
import Plutus.Streaming (ChainSyncEvent (RollForward), SimpleChainSyncEvent)

type TxStatusIndex = SqliteIndex SimpleChainSyncEvent () TxId (Maybe TxConfirmedState)

openIx :: FilePath -> IO TxStatusIndex
openIx path =
  fromJust <$> Ix.newBoxed query store onInsert 2000 2000 path

-- Ignore notifications for now
onInsert :: TxStatusIndex -> SimpleChainSyncEvent -> IO [()]
onInsert _ _ = pure []

-- No one will query this for now.
query :: TxStatusIndex -> TxId -> [SimpleChainSyncEvent] -> IO (Maybe TxConfirmedState)
query _ _ _ = pure Nothing

store :: TxStatusIndex -> IO ()
store ix = do
  -- bufferedEvents <- getBuffer (ix ^. storage)
  let c           = ix ^. handle
      -- bufferedTxs = foldTxs $ getTxs . getBlocks <$> bufferedEvents
  execute_ c "CREATE TABLE IF NOT EXISTS tx_state (txid TEXT PRIMARY KEY, confirmations INTEGER)"
  -- execute_ handle "BEGIN"
  -- forM_ (Map.assocs bufferedTxs) $
  --   \(txid, v) -> execute handle "INSERT INTO tx_state (txid, confirmations) VALUES (?, ?)" (show txid, getSum $ timesConfirmed v)
  -- This will really work your SSD to death, and it is not very useful, since
  -- all txs that are persisted are settled.
  -- execute handle "UPDATE tx_state SET confirmations = confirmations + ?" (Only $ Map.size bufferedTxs)
  -- execute_ handle "COMMIT"

getBlocks :: SimpleChainSyncEvent -> BlockInMode CardanoMode
getBlocks (RollForward block _tip) = block
getBlocks _                        = error "This should never happen"

-- We won't have any rollbacks in the buffered events since those
-- blocks have settled
getTxs :: BlockInMode CardanoMode -> (BlockNo, [TxId])
getTxs (BlockInMode (Block header transactions) era) =
  case era of
    C.ByronEraInCardanoMode   -> go header transactions era
    C.ShelleyEraInCardanoMode -> go header transactions era
    C.AllegraEraInCardanoMode -> go header transactions era
    C.MaryEraInCardanoMode    -> go header transactions era
    C.AlonzoEraInCardanoMode  -> go header transactions era
  where
    go :: forall era. C.IsCardanoEra era
       => C.BlockHeader
       -> [C.Tx era]
       -> C.EraInMode era C.CardanoMode
       -> (BlockNo, [TxId])
    go (BlockHeader _ _ blockNo) txs era' =
      (blockNo, _citxTxId <$> catMaybes (either (const Nothing) Just . fromCardanoTx era' <$> txs))

foldTxs :: [(BlockNo, [TxId])] -> Map TxId TxConfirmedState
foldTxs bs = snd $ foldl go (0, Map.empty) bs
  where
    go :: (Int, Map TxId TxConfirmedState)
       -> (BlockNo, [TxId])
       -> (Int, Map TxId TxConfirmedState)
    go (confirmations, acc) (_, []) = (confirmations, acc)
    go (confirmations, acc) (blockNo@(BlockNo no), tx : txs) =
      let acc' = Map.insert tx (TxConfirmedState { timesConfirmed = Sum confirmations
                                                 , blockAdded = Last (Just $ BlockNumber no)
                                                 , validity = Last (Just TxValid)
                                                 })
                                 acc
      in go (confirmations + 1, acc') (blockNo, txs)
