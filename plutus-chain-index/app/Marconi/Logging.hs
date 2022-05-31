{-# LANGUAGE NamedFieldPuns #-}

module Marconi.Logging (logging) where

import Cardano.Api (Block (Block), BlockHeader (BlockHeader), BlockInMode (BlockInMode), CardanoMode,
                    ChainPoint (ChainPoint, ChainPointAtGenesis), ChainTip (ChainTip, ChainTipAtGenesis),
                    SlotNo (SlotNo))
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List qualified as List
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Plutus.Streaming (ChainSyncEvent (RollBackward, RollForward))
import Streaming (Of, Stream, effect)
import Streaming.Prelude qualified as S
import Text.Printf (printf)

data SyncStats = SyncStats
  { -- | Number of applied blocks
    syncStatsAppliedBlocks     :: !Int,
    -- | Timestamp of last few blocks for computing a progress rate
    syncStatsLastAppliedBlocks :: [UTCTime],
    -- | Number of rollbacks
    syncStatsLastRollback      :: !(Maybe UTCTime),
    -- | Timestamp of last printed message
    syncStatsLastMessage       :: !(Maybe UTCTime)
  }

data SyncState = NotSynchronising | Synchronising Double | Synchronised
  deriving (Show)

logging ::
  Stream (Of (ChainSyncEvent (BlockInMode CardanoMode))) IO r ->
  Stream (Of (ChainSyncEvent (BlockInMode CardanoMode))) IO r
logging s = effect $ do
  stats <- newIORef (SyncStats 0 [] Nothing Nothing)
  return $ S.chain (update stats) s
  where
    -- This approach keeps a list of the timestamps of the last numBlocks
    -- to compute an average rate of processed blocks. A list might not be
    -- the most efficient structure but UTCTime is also not the most
    -- efficient type.
    numBlocks :: Int
    numBlocks = 10

    minSecondsBetweenMsg :: NominalDiffTime
    minSecondsBetweenMsg = 10

    update :: IORef SyncStats -> ChainSyncEvent (BlockInMode CardanoMode) -> IO ()
    update statsRef (RollForward bim ct) = do
      let cp = case bim of (BlockInMode (Block (BlockHeader slotNo hash _blockNo) _txs) _eim) -> ChainPoint slotNo hash
      now <- getCurrentTime
      modifyIORef' statsRef $ \stats ->
        stats
          { syncStatsAppliedBlocks = syncStatsAppliedBlocks stats + 1,
            syncStatsLastAppliedBlocks = take numBlocks (now : syncStatsLastAppliedBlocks stats)
          }
      printMessage statsRef cp ct
    update statsRef (RollBackward cp ct) = do
      now <- getCurrentTime
      modifyIORef' statsRef $ \stats -> stats {syncStatsLastRollback = Just now}
      printMessage statsRef cp ct

    printMessage statsRef cp ct = do
      SyncStats {syncStatsAppliedBlocks, syncStatsLastAppliedBlocks, syncStatsLastRollback, syncStatsLastMessage} <- readIORef statsRef

      let syncStatus = case (cp, ct) of
            (_, ChainTipAtGenesis) ->
              NotSynchronising
            (ChainPointAtGenesis, _) ->
              Synchronising 0
            (ChainPoint (SlotNo chainPointSlot) _, ChainTip (SlotNo chainTipSlot) _header _blockNo)
              -- TODO: MAGIC number here. Is there a better number?
              -- 100 represents the number of slots before the
              -- node where we consider the chain-index to be synced.
              | chainTipSlot - chainPointSlot < 100 -> Synchronised
            (ChainPoint (SlotNo chainPointSlot) _, ChainTip (SlotNo chainTipSlot) _header _blockNo) ->
              let pct = (100 :: Double) * fromIntegral chainPointSlot / fromIntegral chainTipSlot
               in Synchronising pct

      let rate = case syncStatsLastAppliedBlocks of
            _ : _ : _ ->
              let timespan = diffUTCTime (List.head syncStatsLastAppliedBlocks) (List.last syncStatsLastAppliedBlocks)
               in Just (fromIntegral numBlocks / realToFrac timespan :: Double)
            _ ->
              Nothing

      let currentTipMsg = case syncStatus of
            NotSynchronising -> ""
            _                -> " Current point is " <> show cp <> "."

      let rateMsg = case rate of
            Nothing -> ""
            Just r  -> " Current rate: " <> printf "%.2g" r <> " blocks/s."

      let syncMsg = case syncStatus of
            NotSynchronising -> " Not synchronising."
            Synchronising p  -> printf " Synchornising (%0.2f%%)." p
            Synchronised     -> " Synchronised."

      let rollbackMsg = case syncStatsLastRollback of
            Nothing -> ""
            Just t  -> " Last rollback was on " <> show t <> "."

      now <- getCurrentTime

      let shouldPrint = case syncStatsLastMessage of
            Nothing -> True
            Just t
              | diffUTCTime now t > minSecondsBetweenMsg -> True
              | otherwise -> False

      when shouldPrint $ do
        modifyIORef' statsRef $ \stats -> stats {syncStatsLastMessage = Just now}
        putStrLn $
          "[" <> show now <> "]"
            <> " Processed "
            <> show syncStatsAppliedBlocks
            <> " blocks."
            <> rateMsg
            <> rollbackMsg
            <> syncMsg
            <> currentTipMsg
