{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.Logging (logging) where

import Cardano.Api (Block (Block), BlockHeader (BlockHeader), BlockInMode (BlockInMode), CardanoMode,
                    ChainPoint (ChainPoint), ChainTip (ChainTip), SlotNo (SlotNo, unSlotNo))
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime,
                  getCurrentTimeZone, utcToZonedTime)
import Marconi.Orphans ()
import Plutus.Streaming (ChainSyncEvent (RollBackward, RollForward))
import Prettyprinter (Pretty (pretty), (<+>))
import Streaming (Of, Stream, effect)
import Streaming.Prelude qualified as S
import Text.Printf (printf)

data SyncStats = SyncStats
  { -- | Number of applied blocks since last message
    syncStatsAppliedBlocks :: !Int,
    -- | Number of rollbacks
    syncStatsLastRollback  :: !(Maybe UTCTime),
    -- | Timestamp of last printed message
    syncStatsLastMessage   :: !(Maybe UTCTime)
  }

data SyncState = NotSynchronising | Synchronising SlotNo SlotNo Double | Synchronised
  deriving (Show)

logging ::
  Stream (Of (ChainSyncEvent (BlockInMode CardanoMode))) IO r ->
  Stream (Of (ChainSyncEvent (BlockInMode CardanoMode))) IO r
logging s = effect $ do
  stats <- newIORef (SyncStats 0 Nothing Nothing)
  return $ S.chain (update stats) s
  where
    minSecondsBetweenMsg :: NominalDiffTime
    minSecondsBetweenMsg = 10

    update :: IORef SyncStats -> ChainSyncEvent (BlockInMode CardanoMode) -> IO ()
    update statsRef (RollForward bim ct) = do
      let cp = case bim of (BlockInMode (Block (BlockHeader slotNo hash _blockNo) _txs) _eim) -> ChainPoint slotNo hash
      modifyIORef' statsRef $ \stats ->
        stats {syncStatsAppliedBlocks = syncStatsAppliedBlocks stats + 1}
      printMessage statsRef cp ct
    update statsRef (RollBackward cp ct) = do
      now <- getCurrentTime
      modifyIORef' statsRef $ \stats -> stats {syncStatsLastRollback = Just now}
      printMessage statsRef cp ct

    printMessage statsRef cp ct = do
      SyncStats {syncStatsAppliedBlocks, syncStatsLastRollback, syncStatsLastMessage} <- readIORef statsRef

      now <- getCurrentTime
      timeZone <- getCurrentTimeZone

      let showTime = formatTime defaultTimeLocale "%F %T" . utcToZonedTime timeZone

      let timeSinceLastMsg = diffUTCTime now <$> syncStatsLastMessage

      let blocksMsg = case timeSinceLastMsg of
            Nothing -> mempty
            Just t -> "Processed" <+> pretty syncStatsAppliedBlocks <+> "blocks in the last" <+> pretty (formatTime defaultTimeLocale "%s" t) <+> "seconds."

      let rollbackMsg = case syncStatsLastRollback of
            Nothing -> mempty
            Just t  -> "Last rollback was on" <+> pretty (showTime t) <> "."

      let syncStatus = case (cp, ct) of
            (ChainPoint (SlotNo chainPointSlot) _, ChainTip (SlotNo chainTipSlot) _header _blockNo)
              -- TODO: MAGIC number here. Is there a better number?
              -- 100 represents the number of slots before the
              -- node where we consider the chain-index to be synced.
              | chainTipSlot - chainPointSlot < 100 -> Synchronised
            (ChainPoint chainPointSlotNo _, ChainTip chainTipSlotNo _header _blockNo) ->
              let pct = (100 :: Double) * fromIntegral (unSlotNo chainPointSlotNo) / fromIntegral (unSlotNo chainTipSlotNo)
               in Synchronising chainPointSlotNo chainTipSlotNo pct
            _ ->
              NotSynchronising

      let syncMsg = case syncStatus of
            NotSynchronising -> "Starting."
            Synchronising chainPointSlot chainTipSlot p -> pretty (printf "Synchronising. Current slot %d out of %d (%0.2f%%)" (unSlotNo chainPointSlot) (unSlotNo chainTipSlot) p :: String)
            Synchronised -> "Synchronised. Chain tip is" <+> pretty ct

      let shouldPrint = case timeSinceLastMsg of
            Nothing -> True
            Just t
              | t > minSecondsBetweenMsg -> True
              | otherwise -> False

      when shouldPrint $ do
        print $
          "[" <> pretty (showTime now) <> "]"
            <+> blocksMsg
            <+> rollbackMsg
            <+> syncMsg
        modifyIORef' statsRef $ \stats -> stats {syncStatsAppliedBlocks = 0, syncStatsLastMessage = Just now}
