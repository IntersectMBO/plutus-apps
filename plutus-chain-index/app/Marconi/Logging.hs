{-# LANGUAGE NamedFieldPuns #-}

module Marconi.Logging (logging) where

import Cardano.Api (Block (Block), BlockHeader (BlockHeader), BlockInMode (BlockInMode), CardanoMode,
                    ChainPoint (ChainPoint, ChainPointAtGenesis), ChainTip (ChainTip, ChainTipAtGenesis),
                    SlotNo (SlotNo), serialiseToRawBytesHexText)
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime,
                  getCurrentTimeZone, utcToZonedTime)
import Plutus.Streaming (ChainSyncEvent (RollBackward, RollForward))
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

data SyncState = NotSynchronising | Synchronising Double | Synchronised
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
            Nothing -> ""
            Just t -> " Processed " <> show syncStatsAppliedBlocks <> " blocks in the last " <> formatTime defaultTimeLocale "%s" t <> " seconds."

      let rollbackMsg = case syncStatsLastRollback of
            Nothing -> ""
            Just t  -> " Last rollback was on " <> showTime t <> "."

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

      let syncMsg = case syncStatus of
            NotSynchronising -> " Not synchronising."
            Synchronising p  -> printf " Synchronising (%0.2f%%)." p
            Synchronised     -> " Synchronised."

      let currentTipMsg = case syncStatus of
            NotSynchronising -> ""
            _                -> " Current point is " <> prettyChainPoint cp <> "."

      let shouldPrint = case timeSinceLastMsg of
            Nothing -> True
            Just t
              | t > minSecondsBetweenMsg -> True
              | otherwise -> False

      when shouldPrint $ do
        putStrLn $
          "[" <> showTime now <> "]"
            <> blocksMsg
            <> rollbackMsg
            <> syncMsg
            <> currentTipMsg
        modifyIORef' statsRef $ \stats -> stats {syncStatsAppliedBlocks = 0, syncStatsLastMessage = Just now}

prettyChainPoint :: ChainPoint -> String
prettyChainPoint ChainPointAtGenesis = "genesis"
prettyChainPoint (ChainPoint (SlotNo wo) hash) = "slotNo " <> show wo <> " hash " <> T.unpack (serialiseToRawBytesHexText hash)
