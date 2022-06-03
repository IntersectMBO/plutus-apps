{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.Logging (logging) where

import Cardano.Api (Block (Block), BlockHeader (BlockHeader), BlockInMode (BlockInMode), CardanoMode,
                    ChainPoint (ChainPoint), ChainTip (ChainTip), SlotNo (SlotNo))
import Cardano.BM.Trace (Trace, logInfo)
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Marconi.Orphans ()
import Plutus.Streaming (ChainSyncEvent (RollBackward, RollForward))
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutPretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)
import Streaming (Of, Stream, effect)
import Streaming.Prelude qualified as S
import Text.Printf (printf)

data SyncStats = SyncStats
  { -- | Number of applied blocks since last message
    syncStatsNumBlocks    :: !Int,
    -- | Number of rollbacks
    syncStatsNumRollbacks :: !Int,
    -- | Timestamp of last printed message
    syncStatsLastMessage  :: !(Maybe UTCTime)
  }

logging ::
  Trace IO Text ->
  Stream (Of (ChainSyncEvent (BlockInMode CardanoMode))) IO r ->
  Stream (Of (ChainSyncEvent (BlockInMode CardanoMode))) IO r
logging tracer s = effect $ do
  stats <- newIORef (SyncStats 0 0 Nothing)
  return $ S.chain (update stats) s
  where
    minSecondsBetweenMsg :: NominalDiffTime
    minSecondsBetweenMsg = 10

    update :: IORef SyncStats -> ChainSyncEvent (BlockInMode CardanoMode) -> IO ()
    update statsRef (RollForward bim ct) = do
      let cp = case bim of (BlockInMode (Block (BlockHeader slotNo hash _blockNo) _txs) _eim) -> ChainPoint slotNo hash
      modifyIORef' statsRef $ \stats ->
        stats {syncStatsNumBlocks = syncStatsNumBlocks stats + 1}
      printMessage statsRef cp ct
    update statsRef (RollBackward cp ct) = do
      modifyIORef' statsRef $ \stats ->
        stats {syncStatsNumRollbacks = syncStatsNumRollbacks stats + 1}
      printMessage statsRef cp ct

    printMessage statsRef cp ct = do
      SyncStats {syncStatsNumBlocks, syncStatsNumRollbacks, syncStatsLastMessage} <- readIORef statsRef

      now <- getCurrentTime

      let timeSinceLastMsg = diffUTCTime now <$> syncStatsLastMessage

      let blocksMsg = case timeSinceLastMsg of
            Nothing -> id
            Just t -> \k ->
              "Processed"
                <+> pretty syncStatsNumBlocks
                <+> "blocks in the last"
                <+> pretty (formatTime defaultTimeLocale "%s" t)
                <+> "seconds"
                <+> let rate = fromIntegral syncStatsNumBlocks / realToFrac t :: Double
                     in pretty (printf "(%.0f blocks/sec)." rate :: String)
                <+> k

      let rollbackMsg = case timeSinceLastMsg of
            Nothing -> id
            Just t -> \k ->
              ( case syncStatsNumRollbacks of
                  0 -> "No"
                  _ -> pretty syncStatsNumRollbacks
              )
                <+> "rollbacks in the last"
                <+> pretty (formatTime defaultTimeLocale "%s" t)
                <+> "seconds."
                <+> k

      let syncMsg = case (cp, ct) of
            (ChainPoint (SlotNo chainPointSlot) _, ChainTip (SlotNo chainTipSlot) _header _blockNo)
              -- TODO: MAGIC number here. Is there a better number?
              -- 100 represents the number of slots before the
              -- node where we consider the chain-index to be synced.
              | chainTipSlot - chainPointSlot < 100 ->
                "Synchronised."
            (ChainPoint (SlotNo chainPointSlotNo) _, ChainTip (SlotNo chainTipSlotNo) _header _blockNo) ->
              let pct = (100 :: Double) * fromIntegral chainPointSlotNo / fromIntegral chainTipSlotNo
               in pretty
                    ( printf
                        "Synchronising. Current slot %d out of %d (%0.2f%%)."
                        chainPointSlotNo
                        chainTipSlotNo
                        pct ::
                        String
                    )
            _ -> "Starting."

      let shouldPrint = case timeSinceLastMsg of
            Nothing -> True
            Just t
              | t > minSecondsBetweenMsg -> True
              | otherwise -> False

      when shouldPrint $ do
        logInfo tracer $
          renderStrict $
            layoutPretty defaultLayoutOptions $
              syncMsg <+> (blocksMsg $ rollbackMsg $ "Chain tip is" <+> pretty ct <> ".")
        modifyIORef' statsRef $ \stats ->
          stats
            { syncStatsNumBlocks = 0,
              syncStatsNumRollbacks = 0,
              syncStatsLastMessage = Just now
            }
