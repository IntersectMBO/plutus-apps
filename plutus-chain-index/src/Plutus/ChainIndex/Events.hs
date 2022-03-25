{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plutus.ChainIndex.Events where

import Cardano.BM.Trace (Trace)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMQueue (flushTBMQueue, isFullTBMQueue)
import Control.Monad (forever, void)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Numeric.Natural (Natural)
import Plutus.ChainIndex qualified as CI
import Plutus.ChainIndex.Lib (ChainSyncEvent (Resume, RollBackward, RollForward), EventsQueue, RunRequirements,
                              runChainIndexDuringSync)
import Plutus.ChainIndex.SyncStats (SyncLog, logProgress)
import Plutus.Monitoring.Util (PrettyObject (PrettyObject), convertLog, runLogEffects)
import System.Clock (Clock (Monotonic), TimeSpec, diffTimeSpec, getTime)

-- | How often do we check the queue
period :: Int
period = 2_000_000 -- 2s

-- | We estimate the size of the event with the number of the transactions in the block.
-- By doing this we accumulate some number of blocks but with less than 'queueSize' number of transactions.
-- This approach helps to process blocks with a constant memory usage.
--
-- Just accumulating 'queueSize' blocks doesn't work as a block can have any number of transactions.
-- It works fine at the beginning of the chain but later blocks grow in their size and the memory
-- usage grows tremendously.
measureEventByTxs :: ChainSyncEvent -> Natural
measureEventByTxs = \case
  (RollForward (CI.Block _ transactions) _) -> fromIntegral $ length transactions
  _                                         -> 1

-- | 'processEventsQueue' reads events from 'TBQueue', collects enough 'RollForward's to
-- append blocks at once.
processEventsQueue :: Trace IO (PrettyObject SyncLog) -> RunRequirements -> EventsQueue -> IO ()
processEventsQueue trace runReq eventsQueue = forever $ do
  start <- getTime Monotonic
  eventsToProcess <- do
    let
      waitUntilEvents = do
        isFull <- atomically $ isFullTBMQueue eventsQueue
        if isFull then atomically $ flushTBMQueue eventsQueue
        else threadDelay period >> waitUntilEvents
    waitUntilEvents
  processEvents start eventsToProcess
  where
    processEvents :: TimeSpec -> [ChainSyncEvent] -> IO ()
    processEvents start events = case events of
      (Resume resumePoint) : (RollBackward backwardPoint _) : restEvents -> do
        void $ runChainIndexDuringSync runReq $ do
          CI.rollback backwardPoint
          CI.resumeSync resumePoint
        processEvents start restEvents
      rollForwardEvents -> do
          let
            blocks = catMaybes $ rollForwardEvents <&> \case
              (RollForward block _) -> Just block
              _                     -> Nothing
          void $ runChainIndexDuringSync runReq $ CI.appendBlocks blocks
          end <- getTime Monotonic
          void $ runLogEffects (convertLog PrettyObject trace) $ logProgress events (diffTimeSpec end start)
