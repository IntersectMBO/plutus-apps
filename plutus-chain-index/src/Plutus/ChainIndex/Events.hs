{-# LANGUAGE DataKinds           #-}
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
import Data.Maybe (catMaybes, isJust)
import Numeric.Natural (Natural)
import Plutus.ChainIndex qualified as CI
import Plutus.ChainIndex.Lib (ChainSyncEvent (Resume, RollBackward, RollForward), EventsQueue, RunRequirements,
                              runChainIndexDuringSync)
import Plutus.ChainIndex.SyncStats (SyncLog, getSyncState, isSyncStateSynced, logProgress)
import Plutus.ChainIndex.Types (tipAsPoint)
import Plutus.Monitoring.Util (PrettyObject (PrettyObject), convertLog, runLogEffects)
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime)

-- | How often do we check the queue
period :: Int
period = 2_000_000 -- 2s

-- | We estimate the size of the event with the number of the transactions in the block.
-- By doing this we accumulate some number of blocks but with less than 'queueSize' number of transactions.
-- This approach helps to process blocks with a constant memory usage.
--
-- However, once we are in sync with the node, we want to process every block
-- instead of batches of blocks so that we can update the database as frequently
-- as possible.
--
-- Just accumulating 'queueSize' blocks doesn't work as a block can have any number of transactions.
-- It works fine at the beginning of the chain but later blocks grow in their size and the memory
-- usage grows tremendously.
measureEventQueueSizeByTxs :: Natural -> ChainSyncEvent -> Natural
measureEventQueueSizeByTxs maxQueueSize (RollForward (CI.Block syncTip transactions) nodeTip) =
    let syncState = getSyncState (tipAsPoint syncTip) (tipAsPoint nodeTip)
        txLen = fromIntegral $ length transactions
     in if isSyncStateSynced syncState
           then maxQueueSize + 1
           else txLen
measureEventQueueSizeByTxs maxQueueSize _ = maxQueueSize + 1 -- to handle resume and rollback asap

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
  processEvents eventsToProcess
  end <- getTime Monotonic
  void $ runLogEffects (convertLog PrettyObject trace) $ logProgress eventsToProcess (diffTimeSpec end start)
  where
    processEvents :: [ChainSyncEvent] -> IO ()
    processEvents [] = pure ()
    processEvents events@( e : restEvents ) = case e of
      (Resume resumePoint) -> do
        void $ runChainIndexDuringSync runReq $ CI.resumeSync resumePoint
        processEvents restEvents

      (RollBackward backwardPoint _) -> do
        void $ runChainIndexDuringSync runReq $ CI.rollback backwardPoint
        processEvents restEvents

      (RollForward _ _) -> do
        let getBlock = \case
              (RollForward block _) -> Just block
              _                     -> Nothing
            isRollForwardEvt = isJust . getBlock
            (rollForwardEvents, restEvents') = span isRollForwardEvt events
            blocks = catMaybes $ map getBlock rollForwardEvents
        void $ runChainIndexDuringSync runReq $ CI.appendBlocks blocks
        processEvents restEvents'
