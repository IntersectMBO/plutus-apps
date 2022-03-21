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
import Control.Concurrent.STM (atomically, flushTBQueue, isFullTBQueue)
import Control.Monad (forever, void)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Plutus.ChainIndex qualified as CI
import Plutus.ChainIndex.Lib (ChainSyncEvent (Resume, RollBackward, RollForward), EventsQueue, RunRequirements,
                              runChainIndexDuringSync)
import Plutus.ChainIndex.SyncStats (SyncLog, logProgress)
import Plutus.Monitoring.Util (PrettyObject (PrettyObject), convertLog, runLogEffects)
import System.Clock (Clock (Monotonic), TimeSpec, diffTimeSpec, getTime)

-- | How often do we check the queue
period :: Int
period = 5_000_000 -- 5s

-- | 'processEventsQueue' reads events from 'TBQueue', collects enough 'RollForward's to
-- append blocks at once.
processEventsQueue :: Trace IO (PrettyObject SyncLog) -> RunRequirements -> EventsQueue -> IO ()
processEventsQueue trace runReq eventsQueue = forever $ do
  start <- getTime Monotonic
  eventsToProcess <- do
    let
      waitUntilEvents = do
        isFull <- atomically $ isFullTBQueue eventsQueue
        if isFull then atomically $ flushTBQueue eventsQueue
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
