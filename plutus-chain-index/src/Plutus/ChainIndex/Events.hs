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
import Control.Monad (void)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Plutus.ChainIndex qualified as CI
import Plutus.ChainIndex.Lib (ChainSyncEvent (Resume, RollBackward, RollForward), EventsQueue, RunRequirements,
                              runChainIndexDuringSync)
import Plutus.ChainIndex.SyncStats (SyncLog, logProgress)
import Plutus.Monitoring.Util (PrettyObject (PrettyObject), convertLog, runLogEffects)
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime)

-- | How often do we check the queue
period :: Int
period = 5_000_000 -- 5s

-- | 'processEventsQueue' reads events from 'TBQueue', collects enough 'RollForward's to
-- append blocks at once.
processEventsQueue :: Trace IO (PrettyObject SyncLog) -> RunRequirements -> EventsQueue -> IO ()
processEventsQueue trace runReq eventsQueue = void $ do
  putStrLn "Starting processing of events"
  go []
  where
    go unprocessedEvents = do
      start <- getTime Monotonic
      eventsToProcess <-
        if null unprocessedEvents then do
          let
            waitUntilEvents = do
              isFull <- atomically $ isFullTBQueue eventsQueue
              if isFull then atomically $ flushTBQueue eventsQueue
              else threadDelay period >> waitUntilEvents
          waitUntilEvents
        else return unprocessedEvents
      case eventsToProcess of
        firstEvent : restEvents -> do
          -- rollback or resume
          void $ runChainIndexDuringSync runReq $ case firstEvent of
            (RollBackward point _) -> CI.rollback point
            (Resume point)         -> CI.resumeSync point
            _                      -> pure () -- ignore forward block
          -- append blocks
          let
            -- if the first event is 'RollForward' then process all events, otherwise only the tail
            eventsToProcess' = case firstEvent of
              (RollForward _ _) -> eventsToProcess
              _                 -> restEvents
            isRollForward = \case { (RollForward _ _) -> True; _ -> False }
            (rollForwardEvents, restUnprocessedEvents) = span isRollForward eventsToProcess'
            blocks = catMaybes $ rollForwardEvents <&> \case
              (RollForward block _) -> Just block
              _                     -> Nothing
          void $ runChainIndexDuringSync runReq $ CI.appendBlocks blocks
          -- we should include 'firstEvent' to processed events if it was a rollback or a resume
          let processedEvents = (if isRollForward firstEvent then [] else [firstEvent]) ++ rollForwardEvents
          end <- getTime Monotonic
          void $ runLogEffects (convertLog PrettyObject trace) $ logProgress processedEvents (diffTimeSpec end start)
          go restUnprocessedEvents
        [] -> putStrLn "The queue can't be empty"
      go []
