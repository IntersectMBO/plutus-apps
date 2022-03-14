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

-- | 'processEventsQueue' reads events from 'TBQueue', collects enough 'RollForward's to
-- append blocks at once.
processEventsQueue :: Trace IO (PrettyObject SyncLog) -> RunRequirements -> EventsQueue -> Int -> IO ()
processEventsQueue trace runReq eventsQueue period = void $ do
  putStrLn "Starting processing of events"
  go []
  where
    go unprocessedEvents = do
      isFull <- atomically $ isFullTBQueue eventsQueue
      if isFull then do
        events :: [ChainSyncEvent] <- atomically $ flushTBQueue eventsQueue
        let eventsToProcess = unprocessedEvents ++ events
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
            let processedEvents = [x | x <- [firstEvent], not $ isRollForward x] ++ rollForwardEvents
            void $ runLogEffects (convertLog PrettyObject trace) $ logProgress processedEvents
            go restUnprocessedEvents
          [] -> putStrLn "The queue can't be empty"
      else threadDelay period >> go unprocessedEvents
