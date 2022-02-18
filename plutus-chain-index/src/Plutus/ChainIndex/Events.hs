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
import Control.Concurrent.STM (atomically, dupTChan, tryReadTChan)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Plutus.ChainIndex qualified as CI
import Plutus.ChainIndex.Lib (ChainSyncEvent (Resume, RollBackward, RollForward), EventsChan, RunRequirements,
                              runChainIndexDuringSync)
import Plutus.ChainIndex.SyncStats (SyncLog, logProgress)
import Plutus.Monitoring.Util (PrettyObject (PrettyObject), convertLog, runLogEffects)

processEventsChan :: Trace IO (PrettyObject SyncLog) -> RunRequirements -> EventsChan -> Int -> Int -> IO ()
processEventsChan trace runReq eventsChan period batchSize = void $ do
  chan <- liftIO $ atomically $ dupTChan eventsChan
  putStrLn "Starting processing of events"
  go chan
  where
    go chan = do
      events :: [ChainSyncEvent] <- readEventsFromTChan chan period batchSize
      case events of
        firstBlock : rollForwardEvents -> do
          void $ runChainIndexDuringSync runReq $ do
            let
              -- if the first block is 'RollForward' then process all events
              -- otherwise only the tail
              rollForwardEvents' = case firstBlock of
                (RollForward _ _) -> events
                _                 -> rollForwardEvents
              blocks = catMaybes $ rollForwardEvents' <&> \case
                (RollForward block _) -> Just block
                _                     -> Nothing
            CI.appendBlocks blocks
            case firstBlock of
              (RollBackward point _) -> CI.rollback point
              (Resume point)         -> CI.resumeSync point
              _                      -> pure () -- ignore forward block
          -- logging
          void $ runLogEffects (convertLog PrettyObject trace) $ logProgress events period
        [] -> putStrLn "The list of events is empty"
      go chan

-- | Read 'RollForward' events from the 'TChan' until either:
-- 1. Collected the 'batchSize' number of events
-- 2. 'RollBackward' or 'Resume' is met
readEventsFromTChan :: EventsChan -> Int -> Int -> IO [ChainSyncEvent]
readEventsFromTChan chan period batchSize =
    let
      go combined 0 = pure combined
      go combined n = do
            eventM <- atomically $ tryReadTChan chan
            case eventM of
              Nothing    -> do
                -- the chain is empty, waiting
                threadDelay period
                go combined n
              Just event -> case event of
                (RollForward _ _) -> go (event : combined) (n - 1)
                _                 -> pure (event : combined)
     in go [] batchSize
