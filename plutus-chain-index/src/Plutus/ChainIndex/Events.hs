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

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, dupTChan, tryReadTChan)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Plutus.ChainIndex qualified as CI
import Plutus.ChainIndex.Lib (ChainSyncEvent (Resume, RollBackward, RollForward), EventsChan, RunRequirements,
                              runChainIndexDuringSync)

batchSize :: Int
batchSize = 15000

-- | 30s
period :: Int
period = 30_000_000

processEventsChan :: RunRequirements -> EventsChan -> IO ()
processEventsChan runReq eventsChan = void $ do
  chan <- liftIO $ atomically $ dupTChan eventsChan
  putStrLn "started"
  go chan
  where
    go chan = do
      events :: [ChainSyncEvent] <- readEventsFromTChan chan
      case events of
        firstBlock : rollForwardEvents -> do
          print $ show $ length rollForwardEvents
          void $ runChainIndexDuringSync runReq $ do
            let
              -- if the first block is 'RollForward' then process all events
              rollForwardEvents' = case firstBlock of
                (RollForward _ _) -> events
                _                 -> rollForwardEvents
              getBlock = \case
                (RollForward block _) -> Just block
                _                     -> Nothing
              blocks = catMaybes $ map getBlock rollForwardEvents'
            CI.appendBlocks blocks
            case firstBlock of
              (RollBackward point _) -> CI.rollback point
              (Resume point)         -> CI.resumeSync point
              _                      -> pure () -- ignore forward block
        [] -> putStrLn "empty list of events"
      go chan

-- | Read 'RollForward' events from the 'TChan' the until 'RollBackward' or 'Resume'.
readEventsFromTChan :: EventsChan -> IO [ChainSyncEvent]
readEventsFromTChan chan =
    let
      go combined 0 = pure combined
      go combined n = do
            eventM <- atomically $ tryReadTChan chan
            case eventM of
              Nothing    -> putStrLn "nothing here, waiting" >> threadDelay period >> go combined n
              Just event -> case event of
                (RollForward _ _) -> go (event : combined) (n - 1)
                _                 -> putStrLn "not RollForward!!!" >> pure (event : combined)
     in go [] batchSize
