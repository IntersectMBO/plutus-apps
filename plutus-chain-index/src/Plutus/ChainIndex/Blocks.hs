{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plutus.ChainIndex.Blocks where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, dupTChan)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Plutus.ChainIndex qualified as CI
import Plutus.ChainIndex.Effects (appendBlocks)
import Plutus.ChainIndex.Lib (BlocksChan, RunRequirements, readNFromTChan, runChainIndexDuringSync)

batchSize :: Int
batchSize = 100

-- | 30s
period :: Int
period = 5_000_000

processBlockChan :: RunRequirements -> BlocksChan -> IO ()
processBlockChan runReq blocksChan = void $ do
    chan <- liftIO $ atomically $ dupTChan blocksChan
    go chan
  where
    go chan = do
      liftIO $ threadDelay period
      blocks :: [CI.ChainSyncBlock] <- liftIO $ readNFromTChan batchSize chan
      liftIO $ print $ show $ length blocks
      void $ runChainIndexDuringSync runReq $ appendBlocks blocks
      go chan
