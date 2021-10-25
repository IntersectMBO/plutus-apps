{-# LANGUAGE OverloadedStrings #-}

module Behaviours.Simulator where

import           Control.Monad        (void)
import           Data.Text
import qualified Elements             as E
import           Test.WebDriver
import           Test.WebDriver.Class
import           Utils

evaluate :: (WebDriver wd) => wd ()
evaluate = do
  clickElem E.evaluateBtn
  void $ waitForElem E.transactionsHeader

confirmBlockchainTransaction :: (WebDriver wd) => Integer -> Integer -> wd ()
confirmBlockchainTransaction i j = void $ waitForElem $ E.blockchainTransaction i j

confirmCrowdFundingBlockchainTransactions :: (WebDriver wd) => wd ()
confirmCrowdFundingBlockchainTransactions = do
   confirmBlockchainTransaction 0 0
   confirmBlockchainTransaction 1 0
   confirmBlockchainTransaction 1 1
   confirmBlockchainTransaction 1 2
   confirmBlockchainTransaction 40 0

checkLogsContainsText :: (WebDriver wd) => Text -> wd ()
checkLogsContainsText t = void $ waitForElem $ E.logsText t
