{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Behaviours.Common      as Common
import qualified Behaviours.Editor      as Editor
import qualified Behaviours.Simulator   as Simulator
import qualified BrowserConfig          (firefoxConfig)
import           Control.Monad.IO.Class
import           System.Environment     (getArgs)
import           Test.WebDriver
import           Test.WebDriver.Class
import           Utils

main :: IO ()
main = do
  firefoxConfig <- BrowserConfig.firefoxConfig
  runTests firefoxConfig
  putStrLn "All tests pass!"

runTests :: WDConfig -> IO ()
runTests browserConfig = runSession browserConfig . closeOnException $ do

  setPageLoadTimeout 5000

  runHelloWorldTest
  liftIO $ putStrLn "Hello World test pass"
  runCrowdFundingTest
  liftIO $ putStrLn "Crowd Funding test pass"

  closeSession

runHelloWorldTest :: (WebDriver wd) => wd ()
runHelloWorldTest = do

  -- Given
  let textToInsert = "wonderful"
  Common.openPlayground
  Common.selectHelloWorldDemo

  -- When
  Editor.modifyEditorHelloWorldText textToInsert
  Editor.compile
  Editor.simulate
  Simulator.evaluate

  -- Then
  Simulator.checkLogsContainsText textToInsert

runCrowdFundingTest :: (WebDriver wd) => wd ()
runCrowdFundingTest = do

  -- Given
  Common.openPlayground
  Common.selectCrowdFundingDemo

  -- When
  Editor.compile
  Editor.simulate
  Simulator.evaluate

  -- Then
  Simulator.confirmCrowdFundingBlockchainTransactions
  -- TODO: confirm logs
