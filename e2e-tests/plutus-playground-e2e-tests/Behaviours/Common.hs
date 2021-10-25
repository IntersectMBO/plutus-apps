{-# LANGUAGE OverloadedStrings #-}

module Behaviours.Common where

import           Control.Monad        (void)
import           Data.Text
import qualified Elements             as E
import           Test.WebDriver
import           Test.WebDriver.Class
import           Utils

playgroundUrl = "https://localhost:8009"

openPlayground :: (WebDriver wd) => wd ()
openPlayground = do
    openPage playgroundUrl
    void $ waitForElem E.header
    void $ waitForElem E.helloWorldDemoLnk

selectHelloWorldDemo :: (WebDriver wd) => wd ()
selectHelloWorldDemo = clickElem E.helloWorldDemoLnk

selectCrowdFundingDemo :: (WebDriver wd) => wd ()
selectCrowdFundingDemo = clickElem E.crowdFundingDemoLnk
