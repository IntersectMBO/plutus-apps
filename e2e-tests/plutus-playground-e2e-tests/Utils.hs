{-# LANGUAGE OverloadedStrings #-}

module Utils where

import           Control.Monad                (void)
import           Data.CallStack
import           Data.Text                    (Text, unpack)
import           Prelude
import           Test.WebDriver
import           Test.WebDriver.Class
import           Test.WebDriver.Commands.Wait

assertTrue :: (WebDriver wd) => Bool -> String -> wd ()
assertTrue a e
         | a         = return ()
         | otherwise = unexpected e

assertURL :: (WebDriver wd) => String -> wd ()
assertURL s = do
    url <- getCurrentURL
    assertTrue (url == s) ("Actual URL: " ++ url ++ "  Expected URL: " ++ s)

assertText :: (WebDriver wd) => Element -> Text -> wd ()
assertText e et = do
    at <- getText e
    assertTrue (at == et) ("Actual element text: " ++ unpack at ++ "  Expected element text: " ++ unpack et)

getElem :: (WebDriver wd) => Text -> wd Element
getElem xp = findElem $ ByXPath xp

waitForElem :: (WebDriver wd) => Text -> wd Element
waitForElem xp = waitUntil 60 $ getElem xp -- 60s for slower compiling in CI

waitForElemText :: (WebDriver wd) => Element -> Text -> wd ()
waitForElemText e t = waitUntil 60 $ assertText e t -- 60s for slower compiling in CI

clickElem :: (WebDriver wd) => Text -> wd ()
clickElem xp = do
    e <- waitForElem xp
    click e

