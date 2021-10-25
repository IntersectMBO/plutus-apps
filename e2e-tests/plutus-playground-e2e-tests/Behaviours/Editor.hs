{-# LANGUAGE OverloadedStrings #-}

module Behaviours.Editor where

import           Control.Monad        (void)
import           Data.Text
import qualified Elements             as E
import           Test.WebDriver
import           Test.WebDriver.Class
import           Utils

wonderfulText = "wonderful" :: Text
notCompiledText = "Not compiled" :: Text
codeChangedText = "Code changed since last compilation" :: Text
compilingText = "Compiling ..." :: Text
compilationSuccessfulText = "Compilation successful" :: Text

checkCompilerStatus :: (WebDriver wd) => Text -> wd Element
checkCompilerStatus t = do
    feedbackHeaderElem <- getElem E.feedbackHeader
    assertText feedbackHeaderElem t
    return feedbackHeaderElem

waitForCompilerToSucceed :: (WebDriver wd) => wd ()
waitForCompilerToSucceed = void $ checkCompilerStatus compilationSuccessfulText

modifyEditorHelloWorldText :: (WebDriver wd) => Text -> wd ()
modifyEditorHelloWorldText t = do
    helloWorldTextElem <- waitForElem E.helloWorldEditorText
    checkCompilerStatus compilationSuccessfulText -- it's already compiled becuse it's an example?
    click helloWorldTextElem
    bodyElem <- getElem E.body
    sendKeys t bodyElem
    void $ checkCompilerStatus codeChangedText

compile :: (WebDriver wd) => wd ()
compile = do
  clickElem E.compileBtn
  compilerStatusElem <- checkCompilerStatus compilingText
  waitForElemText compilerStatusElem compilationSuccessfulText

simulate :: (WebDriver wd) => wd ()
simulate = do
  clickElem E.simulateBtn
  void $ waitForElem E.walletsHeader

