{-# LANGUAGE OverloadedStrings #-}

module Elements where

import           Data.Text

{-
   Common
-}

body = "//body" :: Text
header  = "//span[text() = 'Plutus playground']" :: Text
helloWorldDemoLnk = "//a[text() = 'Hello, world']" :: Text
crowdFundingDemoLnk = "//a[text() = 'Crowd Funding']" :: Text

{-
   Editor
-}

feedbackHeader = "//div[@class='editor-feedback-header']" :: Text
helloWorldEditorText = "//div[@class='code-editor']/descendant::span[contains(text(), 'Hello') and contains(text(), 'world')]" :: Text
compileBtn = "//button[text() = 'Compile']" :: Text
simulateBtn = "//button[text() = 'Simulate']" :: Text

{-
   Simulator
-}

walletsHeader = "//h2[text()='Wallets']" :: Text
transactionsHeader = "//h2[text()='Transactions']" :: Text
evaluateBtn = "//button[text() = 'Evaluate']" :: Text

blockchainTransaction :: Integer -> Integer -> Text -- slot and transaction number
blockchainTransaction i j = "//div[@class='row blocks']//div[@class='card-header' and descendant::span[text()='Slot' and following-sibling::span[1][text()='" `append` (pack $ show i) `append` "']] and descendant::span[text()='Tx' and following-sibling::span[text()='" `append` (pack $ show j) `append` "']]]"

logsText :: Text -> Text
logsText t = "//div[@class='logs']//descendant::div[contains(text(),'" `append` t `append` "')]"
