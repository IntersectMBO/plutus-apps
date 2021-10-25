{-# LANGUAGE OverloadedStrings #-}

module BrowserConfig where

import           System.Environment (getArgs)
import           Test.WebDriver

firefoxConfig :: IO WDConfig
firefoxConfig = do
  args <- getArgs
  let nargs = length args
  let ci = if nargs > 0 then read (args!!0) else False
  let firefoxBin = if ci then "/opt/hostedtoolcache/firefox/latest/x64/firefox" else "/usr/bin/firefox"
  return $ useBrowser firefox { ffBinary=Just firefoxBin, ffAcceptInsecureCerts=Just True } defaultConfig
