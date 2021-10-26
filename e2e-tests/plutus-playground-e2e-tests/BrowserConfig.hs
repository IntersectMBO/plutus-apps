{-# LANGUAGE OverloadedStrings #-}

module BrowserConfig where

import           Data.String
import           System.Environment (getArgs)
import           Test.WebDriver

-- May be useful in future if we want to configure display area:
--firefoxConfig = useBrowser firefox { ffBinary=Just firefoxBin, ffAcceptInsecureCerts=Just True } defaultConfig { wdCapabilities = defaultCaps { additionalCaps = [ ("moz:firefoxOptions", object [ ("args", Array (fromList [String "--headless", String "--window-size=1050,1080"])) ]) ] } }

firefoxConfig :: IO WDConfig
firefoxConfig = do
  args <- getArgs
  let nargs = length args
  let firefoxBin = if nargs > 0 then fromString (args!!0) else "/usr/bin/firefox"
  return $ useBrowser firefox { ffBinary=Just firefoxBin, ffAcceptInsecureCerts=Just True } defaultConfig
