{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main
    ( main
    ) where

import Data.Proxy
import DemoContract (DemoContract)
import Options.Applicative
import Plutus.PAB.Run.PSGenerator qualified as PSGenerator

parseOptions :: IO FilePath
parseOptions = customExecParser
    (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
    (info (helper <*> psGenOutputDirParser) idm)

psGenOutputDirParser :: Parser FilePath
psGenOutputDirParser = option str
    (long "output-dir" <>
     metavar "OUTPUT_DIR" <>
     help "Output directory to write PureScript files to.")

main :: IO ()
main = do
    psGenOutputDir <- parseOptions
    PSGenerator.generateAPIModule (Proxy :: Proxy DemoContract) psGenOutputDir
