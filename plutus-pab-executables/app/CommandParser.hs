{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module CommandParser (parseOptions, AppOpts(..), NoConfigCommand(..)) where

import Cardano.BM.Data.Severity (Severity (..))
import Data.Aeson qualified as JSON
import GHC.Generics (Generic)
import Options.Applicative (CommandFields, Mod, Parser, argument, command, customExecParser, disambiguate, flag,
                            fullDesc, help, helper, idm, info, long, metavar, option, prefs, progDesc, short,
                            showHelpOnEmpty, showHelpOnError, str, subparser, value)

data NoConfigCommand = WriteDefaultConfig -- ^ Write default logging configuration
          { outputFile :: !FilePath -- ^ Path to write configuration to
          }
    deriving stock (Show, Eq, Generic)
    deriving anyclass JSON.ToJSON

data AppOpts = AppOpts { minLogLevel   :: Maybe Severity
                       , logConfigPath :: Maybe FilePath
                       , cmd           :: NoConfigCommand
                       }

parseOptions :: IO AppOpts
parseOptions = customExecParser
            (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
            (info (helper <*> commandLineParser) idm)

logLevelFlag :: Parser (Maybe Severity)
logLevelFlag =
    flag
        Nothing
        (Just Debug)
        (short 'v' <> long "verbose" <> help "Enable debugging output.")

commandLineParser :: Parser AppOpts
commandLineParser =
        AppOpts <$> logLevelFlag
                <*> logConfigFileParser
                <*> commandParser

logConfigFileParser :: Parser (Maybe FilePath)
logConfigFileParser =
    option
        (Just <$> str)
        (long "log-config" <>
         metavar "LOG_CONFIG_FILE" <>
         help "Logging config file location." <> value Nothing)

commandParser :: Parser NoConfigCommand
commandParser =
    subparser $
    mconcat
        [ defaultConfigParser
        ]

defaultConfigParser :: Mod CommandFields NoConfigCommand
defaultConfigParser =
    command "default-logging-config" $
    flip info (fullDesc <> progDesc "Write the default logging configuration YAML to a file") $ do
        outputFile <-
            argument
                str
                (metavar "OUTPUT_FILE" <>
                 help "Output file to write logging config YAML to.")
        pure $ WriteDefaultConfig {outputFile}
