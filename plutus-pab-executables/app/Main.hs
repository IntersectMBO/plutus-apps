{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main
    ( main
    ) where

import CommandParser (AppOpts (..), NoConfigCommand (..), parseOptions)

import Cardano.BM.Configuration.Model qualified as CM
import Control.Monad.Logger (logErrorN, runStdoutLoggingT)
import Data.Text.Extras (tshow)
import Plutus.PAB.Monitoring.Monitoring qualified as LM
import Plutus.PAB.Types (PABError)

import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)

runNoConfigCommand ::
    NoConfigCommand
    -> IO ()
runNoConfigCommand = \case
    -- Get default logging configuration
    WriteDefaultConfig{outputFile} -> LM.defaultConfig >>= flip CM.exportConfiguration outputFile

main :: IO ()
main = do
    AppOpts { cmd } <- parseOptions

    -- execute parsed pab command and handle errors on failure
    result <- Right <$> runNoConfigCommand cmd
    either handleError (const exitSuccess) result

    where
        handleError (err :: PABError) = do
            runStdoutLoggingT $ (logErrorN . tshow) err
            exitWith (ExitFailure 1)
