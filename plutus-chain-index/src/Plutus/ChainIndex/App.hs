{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-| Main entry points to the chain index.
-}
module Plutus.ChainIndex.App(main, runMain) where

import Control.Exception (throwIO)
import Data.Aeson qualified as A
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Yaml qualified as Y
import Options.Applicative (execParser)
import Prettyprinter (Pretty (..))

import Cardano.BM.Configuration.Model qualified as CM

import Plutus.ChainIndex.CommandLine (AppConfig (..), Command (..), applyOverrides, cmdWithHelpParser)
import Plutus.ChainIndex.Compatibility (fromCardanoBlockNo)
import Plutus.ChainIndex.Config qualified as Config
import Plutus.ChainIndex.Lib (defaultChainSyncHandler, getTipSlot, showingProgress, storeFromBlockNo, syncChainIndex,
                              withRunRequirements)
import Plutus.ChainIndex.Logging qualified as Logging
import Plutus.ChainIndex.Server qualified as Server

main :: IO ()
main = do
  -- Parse comand line arguments.
  cmdConfig@AppConfig{acLogConfigPath, acConfigPath, acMinLogLevel, acCommand, acCLIConfigOverrides} <- execParser cmdWithHelpParser

  case acCommand of
    DumpDefaultConfig path ->
      A.encodeFile path Config.defaultConfig

    DumpDefaultLoggingConfig path ->
      Logging.defaultConfig >>= CM.toRepresentation >>= Y.encodeFile path

    StartChainIndex{} -> do
      -- Initialise logging
      logConfig <- maybe Logging.defaultConfig Logging.loadConfig acLogConfigPath
      for_ acMinLogLevel $ \ll -> CM.setMinSeverity logConfig ll

      -- Reading configuration file
      config <- applyOverrides acCLIConfigOverrides <$> case acConfigPath of
        Nothing -> pure Config.defaultConfig
        Just p  -> A.eitherDecodeFileStrict p >>=
          either (throwIO . Config.DecodeConfigException) pure

      putStrLn "\nCommand line config:"
      print cmdConfig

      putStrLn "\nLogging config:"
      CM.toRepresentation logConfig >>= print

      putStrLn "\nChain Index config:"
      print (pretty config)

      runMain logConfig config

runMain :: CM.Configuration -> Config.ChainIndexConfig -> IO ()
runMain logConfig config = do
  withRunRequirements logConfig config $ \runReq -> do

    putStr "\nThe tip of the local node: "
    slotNo <- getTipSlot config
    print slotNo

    syncHandler
      <- defaultChainSyncHandler runReq
        & storeFromBlockNo (fromCardanoBlockNo $ Config.cicStoreFrom config)
        & showingProgress

    putStrLn $ "Connecting to the node using socket: " <> Config.cicSocketPath config
    syncChainIndex config runReq syncHandler

    let port = show (Config.cicPort config)
    putStrLn $ "Starting webserver on port " <> port
    putStrLn $ "A Swagger UI for the endpoints are available at "
            <> "http://localhost:" <> port <> "/swagger/swagger-ui"
    Server.serveChainIndexQueryServer (Config.cicPort config) runReq

