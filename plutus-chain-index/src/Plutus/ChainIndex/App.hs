{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| Main entry points to the chain index.
-}
module Plutus.ChainIndex.App(main, runMain, runMainWithLog) where

import Control.Exception (throwIO)
import Data.Aeson qualified as A
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Yaml qualified as Y
import Options.Applicative (execParser)
import Prettyprinter (Pretty (pretty))

import Cardano.BM.Configuration.Model qualified as CM

import Cardano.BM.Setup (setupTrace_)
import Cardano.BM.Trace (Trace)
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM.TBMQueue (newTBMQueueIO)
import Plutus.ChainIndex.CommandLine (AppConfig (AppConfig, acCLIConfigOverrides, acCommand, acConfigPath, acLogConfigPath, acMinLogLevel),
                                      Command (DumpDefaultConfig, DumpDefaultLoggingConfig, StartChainIndex),
                                      applyOverrides, cmdWithHelpParser)
import Plutus.ChainIndex.Compatibility (fromCardanoBlockNo)
import Plutus.ChainIndex.Config qualified as Config
import Plutus.ChainIndex.Events (measureEventQueueSizeByTxs, processEventsQueue)
import Plutus.ChainIndex.Lib (getTipSlot, storeChainSyncHandler, storeFromBlockNo, syncChainIndex, withRunRequirements)
import Plutus.ChainIndex.Logging qualified as Logging
import Plutus.ChainIndex.Server qualified as Server
import Plutus.ChainIndex.SyncStats (SyncLog)
import Plutus.Monitoring.Util (PrettyObject)
import System.Exit (exitFailure)

main :: IO ()
main = do
  -- Parse comand line arguments.
  cmdConfig@AppConfig{acLogConfigPath, acConfigPath, acMinLogLevel, acCommand, acCLIConfigOverrides} <- execParser cmdWithHelpParser

  case acCommand of
    DumpDefaultConfig path ->
      A.encodeFile path Config.defaultConfig

    DumpDefaultLoggingConfig path ->
      Logging.defaultConfig >>= CM.toRepresentation >>= Y.encodeFile path

    StartChainIndex {} -> do
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
runMain = runMainWithLog putStrLn

-- Run main with provided function to log startup logs.
runMainWithLog :: (String -> IO ()) -> CM.Configuration -> Config.ChainIndexConfig -> IO ()
runMainWithLog logger logConfig config = do
  withRunRequirements logConfig config $ \runReq -> do

    mslotNo <- getTipSlot config
    case mslotNo of
      Just slotNo -> do
        let slotNoStr = "\nThe tip of the local node: " <> show slotNo
        logger slotNoStr
      Nothing -> do
        putStrLn "\nLocal node still at Genesis Tip !!!"
        exitFailure

    -- Queue for processing events
    let maxQueueSize = Config.cicAppendTransactionQueueSize config
    eventsQueue <- newTBMQueueIO maxQueueSize (measureEventQueueSizeByTxs maxQueueSize)
    syncHandler
      <- storeChainSyncHandler eventsQueue
        & storeFromBlockNo (fromCardanoBlockNo $ Config.cicStoreFrom config)
        & pure

    logger $ "Connecting to the node using socket: " <> Config.cicSocketPath config
    syncChainIndex config runReq syncHandler

    (trace :: Trace IO (PrettyObject SyncLog), _) <- setupTrace_ logConfig "chain-index"
    withAsync (processEventsQueue trace runReq eventsQueue) $ \processAsync -> do

      let port = show (Config.cicPort config)
      logger $ "Starting webserver on port " <> port
      logger $ "A Swagger UI for the endpoints are available at "
              <> "http://localhost:" <> port <> "/swagger/swagger-ui"
      Server.serveChainIndexQueryServer (Config.cicPort config) runReq
      wait processAsync
