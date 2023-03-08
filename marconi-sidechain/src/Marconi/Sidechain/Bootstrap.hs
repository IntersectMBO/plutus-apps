-- |
-- This module bootstraps the marconi-sidechain JSON RPC server, it acts as a glue conntecting the
-- JSON-RPC, HttpServer, marconiIndexer, and marconi cache
--
module Marconi.Sidechain.Bootstrap where

import Control.Concurrent.STM (atomically)
import Control.Lens ((^.))
import Network.Wai.Handler.Warp (Port, defaultSettings, setPort)
import System.FilePath ((</>))

import Marconi.ChainIndex.Indexers (mkIndexerStream, runIndexers, startIndexers, utxoWorker)
import Marconi.ChainIndex.Types (TargetAddresses, utxoDbName)
import Marconi.Sidechain.Api.HttpServer qualified as Http
import Marconi.Sidechain.Api.Query.Indexers.Utxo (UtxoIndexer, initializeEnv, writeTMVar')
import Marconi.Sidechain.Api.Types (CliArgs (CliArgs), HasIndexerEnv (uiIndexer), HasSidechainEnv (queryEnv),
                                    SidechainEnv (SidechainEnv, _httpSettings, _queryEnv))


-- | Bootstraps the JSON-RPC  http server with appropriate settings and marconi cache
-- this is just a wrapper for the bootstrapHttp in json-rpc package
initializeIndexerEnv
    :: Maybe Port
    -> Maybe TargetAddresses
    -> IO SidechainEnv
initializeIndexerEnv maybePort targetAddresses = do
    queryenv <- initializeEnv targetAddresses
    let httpsettings =  maybe defaultSettings (flip setPort defaultSettings ) maybePort
    pure $ SidechainEnv
        { _httpSettings = httpsettings
        , _queryEnv = queryenv
        }

-- |  Marconi cardano blockchain indexer
bootstrapIndexers
    :: CliArgs
    -> SidechainEnv
    -> IO ()
bootstrapIndexers (CliArgs socket dbPath _ networkId targetAddresses) env = do
  let callbackIndexer :: UtxoIndexer -> IO ()
      callbackIndexer = atomically . writeTMVar' (env ^. queryEnv . uiIndexer)
  (chainPointsToResumeFrom, coordinator) <-
      startIndexers
          [ ( utxoWorker callbackIndexer targetAddresses
            , dbPath </> utxoDbName
            )
          ]
  runIndexers
    socket
    networkId
    chainPointsToResumeFrom
    (mkIndexerStream coordinator)
    "marconi-sidechain"
