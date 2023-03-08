-- |
-- This module bootstraps the marconi-sidechain JSON RPC server, it acts as a glue conntecting the
-- JSON-RPC, HttpServer, marconiIndexer, and marconi cache
--
module Marconi.Sidechain.Bootstrap  where

import Control.Concurrent.STM (atomically)
import Control.Lens ((^.))
import Data.List.NonEmpty (fromList, nub)
import Data.Text (pack)
import Network.Wai.Handler.Warp (Port, defaultSettings, setPort)
import System.FilePath ((</>))

import Cardano.Api (AsType (AsShelleyAddress), deserialiseFromBech32)
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

bootstrapHttp
    :: SidechainEnv
    -> IO ()
bootstrapHttp  = Http.bootstrap

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

-- | parses a white space separated address list
-- Note, duplicate addresses are rmoved
targetAddressParser
    :: String           -- ^ contains white spece delimeted lis of addresses
    -> TargetAddresses  -- ^ a non empty list of valid addresses
targetAddressParser =
    nub
    . fromList
    . fromJustWithError
    . traverse (deserializeToCardano . pack)
    . words
    where
        deserializeToCardano = deserialiseFromBech32 AsShelleyAddress

-- | Exit program with error
-- Note, if the targetAddress parser fails, or is empty, there is nothing to do for the hotStore.
-- In such case we should fail fast
fromJustWithError :: (Show e) => Either e a -> a
fromJustWithError v = case v of
    Left e ->
        error $ "\n!!!\n Abnormal Termination with Error: " <> show e <> "\n!!!\n"
    Right accounts -> accounts
