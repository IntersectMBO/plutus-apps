-- |
-- This module bootstraps the mamba JSON RPC server, it acts as a glue conntecting the
-- JSON-RPC, HttpServer, marconiIndexer, and marconi cache
--
module Marconi.Bootstrap  where

import Cardano.Api (AsType (AsShelleyAddress), ChainPoint (ChainPointAtGenesis), deserialiseFromBech32)
import Cardano.Streaming (withChainSyncEventStream)
import Control.Lens ((^.))
import Data.List.NonEmpty (fromList, nub)
import Data.Text (pack)
import Marconi.Api.HttpServer qualified as Http
import Marconi.Api.Types (CliArgs (CliArgs), HasDBQueryEnv (queryQSem), HasJsonRpcEnv (queryEnv),
                          JsonRpcEnv (JsonRpcEnv, _httpSettings, _queryEnv), RpcPortNumber, TargetAddresses)
import Marconi.Api.UtxoIndexersQuery qualified as QIUtxo
import Marconi.Indexers (combineIndexers, queryAwareUtxoWorker)
import Network.Wai.Handler.Warp (defaultSettings, setPort)


-- | Bootstraps the JSON-RPC  http server with appropriate settings and marconi cache
-- this is just a wrapper for the bootstrapHttp in json-rpc package
bootstrapJsonRpc
    :: FilePath
    -> Maybe RpcPortNumber
    -> TargetAddresses
    -> IO JsonRpcEnv
bootstrapJsonRpc dbPath maybePort targetAddresses = do
    queryenv <- QIUtxo.bootstrap dbPath targetAddresses
    let httpsettings =  maybe defaultSettings (flip setPort defaultSettings ) maybePort
    pure $ JsonRpcEnv
        { _httpSettings = httpsettings
        , _queryEnv = queryenv
        }

bootstrapHttp
    :: JsonRpcEnv
    -> IO ()
bootstrapHttp  = Http.bootstrap

-- |  marconi cardano blockchain indexer
bootstrapUtxoIndexers
    :: CliArgs
    -> JsonRpcEnv
    -> IO ()
bootstrapUtxoIndexers (CliArgs socket dbPath _ networkId targetAddresses) env =
    let
      qsem = env ^. queryEnv . queryQSem
      indexers = combineIndexers [( queryAwareUtxoWorker qsem targetAddresses , dbPath)]
      chainPoint = ChainPointAtGenesis
    in
        withChainSyncEventStream socket networkId chainPoint indexers

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
