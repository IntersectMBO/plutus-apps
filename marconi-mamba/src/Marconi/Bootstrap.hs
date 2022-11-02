-- |
-- This module bootstraps the mamba JSON RPC server, it acts as a glue conntecting the
-- JSON-RPC, HttpServer, marconiIndexer, and marconi cache
--
module Marconi.Bootstrap  where

import Cardano.Api (ChainPoint (ChainPointAtGenesis), NetworkId, deserialiseFromBech32, proxyToAsType)
import Cardano.Streaming (withChainSyncEventStream)
import Control.Lens ((^.))
import Data.List (nub)
import Data.List.NonEmpty (fromList)
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified as Text (Text, pack, unpack, words)
import Marconi.Api.HttpServer qualified as Http
import Marconi.Api.Types (CardanoAddress, CliArgs (CliArgs), HasDBQueryEnv (queryComm), HasJsonRpcEnv (queryEnv),
                          JsonRpcEnv (JsonRpcEnv, _HttpSettings, _QueryEnv), RpcPortNumber, TargetAddresses)
import Marconi.Api.UtxoIndexersQuery qualified as QIUtxo
import Marconi.Indexers (combineIndexers, queryAwareUtxoWorker)
import Network.Wai.Handler.Warp (defaultSettings, setPort)


-- | Bootstraps the JSON-RPC  http server with appropriate settings and marconi cache
-- this is just a wrapper for the bootstrapHttp in json-rpc package
bootstrapJsonRpc
    :: FilePath
    -> Maybe RpcPortNumber
    -> TargetAddresses
    -> NetworkId
    -> IO JsonRpcEnv
bootstrapJsonRpc dbPath maybePort targetAddresses nId = do
    queryenv <- QIUtxo.bootstrap dbPath targetAddresses nId
    let httpsettings =  maybe defaultSettings (flip setPort defaultSettings ) maybePort
    pure $ JsonRpcEnv
        { _HttpSettings = httpsettings
        , _QueryEnv = queryenv
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
      qsem = env ^. queryEnv . queryComm
      indexers = combineIndexers [( queryAwareUtxoWorker qsem targetAddresses , dbPath)]
      chainPoint = ChainPointAtGenesis
    in
        withChainSyncEventStream socket networkId chainPoint indexers

-- | parses a white space separated address list
-- Note, duplicate addresses are rmoved
targetAddressParser
    :: String           -- ^ contains white spece delimeted lis of addresses
    -> TargetAddresses  -- ^ a non empty list of valid addresses
targetAddressParser
    = fromList
    . nub
    . fmap (txtToCardano )
    . Text.words
    . Text.pack

-- | Exit program with error
-- Note, if the targetAddress parser fails, or is empty, there is nothing to do for the hotStore.
-- In such case we should fail fast
fromJustWithError :: (Show e) => Either e a -> a
fromJustWithError v = case v of
    Left e ->
        error $ "\n!!!\n Abnormal Termination with Error: " <> show e <> "\n!!!\n"
    Right accounts -> accounts

txtToCardano :: Text.Text -> CardanoAddress
txtToCardano addr = case deserialiseToCardano addr of
    Left e -> error ( "Error deserializing user provided bech32 address "
                      <> Text.unpack addr <> " to Cardano.Api.Address with error"
                      <> show e  )
    Right a -> a
    where
        deserialiseToCardano = deserialiseFromBech32 (proxyToAsType Proxy)
