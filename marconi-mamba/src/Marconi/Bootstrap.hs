-- |
-- This module bootstraps the mamba JSON RPC server, it acts as a glue conntecting the
-- JSON-RPC, HttpServer, marconiIndexer, and marconi cache
--
module Marconi.Bootstrap  where

import Control.Concurrent.STM (atomically)
import Control.Exception (catch)
import Control.Lens ((^.))
import Data.List.NonEmpty (fromList, nub)
import Data.Text (pack)
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)

import Cardano.Api (AsType (AsShelleyAddress), ChainPoint (ChainPointAtGenesis), deserialiseFromBech32)
import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logError)
import Cardano.BM.Tracing (defaultConfigStdout)
import Cardano.Streaming (ChainSyncEventException (NoIntersectionFound), withChainSyncEventStream)
import Marconi.Api.HttpServer qualified as Http
import Marconi.Api.Types (CliArgs (CliArgs), HasDBQueryEnv (queryTMVar), HasJsonRpcEnv (queryEnv),
                          JsonRpcEnv (JsonRpcEnv, _httpSettings, _queryEnv), RpcPortNumber, TargetAddresses,
                          UtxoQueryTMVar (UtxoQueryTMVar))
import Marconi.Api.UtxoIndexersQuery qualified as QApi
import Marconi.Indexers (combineIndexers, utxoWorker)
import Network.Wai.Handler.Warp (defaultSettings, setPort)


-- | Bootstraps the JSON-RPC  http server with appropriate settings and marconi cache
-- this is just a wrapper for the bootstrapHttp in json-rpc package
bootstrapJsonRpc
    :: Maybe RpcPortNumber
    -> TargetAddresses
    -> IO JsonRpcEnv
bootstrapJsonRpc maybePort targetAddresses = do
    queryenv <- QApi.bootstrap targetAddresses
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
    do
        let (UtxoQueryTMVar qTMVar) = env ^. queryEnv . queryTMVar
            callbackIndexer :: QApi.UtxoIndex -> IO QApi.UtxoIndex
            callbackIndexer index = atomically $ (QApi.writeTMVar qTMVar  index)  >>  pure index
            indexers = combineIndexers [( utxoWorker  callbackIndexer (Just targetAddresses) , dbPath)]
            chainPoint = ChainPointAtGenesis
        c <- defaultConfigStdout
        withTrace c "marconi-mamba" $ \trace ->
          withChainSyncEventStream socket networkId chainPoint indexers
          `catch` \NoIntersectionFound ->
            logError trace $
                renderStrict $
                    layoutPretty defaultLayoutOptions $
                        "No intersection found when looking for the chain point"
                        <+> (pretty . show  $ chainPoint)  <> "."
                        <+> "Please check the slot number and the block hash do belong to the chain"

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
