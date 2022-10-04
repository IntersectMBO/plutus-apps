-- |
-- This module bootstraps the mamba JSON RPC server, it acts as a glue conntecting the
-- JSON-RPC, HttpServer, marconiIndexer, and marconi cache
--
module Marconi.Bootstrap (
   bootstrapJsonRpc
   , jsonRpcEnv
   ) where

import Cardano.Api qualified
import Data.List.NonEmpty (fromList)
import Data.Proxy (Proxy (Proxy))
import Data.Text (pack)
import Marconi.Api.HttpServer (bootstrap)
import Marconi.Api.Types (JsonRpcEnv (JsonRpcEnv), RpcPortNumber)
import Marconi.IndexersHotStore (TargetAddresses, bootstrapHotStore, targetAddressParser)
import Network.Wai.Handler.Warp (defaultSettings, setPort)

-- | Bootstraps the JSON-RPC  http server with appropriate settings and marconi cache
-- this is just a wrapper for the bootstrapHttp in json-rpc package
bootstrapJsonRpc
    :: JsonRpcEnv -- ^ JSON-RPC run environment
    -> IO ()
bootstrapJsonRpc = bootstrap

-- | configure json-rpc env.
-- Note if no tcp/ip port number is provided, we default to servant default port of 3000
-- @see (warp defaultSettings)[https://hackage.haskell.org/package/warp-3.3.23/docs/Network-Wai-Handler-Warp.html#v:defaultSettings] for details
jsonRpcEnv
    :: Maybe RpcPortNumber -- ^ http port
    ->  IO JsonRpcEnv -- ^
jsonRpcEnv maybePort = do
    hotStore <- bootstrapHotStore
    let httpSetting =  maybe defaultSettings (flip setPort defaultSettings ) maybePort
    pure  (JsonRpcEnv httpSetting hotStore)
