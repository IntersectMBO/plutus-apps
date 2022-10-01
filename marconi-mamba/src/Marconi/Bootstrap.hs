-- |
-- This module bootstraps the mamba JSON RPC server, it acts as a glue conntecting the
-- JSON-RPC, HttpServer, marconiIndexer, and marconi cache
--
module Marconi.Bootstrap where
import Cardano.Api qualified
import Data.List.NonEmpty (fromList)
import Data.Proxy (Proxy (Proxy))
import Data.Text (pack)
import Marconi.Api.HttpServer (bootstrapHttp)
import Marconi.Api.Types (JsonRpcEnv (JsonRpcEnv), RpcPortNumber)
import Marconi.IndexerCache (TargetAddresses, initCache)
import Network.Wai.Handler.Warp (defaultSettings, setPort)

-- | Bootstraps the JSON-RPC  http server with appropriate settings and marconi cache
-- this is just a wrapper for the bootstrapHttp in json-rpc package
bootstrapJsonRpc
    :: JsonRpcEnv -- ^ JSON-RPC run environment
    -> IO ()
bootstrapJsonRpc = bootstrapHttp

-- TODO, we need to fix this function and error out with grace
targetAddressParser
    :: String -- ^ contains white spece delimeted lis of addresses
    -> TargetAddresses -- ^ a non empty list of valid addresses
targetAddressParser =  fromList . fromJustWithError . traverse maybeAddress . words
    where
        eitherAddress :: String -> Either Cardano.Api.Bech32DecodeError (Cardano.Api.Address  Cardano.Api.ShelleyAddr )
        eitherAddress  =  Cardano.Api.deserialiseFromBech32 (Cardano.Api.proxyToAsType Proxy) . pack

        maybeAddress  :: String -> Maybe (Cardano.Api.Address  Cardano.Api.ShelleyAddr )
        maybeAddress = either (const Nothing) Just  . eitherAddress

        fromJustWithError :: Maybe a -> a
        fromJustWithError (Just a) = a
        fromJustWithError _        = error "Empty or Invalid address list.  Addresses must be Bech32 compatable!"

-- default to port 3000
jsonRpcEnv
    :: Maybe RpcPortNumber -- ^ http port
    -> TargetAddresses -- ^ non empty list of addresses to index
    ->  IO JsonRpcEnv -- ^
jsonRpcEnv maybePort addresses = do
    cache <- initCache addresses
    let httpSetting =  maybe defaultSettings (flip setPort defaultSettings ) maybePort
    pure  (JsonRpcEnv httpSetting cache)
