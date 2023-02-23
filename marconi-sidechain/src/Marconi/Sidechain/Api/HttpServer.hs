{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Marconi.Sidechain.Api.HttpServer(
    bootstrap
    ) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, pack)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Network.Wai.Handler.Warp (runSettings)
import Servant.API ((:<|>) ((:<|>)))
import Servant.Server (Handler, Server, serve)

import Cardano.Api ()
import Marconi.Sidechain.Api.Query.Indexers.Utxo qualified as Q.Utxo
import Marconi.Sidechain.Api.Routes (API)
import Marconi.Sidechain.Api.Types (HasSidechainEnv (httpSettings, queryEnv), IndexerEnv, QueryExceptions, SidechainEnv,
                                    UtxoQueryResult)
import Network.JsonRpc.Server.Types ()
import Network.JsonRpc.Types (JsonRpcErr (JsonRpcErr, errorCode, errorData, errorMessage), parseErrorCode)

-- | Bootstraps the HTTP server
bootstrap :: SidechainEnv -> IO ()
bootstrap env =  runSettings
        (env ^. httpSettings)
        (serve (Proxy @API) (server (env ^. queryEnv ) ) )

server
  :: IndexerEnv -- ^  Utxo Environment to access Utxo Storage running on the marconi thread
  -> Server API
server env = jsonRpcServer :<|> restApiServer
 where
     jsonRpcServer =
              echo
         :<|> getUtxoFromAddressHandler env
         :<|> getTargetAddressesQueryHandler env

     restApiServer =
              getTimeHandler
         :<|> getTargetAddressesHandler env

-- | Echos message back as a Jsonrpc response. Used for testing the server.
echo
    :: String
    ->  Handler (Either (JsonRpcErr String) String)
echo = return . Right

-- | Echos current time as REST response. Used for testing the http server outside of jsonrpc
-- protocol.
getTimeHandler :: Handler String
getTimeHandler = timeString <$> liftIO getCurrentTime
    where
    timeString = formatTime defaultTimeLocale "%T"

-- | prints TargetAddresses Bech32 representation to the console
getTargetAddressesHandler
    :: IndexerEnv -- ^  Utxo Environment to access Utxo Storage running on the marconi thread
    -> Handler [Text]
getTargetAddressesHandler =  pure . Q.Utxo.reportBech32Addresses

-- | Handler for retrieving UTXOs by Address
getUtxoFromAddressHandler
    :: IndexerEnv       -- ^ Utxo Environment to access Utxo Storage running on the marconi thread
    -> String           -- ^ Bech32 addressCredential
    -> Handler (Either (JsonRpcErr String) UtxoQueryResult)
getUtxoFromAddressHandler env address = liftIO $
    first toRpcErr <$> (Q.Utxo.findByBech32Address env . pack $ address)

-- | prints TargetAddresses Bech32 representation as thru JsonRpc
getTargetAddressesQueryHandler
    :: IndexerEnv -- ^ database configuration
    -> String
    -- ^ Will always be an empty string as we are ignoring this param, and returning everything
    -> Handler (Either (JsonRpcErr String) [Text])
getTargetAddressesQueryHandler env _ = pure . Right . Q.Utxo.reportBech32Addresses $ env

-- | convert form to Jsonrpc protocal error
toRpcErr
    :: QueryExceptions
    -> JsonRpcErr String
toRpcErr e = JsonRpcErr {
           errorCode = parseErrorCode
           , errorMessage = "marconi RPC query related error!"
           , errorData = Just . show $ e
           }
