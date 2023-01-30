{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Marconi.Api.HttpServer(
    bootstrap
    ) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, pack)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Network.Wai.Handler.Warp (runSettings)
import Servant.API (NoContent (NoContent), (:<|>) ((:<|>)))
import Servant.Server (Handler, Server, serve)

import Cardano.Api ()
import Marconi.Api.Routes (API)
import Marconi.Api.Types (HasJsonRpcEnv (httpSettings, queryEnv), JsonRpcEnv, QueryExceptions, UtxoIndexerEnv,
                          UtxoReport)
import Marconi.Api.UtxoIndexersQuery qualified as Q.Utxo
import Marconi.JsonRpc.Types (JsonRpcErr (JsonRpcErr, errorCode, errorData, errorMessage), parseErrorCode)
import Marconi.Server.Types ()

-- | bootstraps the he http server
bootstrap :: JsonRpcEnv -> IO ()
bootstrap env =  runSettings
        (env ^. httpSettings)
        (serve (Proxy @API) (server (env ^. queryEnv ) ) )

server
  :: UtxoIndexerEnv -- ^  Utxo Environment to access Utxo Storage running on the marconi thread
  -> Server API
server env
    = ( echo
        :<|> utxoJsonReport env
        :<|> targetAddressesReport env
        :<|> printMessage env )
      :<|> (getTime
            :<|> getTargetAddresses env
            :<|> printMessage env)

-- | prints message to console
--  Used for testing the server from console
printMessage
    :: UtxoIndexerEnv    -- ^  Utxo Environment to access Utxo Storage running on the marconi thread
    -> String
    -> Handler NoContent
printMessage env msg = NoContent <$  (
    liftIO $ do
            putStrLn msg
            putStrLn "\n"
            print (Q.Utxo.reportBech32Addresses env)
    )

-- | echos message back as a jsonrpc response
--  Used for testing the server
echo
    :: String
    ->  Handler (Either (JsonRpcErr String) String)
echo  = return . Right

-- | echos current time as REST response
--  Used for testing the http server outside of jsonrpc protocol
getTime :: Handler String
getTime = timeString <$> liftIO getCurrentTime
    where
    timeString = formatTime defaultTimeLocale "%T"

getTargetAddresses
    :: UtxoIndexerEnv -- ^  Utxo Environment to access Utxo Storage running on the marconi thread
    -> Handler [Text]
getTargetAddresses =  pure . Q.Utxo.reportBech32Addresses

-- | Retrieves a set of TxOutRef
utxoJsonReport
    :: UtxoIndexerEnv   -- ^  Utxo Environment to access Utxo Storage running on the marconi thread
    -> String           -- ^ bech32 addressCredential
    -> Handler (Either (JsonRpcErr String) UtxoReport )
utxoJsonReport env address = liftIO $
    first toRpcErr <$> (Q.Utxo.findByAddress env . pack $ address)

targetAddressesReport
    :: UtxoIndexerEnv                   -- ^ database configuration
    -> Int                              -- ^ limit, for now we are ignoring returning everyting
    -> Handler (Either (JsonRpcErr String) [Text] )
targetAddressesReport env _ = pure . Right . Q.Utxo.reportBech32Addresses $ env

-- | convert form to jsonrpc protocal error
toRpcErr
    :: QueryExceptions
    -> JsonRpcErr String
toRpcErr e = JsonRpcErr {
           errorCode = parseErrorCode
           , errorMessage = "marconi RPC query related error!"
           , errorData = Just . show $ e
           }
