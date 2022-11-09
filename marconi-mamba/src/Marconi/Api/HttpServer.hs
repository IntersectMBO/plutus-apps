{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Marconi.Api.HttpServer(
    bootstrap
    ) where

import Cardano.Api ()
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Text (Text, pack)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Marconi.Api.Routes (API)
import Marconi.Api.Types (DBQueryEnv, HasJsonRpcEnv (httpSettings, queryEnv), JsonRpcEnv, QueryExceptions,
                          UtxoTxOutReport)
import Marconi.Api.UtxoIndexersQuery qualified as Q.Utxo (findAll, findByAddress, reportBech32Addresses)
import Marconi.JsonRpc.Types (JsonRpcErr (JsonRpcErr, errorCode, errorData, errorMessage), parseErrorCode)
import Marconi.Server.Types ()
import Network.Wai.Handler.Warp (runSettings)
import Servant.API (NoContent (NoContent), (:<|>) ((:<|>)))
import Servant.Server (Handler, Server, serve)

-- | bootstraps the he http server
bootstrap :: JsonRpcEnv -> IO ()
bootstrap env =  runSettings
        (env ^. httpSettings)
        (serve (Proxy @API) (server (env ^. queryEnv ) ) )

server :: DBQueryEnv -> Server API
server env
    = ( echo
        :<|> utxoTxOutReport env
        :<|> utxoTxOutReports env
        :<|> targetAddressesReport env
        :<|> printMessage env )
      :<|> (getTime
            :<|> getTargetAddresses env
            :<|> printMessage env)

-- | prints message to console
--  Used for testing the server from console
printMessage
    :: DBQueryEnv               -- ^ database configuration
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
    :: DBQueryEnv               -- ^ database configuration
    ->  Handler (Set Text)
getTargetAddresses =  pure . Q.Utxo.reportBech32Addresses

-- | Retrieves a set of TxOutRef
utxoTxOutReport
    :: DBQueryEnv               -- ^ database configuration
    -> String                   -- ^ bech32 addressCredential
    -> Handler (Either (JsonRpcErr String) UtxoTxOutReport )
utxoTxOutReport env address = liftIO $
    first toRpcErr <$> (Q.Utxo.findByAddress env . pack $ address)

-- | Retrieves a set of TxOutRef
-- TODO convert this to stream
utxoTxOutReports
    :: DBQueryEnv                   -- ^ database configuration
    -> Int                          -- ^ limit, for now we are ignoring this param and return 100
    -> Handler (Either (JsonRpcErr String) (Set UtxoTxOutReport))
utxoTxOutReports env _ =
    liftIO $ Right <$> Q.Utxo.findAll env

targetAddressesReport
    :: DBQueryEnv                   -- ^ database configuration
    -> Int                          -- ^ limit, for now we are ignoring this param and return 100
    -> Handler (Either (JsonRpcErr String) (Set Text) )
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
