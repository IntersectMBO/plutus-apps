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
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Text (pack)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Ledger.Tx.CardanoAPI (ToCardanoError)
import Marconi.Api.Routes (API)
import Marconi.Api.Types (DBQueryEnv, HasJsonRpcEnv (httpSettings, queryEnv), JsonRpcEnv, TxOutRef, UtxoRowWrapper)
import Marconi.Api.UtxoIndexersQuery qualified as Q.Utxo (findByAddress, findTxOutRefs, findUtxos)
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
server env =
    (echo :<|>
      findTxOutRef env :<|>
      findTxOutRefs env :<|>
      findUtxos env :<|>
      printMessage
    ) :<|> (getTime :<|> printMessage)

-- | prints message to console
--  Used for testing the server from console
printMessage
    :: String
    -> Handler NoContent
printMessage msg = NoContent <$ liftIO (putStrLn msg)

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

-- | Retrieves a set of TxOutRef
findTxOutRef
    :: DBQueryEnv               -- ^ database configuration
    -> String                   -- ^ bech32 addressCredential
    -> Handler (Either (JsonRpcErr String) (Set TxOutRef))
findTxOutRef env address =
    liftIO $ cardanoErrToRpcErr <$> (Q.Utxo.findByAddress env . pack ) address

-- | Retrieves a set of TxOutRef
--
findTxOutRefs
    :: DBQueryEnv                   -- ^ database configuration
    -> Int                          -- ^ limit, for now we are ignoring this param and return 100
    -> Handler (Either (JsonRpcErr String) (Set TxOutRef))
findTxOutRefs env _ =
    liftIO $ Right <$> Q.Utxo.findTxOutRefs env

-- | Retrieves a set of TxOutRef
--
findUtxos
    :: DBQueryEnv                   -- ^ database configuration
    -> Int                          -- ^ limit, for now we are ignoring this param and return 100
    -> Handler (Either (JsonRpcErr String) (Set UtxoRowWrapper))
findUtxos env _ =
    liftIO $ Right <$> Q.Utxo.findUtxos env

-- | convert form cardano error, to jsonrpc protocal error
cardanoErrToRpcErr
    :: Either ToCardanoError (Set TxOutRef)
    -> Either (JsonRpcErr String) (Set TxOutRef)
cardanoErrToRpcErr = either (Left . f ) Right
   where
       f :: ToCardanoError -> JsonRpcErr String
       f e = JsonRpcErr {
           errorCode = parseErrorCode
           , errorMessage = "address deserialization or conversion related error."
           , errorData = Just . show $ e
           }
