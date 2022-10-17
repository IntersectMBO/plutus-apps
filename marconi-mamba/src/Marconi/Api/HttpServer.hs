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
import Ledger (Address)
import Ledger.Tx (TxOutRef)
import Ledger.Tx.CardanoAPI (ToCardanoError)
import Marconi.Api.Routes (API)
import Marconi.Api.Types
import Marconi.Api.UtxoIndexersQuery (findAll, findByAddress)
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
      printMessage
    ) :<|> (getTime :<|> printMessage)

printMessage :: String -> Handler NoContent
printMessage msg = NoContent <$ liftIO (putStrLn msg)

echo :: String ->  Handler (Either (JsonRpcErr String) String)
echo  = return . Right

getTime :: Handler String
getTime = timeString <$> liftIO getCurrentTime
    where
    timeString = formatTime defaultTimeLocale "%T"

--
findTxOutRef :: DBQueryEnv -> String -> Handler (Either (JsonRpcErr String) (Set TxOutRef))
findTxOutRef hotStore address =
    liftIO $ cardanoErrToRpcErr <$> (findByAddress hotStore . pack ) address

--
findTxOutRefs :: DBQueryEnv -> String -> Handler (Either (JsonRpcErr String) [(Address, Set TxOutRef)])
findTxOutRefs hotStore _ =
    liftIO $ Right <$> findAll hotStore

cardanoErrToRpcErr :: Either ToCardanoError (Set TxOutRef)  -> Either (JsonRpcErr String) (Set TxOutRef)
cardanoErrToRpcErr = either (Left . f ) Right
   where
       f :: ToCardanoError -> JsonRpcErr String
       f e = JsonRpcErr {
           errorCode = parseErrorCode
           , errorMessage = "address deserialization or conversion related error."
           , errorData = Just . show $ e
           }
