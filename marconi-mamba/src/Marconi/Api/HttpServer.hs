{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Marconi.Api.HttpServer(
    bootstrap -- ^ starts the http server
    ) where

import Control.Lens ((^.))
import Control.Monad.IO.Class
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Text (pack)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Ledger (TxId (TxId), TxOutRef (TxOutRef, txOutRefId, txOutRefIdx))
import Ledger.Tx.CardanoAPI (ToCardanoError (..), fromCardanoAddress)
import Marconi.Api.Routes (API)
import Marconi.Api.Types (HasJsonRpcEnv (..), JsonRpcEnv)
import Marconi.IndexersHotStore
import Marconi.JsonRpc.Types (JsonRpcErr (..), parseErrorCode)
import Marconi.Server.Types ()
import Network.Wai.Handler.Warp (runSettings)
import Servant.API (NoContent (NoContent), (:<|>) ((:<|>)))
import Servant.Server (Handler, Server, serve)

bootstrap :: JsonRpcEnv -> IO ()
bootstrap env = runSettings
    (env ^. httpSettings)
    (serve (Proxy @API) (server (env ^. addressTxOutRefCache) ) )

server :: IndexerHotStore -> Server API
server store =  ( add  :<|> echo :<|> (findTxOutRef store) :<|> printMessage) :<|> (getTime :<|> printMessage)

add :: (Int, Int) -> Handler (Either (JsonRpcErr String) Int)
add  = pure . Right . uncurry (+)

printMessage :: String -> Handler NoContent
printMessage msg = NoContent <$ liftIO (putStrLn msg)

echo :: String ->  Handler (Either (JsonRpcErr String) String)
echo  = return . Right

getTime :: Handler String
getTime = timeString <$> liftIO getCurrentTime
    where
    timeString = formatTime defaultTimeLocale "%T"

-- TODO
-- need a readerT and serant Nat here
--
findTxOutRef :: IndexerHotStore -> String -> Handler (Either (JsonRpcErr String) (Set TxOutRef))
findTxOutRef hotStore address =
    liftIO $ (findByAddress hotStore ( pack address)) >>= pure . cardanoErrToRpcErr

cardanoErrToRpcErr :: (Either ToCardanoError (Set TxOutRef) ) -> Either (JsonRpcErr String) (Set TxOutRef)
cardanoErrToRpcErr = either (Left . f ) Right
   where
       f :: ToCardanoError -> JsonRpcErr String
       f e = JsonRpcErr {
           errorCode = parseErrorCode
           , errorMessage = "address deserialization or conversion related error."
           , errorData = Just . show $ e
           }
