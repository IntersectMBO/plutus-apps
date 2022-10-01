{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Marconi.Api.HttpServer(
    bootstrapHttp -- ^ starts the http boo
    ) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (Proxy))
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Ledger (TxId (TxId), TxOutRef (TxOutRef, txOutRefId, txOutRefIdx))
import Marconi.Api.Routes (API)
import Marconi.Api.Types (HasJsonRpcEnv (..), JsonRpcEnv, MambaCache)
import Marconi.JsonRpc.Types (JsonRpcErr)
import Marconi.Server.Types ()
import Network.Wai.Handler.Warp (runSettings)
import Servant.API (NoContent (NoContent), (:<|>) ((:<|>)))
import Servant.Server (Handler, Server, serve)


bootstrapHttp :: JsonRpcEnv -> IO ()
bootstrapHttp env = runSettings
    (env ^. httpSettings)
    (serve (Proxy @API) (server (env ^. addressTxOutRefCache) ) )

server :: MambaCache -> Server API
server _ =  (add :<|> echo :<|> findTxOutRef :<|> printMessage) :<|> (getTime :<|> printMessage)

add :: (Int, Int) -> Handler (Either (JsonRpcErr String) Int)
add = pure . Right . uncurry (+)

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
findTxOutRef :: String -> Handler (Either (JsonRpcErr String) TxOutRef)
findTxOutRef _ =  pure . Right $ txoutref
    where
        txoutref = TxOutRef{txOutRefId=TxId "08affff", txOutRefIdx=123456}
