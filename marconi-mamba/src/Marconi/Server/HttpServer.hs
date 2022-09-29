{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Marconi.Server.HttpServer where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (Proxy))
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Ledger (TxId (TxId), TxOutRef (TxOutRef, txOutRefId, txOutRefIdx))
import Marconi.JsonRpc.Types (JsonRpcErr)
import Marconi.Server.Routes (API)
import Marconi.Server.Types (AddressTxOutRefCache, HasHttpEnv (addressTxOutRefCache, portNumber), HttpEnv)
import Network.Wai.Handler.Warp (run)
import Servant.API (NoContent (NoContent), (:<|>) ((:<|>)))
import Servant.Server (Handler, Server, serve)

httpMain :: HttpEnv -> IO ()
httpMain env =
    let
        port = env ^. portNumber
        cache = env ^. addressTxOutRefCache
    in
        run port (serve (Proxy @API) (server cache) )

server :: AddressTxOutRefCache -> Server API
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
