{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

-- | A sample servant json-rpc client

module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (Proxy))
import Ledger.Tx (TxOutRef)
import Marconi.Client.Types (JsonRpcResponse)
import Marconi.JsonRpc.Types (JsonRpc, JsonRpcNotification, RawJsonRpc)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API (Get, NoContent, PlainText, Post, ReqBody, (:<|>) ((:<|>)), (:>))
import Servant.Client (ClientM, client, mkClientEnv, parseBaseUrl, runClientM)

-- | start json-rpc client
-- Note, we use default port, 3000,  [defaultSettings](https://hackage.haskell.org/package/warp-3.3.23/docs/Network-Wai-Handler-Warp.html#v:defaultSettings)
main :: IO ()
main = do
    env <- mkClientEnv <$>
        newManager defaultManagerSettings <*>
        parseBaseUrl "http://localhost:3000"
    void . flip runClientM env $ do
        void . jsonRpcPrint $ "Starting RPC calls"
        liftIO . print =<< add (2, 10)
        liftIO . print =<< findTxOutRef "0654321"

        void . printMessage $ "Starting REST calls"
        liftIO . print =<< getTime

add :: (Int, Int) -> ClientM (JsonRpcResponse String Int)
getTime :: ClientM String
findTxOutRef :: String -> ClientM (JsonRpcResponse String TxOutRef)
jsonRpcPrint :: String -> ClientM NoContent
printMessage :: String -> ClientM NoContent
(add :<|> findTxOutRef :<|>  jsonRpcPrint) :<|> ( getTime :<|> printMessage) = client $ Proxy @API

type Add            = JsonRpc "add"      (Int, Int) String Int
type FindTxOutRef   = JsonRpc "txOutRef" String String TxOutRef
type Print          = JsonRpcNotification "print" String

type RpcAPI = Add :<|> FindTxOutRef :<|> Print

type JsonRpcAPI = "json-rpc" :> RawJsonRpc RpcAPI

type GetTime = "time" :> Get '[PlainText] String
type PrintMessage = "print" :> ReqBody '[PlainText] String :> Post '[PlainText] NoContent

type RestAPI = "rest" :> (GetTime :<|> PrintMessage)

type API = JsonRpcAPI :<|> RestAPI

type NonEndpoint = "json-rpc" :> RawJsonRpc (JsonRpc "launch-missles" Int String Bool)
