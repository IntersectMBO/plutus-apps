{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (..))
import Ledger (TxOutRef)
import Marconi.Client.Types (JsonRpcResponse)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API (NoContent, (:<|>) (..))
import Servant.Client (ClientM, client, mkClientEnv, parseBaseUrl, runClientM)
import Types (API)

main :: IO ()
main = do
    env <- mkClientEnv <$>
        newManager defaultManagerSettings <*>
        parseBaseUrl "http://localhost:9000"
    void . flip runClientM env $ do
        void . jsonRpcPrint $ "Starting RPC calls"
        liftIO . print =<< add (2, 10)
        liftIO . print =<< findTxOutRef ("0654321")

        void . printMessage $ "Starting REST calls"
        liftIO . print =<< getTime

add :: (Int, Int) -> ClientM (JsonRpcResponse String Int)
getTime :: ClientM String
findTxOutRef :: String -> ClientM (JsonRpcResponse String TxOutRef)
jsonRpcPrint :: String -> ClientM NoContent
printMessage :: String -> ClientM NoContent
(add :<|> findTxOutRef :<|>  jsonRpcPrint) :<|> ( getTime :<|> printMessage) = client $ Proxy @API
