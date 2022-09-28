{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API (NoContent, (:<|>) (..))
import Servant.Client (ClientM, client, mkClientEnv, parseBaseUrl, runClientM)
import Servant.JsonRpc.Example (API, NonEndpoint)


main :: IO ()
main = do
    env <- mkClientEnv <$> newManager defaultManagerSettings <*> parseBaseUrl "http://localhost:8080"
    void . flip runClientM env $ do
        jsonRpcPrint "Starting RPC calls"
        liftIO . print =<< add (2, 10)
        liftIO . print =<< multiply (2, 10)

        printMessage "Starting REST calls"
        liftIO . print =<< getTime

    -- A JSON-RPC error response
    print =<< runClientM (launchMissiles 100) env


add, multiply :: (Int, Int) -> ClientM (JsonRpcResponse String Int)
jsonRpcPrint :: String -> ClientM NoContent
getTime :: ClientM String
printMessage :: String -> ClientM NoContent
(add :<|> multiply :<|> jsonRpcPrint) :<|> (getTime :<|> printMessage) = client $ Proxy @API


launchMissiles :: Int -> ClientM (JsonRpcResponse String Bool)
launchMissiles = client $ Proxy @NonEndpoint

-- | The 'RawJsonRpc' construct is completely transparent to clients
instance (RunClient m, HasClient m api) => HasClient m (RawJsonRpc api) where
    type Client m (RawJsonRpc api) = Client m api
    clientWithRoute pxm _  = clientWithRoute pxm (Proxy @api)
    hoistClientMonad pxm _ = hoistClientMonad pxm (Proxy @api)


instance (RunClient m, KnownSymbol method, ToJSON p, FromJSON e, FromJSON r)
    => HasClient m (JsonRpc method p e r) where

    type Client m (JsonRpc method p e r)
        = p -> m (JsonRpcResponse e r)

    clientWithRoute _ _ req p =
        client req jsonRpcRequest

        where
        client = clientWithRoute (Proxy @m) endpoint
        jsonRpcRequest = Request (symbolVal $ Proxy @method) p (Just 0)

        endpoint = Proxy @(JsonRpcEndpoint (JsonRpc method p e r))

    hoistClientMonad _ _ f x p = f $ x p


instance (RunClient m, KnownSymbol method, ToJSON p)
    => HasClient m (JsonRpcNotification method p) where

    type Client m (JsonRpcNotification method p)
        = p -> m NoContent

    clientWithRoute _ _ req p =
        client req jsonRpcRequest
        where
        client = clientWithRoute (Proxy @m) endpoint
        jsonRpcRequest = Request (symbolVal $ Proxy @method) p Nothing

        endpoint = Proxy @(JsonRpcEndpoint (JsonRpcNotification method p))

    hoistClientMonad _ _ f x p = f $ x p
