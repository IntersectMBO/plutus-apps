-- | Defines REST and JSON-RPC routes

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.Sidechain.Api.Routes where

import Data.Text (Text)
import Marconi.Sidechain.Api.Types (UtxoQueryResult)
import Network.JsonRpc.Types (JsonRpc, RawJsonRpc)
import Servant.API (Get, JSON, PlainText, (:<|>), (:>))

------------------------------------------
  --  RPC types
  --  methodName -> parameter(s) -> return-type
------------------------------------------
type RpcEcho = JsonRpc "echo" String String String

type RpcUtxoQueryResult = JsonRpc "getUtxoFromAddress" String String UtxoQueryResult

type RpcTargetAddresses = JsonRpc "getTargetAddresses" String String [Text]

-- | Rpc routes
type RpcAPI = RpcEcho :<|> RpcTargetAddresses :<|> RpcUtxoQueryResult

-- | JSON-RPC API, endpoint
type JsonRpcAPI = "json-rpc" :> RawJsonRpc RpcAPI

--------------------
--- REST related ---
--------------------
type GetTime = "time" :> Get '[PlainText] String

type GetTargetAddresses = "addresses" :> Get '[JSON] [Text]

-- | REST API, endpoints
type RestAPI = "rest" :> (GetTime :<|> GetTargetAddresses)

-- | marconi-sidechain APIs
type API = JsonRpcAPI :<|> RestAPI
