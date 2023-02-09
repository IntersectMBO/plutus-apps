{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.Mamba.Api.Routes where

import Data.Text (Text)
import Marconi.Mamba.Api.Types (UtxoReport)
import Network.JsonRpc.Types (JsonRpc, JsonRpcNotification, RawJsonRpc)
import Servant.API (Get, JSON, NoContent, PlainText, Post, ReqBody, (:<|>), (:>))

  --  RPC method parameter(s) return-type
type Echo = JsonRpc "echo" String String String

type UtxoJsonReport = JsonRpc "utxoJsonReport" String String UtxoReport

type TargetAddressesReport  = JsonRpc "addressesBech32Report" Int String [Text]

type Print    = JsonRpcNotification "print" String

type RpcAPI = Echo :<|> UtxoJsonReport :<|> TargetAddressesReport :<|> Print

type JsonRpcAPI = "json-rpc" :> RawJsonRpc RpcAPI

type GetTime = "time" :> Get '[PlainText] String

type GetTargetAddresses = "addresses" :> Get '[JSON] [Text]

type PrintMessage = "print" :> ReqBody '[PlainText] String :> Post '[PlainText] NoContent

type RestAPI = "rest" :> (GetTime :<|> GetTargetAddresses :<|> PrintMessage)

type API = JsonRpcAPI :<|> RestAPI

type NonEndpoint = "json-rpc" :> RawJsonRpc (JsonRpc "launch-missles" Int String Bool)
