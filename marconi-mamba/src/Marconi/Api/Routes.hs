{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.Api.Routes where

import Data.Set (Set)
import Ledger.Tx (TxOutRef)
import Marconi.JsonRpc.Types (JsonRpc, JsonRpcNotification, RawJsonRpc)
import Servant.API (Get, NoContent, PlainText, Post, ReqBody, (:<|>), (:>))

type Add            = JsonRpc "add"      (Int, Int) String Int
type Echo           = JsonRpc "echo" String String String
type FindTxOutRef   = JsonRpc "txOutRef" String String (Set TxOutRef)

type Print    = JsonRpcNotification "print" String

type RpcAPI = Add :<|> Echo :<|> FindTxOutRef :<|> Print

type JsonRpcAPI = "json-rpc" :> RawJsonRpc RpcAPI

type GetTime = "time" :> Get '[PlainText] String

type PrintMessage = "print" :> ReqBody '[PlainText] String :> Post '[PlainText] NoContent

type RestAPI = "rest" :> (GetTime :<|> PrintMessage)

type API = JsonRpcAPI :<|> RestAPI

type NonEndpoint = "json-rpc" :> RawJsonRpc (JsonRpc "launch-missles" Int String Bool)
