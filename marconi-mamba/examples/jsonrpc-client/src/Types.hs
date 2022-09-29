{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import Ledger (TxOutRef)
import Marconi.JsonRpc.Types (JsonRpc, JsonRpcNotification, RawJsonRpc)
import Servant.API (Get, NoContent, PlainText, Post, ReqBody, (:<|>) (..), (:>))


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
