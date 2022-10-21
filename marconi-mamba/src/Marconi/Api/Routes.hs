{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.Api.Routes where

import Data.Set (Set)
import Ledger.Address qualified as P
import Ledger.Tx (TxOutRef)
import Marconi.Api.Types (UtxoRowWrapper)
import Marconi.JsonRpc.Types (JsonRpc, JsonRpcNotification, RawJsonRpc)
import Servant.API (Get, NoContent, PlainText, Post, ReqBody, (:<|>), (:>))

type Echo                   = JsonRpc "echo" String String String
type TxOutRefReport         = JsonRpc "txOutRefReport" String String (Set TxOutRef)
type TxOutRefsReport        = JsonRpc "txOutRefsReport" Int String (Set TxOutRef)
type UtxosReport            = JsonRpc "utxosReport" Int String (Set UtxoRowWrapper)
type TargetAddressesReport  = JsonRpc "addressesReport" Int String (Set P.Address )

type Print    = JsonRpcNotification "print" String

type RpcAPI
    = Echo
    :<|> TxOutRefReport
    :<|> TxOutRefsReport
    :<|> UtxosReport
    :<|> TargetAddressesReport
    :<|> Print

type JsonRpcAPI = "json-rpc" :> RawJsonRpc RpcAPI

type GetTime = "time" :> Get '[PlainText] String

type PrintMessage = "print" :> ReqBody '[PlainText] String :> Post '[PlainText] NoContent

type RestAPI = "rest" :> (GetTime :<|> PrintMessage)

type API = JsonRpcAPI :<|> RestAPI

type NonEndpoint = "json-rpc" :> RawJsonRpc (JsonRpc "launch-missles" Int String Bool)
