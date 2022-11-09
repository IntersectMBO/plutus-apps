{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Marconi.Api.Routes where

import Data.Set (Set)
import Data.Text (Text)
import Marconi.Api.Types (UtxoTxOutReport)
import Marconi.JsonRpc.Types (JsonRpc, JsonRpcNotification, RawJsonRpc)
import Servant.API (Get, JSON, NoContent, PlainText, Post, ReqBody, (:<|>), (:>))
type Echo                   = JsonRpc "echo" String String String
type TxOutRefReport         = JsonRpc "utxoTxOutReport" String String UtxoTxOutReport
type TxOutRefsReport        = JsonRpc "utxoTxOutReports" Int String (Set UtxoTxOutReport)
type TargetAddressesReport  = JsonRpc "addressesBech32Report" Int String (Set Text)

type Print    = JsonRpcNotification "print" String

type RpcAPI
    = Echo
    :<|> TxOutRefReport
    :<|> TxOutRefsReport
    :<|> TargetAddressesReport
    :<|> Print

type JsonRpcAPI = "json-rpc" :> RawJsonRpc RpcAPI

type GetTime = "time" :> Get '[PlainText] String

type GetTargetAddresses = "addresses" :> Get '[JSON] (Set Text)

type PrintMessage = "print" :> ReqBody '[PlainText] String :> Post '[PlainText] NoContent

type RestAPI
    = "rest"
    :> (GetTime
        :<|> GetTargetAddresses
        :<|> PrintMessage)

type API = JsonRpcAPI :<|> RestAPI

type NonEndpoint = "json-rpc" :> RawJsonRpc (JsonRpc "launch-missles" Int String Bool)
