-- | Defines REST and JSON-RPC routes

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Marconi.Sidechain.Api.Routes where

import Cardano.Api qualified as C
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object), object, (.:), (.=))
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Marconi.ChainIndex.Indexers.EpochStakepoolSize qualified as EpochSPD
import Marconi.ChainIndex.Indexers.MintBurn qualified as MpsTx
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Network.JsonRpc.Types (JsonRpc, RawJsonRpc)
import Servant.API (Get, JSON, PlainText, (:<|>), (:>))

-- | marconi-sidechain APIs
type API = JsonRpcAPI :<|> RestAPI

----------------------------------------------
--  RPC types
--  methodName -> parameter(s) -> return-type
----------------------------------------------

-- | JSON-RPC API, endpoint
type JsonRpcAPI = "json-rpc" :> RawJsonRpc RpcAPI

-- | RPC routes
type RpcAPI = RpcEchoMethod
         :<|> RpcTargetAddressesMethod
         :<|> RpcCurrentSyncedPointMethod
         :<|> RpcAddressUtxoMethod
         :<|> RpcMintingPolicyHashTxMethod
         :<|> RpcEpochStakePoolDelegationMethod
         :<|> RpcEpochNonceMethod

type RpcEchoMethod = JsonRpc "echo" String String String

type RpcTargetAddressesMethod = JsonRpc "getTargetAddresses" String String [Text]

type RpcCurrentSyncedPointMethod =
    JsonRpc "getCurrentSyncedPoint"
            String
            String
            CurrentSyncedPointResult

type RpcAddressUtxoMethod =
    JsonRpc "getUtxoFromAddress"
            String
            String
            AddressUtxoResult

type RpcMintingPolicyHashTxMethod =
    JsonRpc "getTxWithMintingPolicy"
            String
            String
            MintingPolicyHashTxResult

type RpcEpochStakePoolDelegationMethod =
    JsonRpc "getStakePoolDelegationByEpoch"
            Word64
            String
            EpochStakePoolDelegationResult

type RpcEpochNonceMethod =
    JsonRpc "getNonceByEpoch"
            Word64
            String
            EpochNonceResult

--------------------
-- REST related ---
--------------------

-- | REST API, endpoints
type RestAPI = "rest" :> (GetTime :<|> GetTargetAddresses)

type GetTime = "time" :> Get '[PlainText] String

type GetTargetAddresses = "addresses" :> Get '[JSON] [Text]

--------------------------
-- Query and Result types
--------------------------

newtype CurrentSyncedPointResult = CurrentSyncedPointResult C.ChainPoint
    deriving (Eq, Ord, Generic, Show)

instance ToJSON CurrentSyncedPointResult where
    toJSON (CurrentSyncedPointResult C.ChainPointAtGenesis) =
        object
            [ "tag" .= ("ChainPointAtGenesis" :: Text) ]
    toJSON (CurrentSyncedPointResult (C.ChainPoint (C.SlotNo slotNo) bhh)) =
        object
            [ "tag" .= ("ChainPoint" :: Text)
            , "slotNo" .= slotNo
            , "blockHeaderHash" .= bhh
            ]

instance FromJSON CurrentSyncedPointResult where
    parseJSON (Object v) = do
        (tag :: Text) <- v .: "tag"
        if tag == "ChainPointAtGenesis"
        then
            pure $ CurrentSyncedPointResult C.ChainPointAtGenesis
        else if tag == "ChainPoint"
        then do
            cp <- C.ChainPoint
                <$> v .: "slotNo"
                <*> v .: "blockHeaderHash"
            pure $ CurrentSyncedPointResult cp
        else
            mempty
    parseJSON _ = mempty

newtype AddressUtxoResult = AddressUtxoResult [Utxo.UtxoRow]
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

newtype MintingPolicyHashTxResult =
    MintingPolicyHashTxResult [MpsTx.TxMintRow]
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

newtype EpochStakePoolDelegationResult =
    EpochStakePoolDelegationResult [EpochSPD.EpochSPDRow]
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

newtype EpochNonceResult =
    EpochNonceResult String
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)
