{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- This module provides support for writing handlers for JSON-RPC endpoints
module Marconi.Api.Types  where

import Control.Concurrent.STM.TMVar (TMVar)
import Control.Exception (Exception)
import Control.Lens (makeClassy)
import Data.Aeson (ToJSON (toEncoding, toJSON), defaultOptions, genericToEncoding, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (Settings)

import Cardano.Api qualified as C
import Marconi.Index.Utxo qualified as Utxo
import Marconi.Types as Export (TargetAddresses)

-- | Type represents http port for JSON-RPC
type RpcPortNumber = Int

data CliArgs = CliArgs
  { socket          :: FilePath             -- ^ POSIX socket file to communicate with cardano node
  , dbPath          :: FilePath             -- ^ filepath to local sqlite for utxo index table
  , httpPort        :: Maybe Int            -- ^ optional tcp/ip port number for JSON-RPC http server
  , networkId       :: C.NetworkId          -- ^ cardano network id
  , targetAddresses :: TargetAddresses      -- ^ white-space sepparated list of Bech32 Cardano Shelley addresses
  } deriving (Show)

newtype UtxoIndexerWrapper = UtxoIndexerWrapper
    { unWrap :: TMVar Utxo.UtxoIndexer           -- ^ for query thread to access in-memory utxos
    }

data UtxoIndexerEnv = UtxoIndexerEnv
    { _uiIndexer    :: UtxoIndexerWrapper
    , _uiQaddresses :: TargetAddresses        -- ^ user provided addresses to filter
    }
makeClassy ''UtxoIndexerEnv

-- | JSON-RPC configuration
data JsonRpcEnv = JsonRpcEnv
    { _httpSettings :: Settings               -- ^ HTTP server setting
    , _queryEnv     :: UtxoIndexerEnv           -- ^ used for query sqlite
    }
makeClassy ''JsonRpcEnv

instance ToJSON C.ScriptData where
  toJSON = C.scriptDataToJson C.ScriptDataJsonDetailedSchema


instance ToJSON Utxo.Utxo where
  toJSON (Utxo.Utxo (Utxo.UtxoAddress addr) tId tIx dtum dtumHash val scrpt scrptHash) = object
    ["address"            .=  addr
    , "txId"              .= tId
    , "txIx"              .= tIx
    , "datum"             .= dtum
    , "datumHash"         .= dtumHash
    , "value"             .= val
    , "inlineScript"      .= scrpt
    , "inlineScriptHash"  .= scrptHash
    ]

data UtxoReport = UtxoReport
    { urAddress :: Text
    , urReport  :: [Utxo.UtxoRow]
    } deriving (Eq, Ord, Generic)

instance ToJSON UtxoReport where
    toEncoding = genericToEncoding defaultOptions

newtype UtxoRowWrapper = UtxoRowWrapper Utxo.UtxoRow
  deriving (Eq, Ord, Show, Generic)

instance ToJSON UtxoRowWrapper where
    toEncoding = genericToEncoding defaultOptions

data QueryExceptions
    = AddressNotInListError QueryExceptions
    | AddressConversionError QueryExceptions
    | TxRefConversionError QueryExceptions
    | QueryError String
    deriving stock Show
    deriving anyclass  Exception


instance ToJSON C.AddressAny where
    toJSON  = Aeson.String . C.serialiseAddress

-- instance ToJSON ByteString where toJSON = Data.Aeson.String . decodeUtf8

instance ToJSON (Utxo.StorableQuery Utxo.UtxoHandle) where
  toJSON (Utxo.UtxoAddress addr) = toJSON addr

instance ToJSON C.BlockNo

instance ToJSON Utxo.UtxoRow where
  toJSON (Utxo.UtxoRow u b s h) = object
    [ "utxo" .= u
    ,  "blockNo" .= b
    ,  "slotNo" .= s
    ,  "blockHeaderHash" .= h]
