{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- This module provides support for writing handlers for JSON-RPC endpoints
module Marconi.Api.Types
    ( CardanoAddress
    , TargetAddresses
    , RpcPortNumber
    , CliArgs (..)
    , DBConfig (..)
    , HasDBConfig (..)
    , DBQueryEnv (..)
    , HasDBQueryEnv (..)
    , JsonRpcEnv (..)
    , HasJsonRpcEnv (..)
    , UtxoRowWrapper (..)
    , UtxoTxOutReport (..)
    , Address
                         )  where

import Cardano.Api qualified (Address, NetworkId, ShelleyAddr)
import Control.Concurrent.QSemN (QSemN)
import Control.Lens (makeClassy)
import Data.Aeson (FromJSON, ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import Ledger (TxOutRef)
import Ledger.Address (Address)
import Marconi.Index.Utxo (UtxoRow (UtxoRow))
import Network.Wai.Handler.Warp (Settings)

type CardanoAddress = Cardano.Api.Address Cardano.Api.ShelleyAddr

-- | Typre represents non empty list of Bech32 compatable addresses"
type TargetAddresses = NonEmpty CardanoAddress

-- | Type represents http port for JSON-RPC
type RpcPortNumber = Int

data CliArgs = CliArgs
  { socket          :: FilePath                 -- ^ POSIX socket file to communicate with cardano node
  , dbPath          :: FilePath                 -- ^ filepath to local sqlite for utxo index table
  , httpPort        :: Maybe Int                -- ^ optional tcp/ip port number for JSON-RPC http server
  , networkId       :: Cardano.Api.NetworkId   -- ^ cardano network id
  , targetAddresses :: !TargetAddresses          -- ^ white-space sepparated list of Bech32 Cardano Shelley addresses
  } deriving (Show)

newtype DBConfig = DBConfig {
    _utxoConn ::  Connection
    }
makeClassy ''DBConfig

data DBQueryEnv = DBQueryEnv
    { _dbConf         :: DBConfig           -- ^ path to dqlite db
    , _queryQSem      :: QSemN           -- ^ used to serialize addess to sqlite
    , _queryAddresses :: TargetAddresses    -- ^ user provided addresses to filter
    , _network        :: Cardano.Api.NetworkId   -- ^ cardano network id
    }
makeClassy ''DBQueryEnv

-- | JSON-RPC configuration
data JsonRpcEnv = JsonRpcEnv {
    _httpSettings :: Settings               -- ^ HTTP server setting
    , _queryEnv   :: DBQueryEnv             -- ^ used for query sqlite
    }
makeClassy ''JsonRpcEnv

data UtxoTxOutReport = UtxoTxOutReport
    { bech32Address :: Text
    , txOutRefs     :: Set TxOutRef
    } deriving (Eq, Ord, Generic)

instance ToJSON UtxoTxOutReport where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON UtxoTxOutReport where

newtype UtxoRowWrapper = UtxoRowWrapper UtxoRow deriving Generic

instance Ord UtxoRowWrapper where
    compare (UtxoRowWrapper (UtxoRow a _) ) ( UtxoRowWrapper (UtxoRow b _)) =  compare a b

instance Eq UtxoRowWrapper where
    (UtxoRowWrapper  (UtxoRow a1 t1) ) == ( UtxoRowWrapper (UtxoRow a2 t2) ) = a1 == a2 &&  t1 == t2

instance ToJSON UtxoRowWrapper where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON UtxoRow where
    toEncoding = genericToEncoding defaultOptions
