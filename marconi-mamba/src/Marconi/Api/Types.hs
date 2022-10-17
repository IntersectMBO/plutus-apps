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
-- |
-- This module provides support for writing handlers for JSON-RPC endpoints
module Marconi.Api.Types where

import Cardano.Api (Address, NetworkId, ShelleyAddr)
import Control.Concurrent.QSemN
import Control.Lens (makeClassy)
import Data.List.NonEmpty (NonEmpty)
import Database.SQLite.Simple (Connection)
import Network.Wai.Handler.Warp (Settings)

type CardanoAddress = Address ShelleyAddr

-- | Typre represents non empty list of Bech32 compatable addresses"
type TargetAddresses = NonEmpty CardanoAddress

-- | Type represents http port for JSON-RPC
type RpcPortNumber = Int

data CliArgs = CliArgs
  { socket          :: FilePath             -- ^ POSIX socket file to communicate with cardano node
  , dbPath          :: FilePath             -- ^ filepath to local sqlite for utxo index table
  , httpPort        :: Maybe Int            -- ^ optional tcp/ip port number for JSON-RPC http server
  , networkId       :: NetworkId            -- ^ cardano network id
  , targetAddresses :: TargetAddresses      -- ^ white-space sepparated list of Bech32 Cardano Shelley addresses
  } deriving (Show)

newtype DBConfig = DBConfig {
    _utxoConn ::  Connection
    }
makeClassy ''DBConfig

data DBQueryEnv = DBQueryEnv
    { _dbConf         :: DBConfig           -- ^ path to dqlite db
    , _queryQSem      :: QSemN           -- ^ used to serialize addess to sqlite
    , _queryAddresses :: TargetAddresses    -- ^ user provided addresses to filter
    }
makeClassy ''DBQueryEnv

-- | JSON-RPC configuration
data JsonRpcEnv = JsonRpcEnv {
    _httpSettings :: Settings               -- ^ HTTP server setting
    , _queryEnv   :: DBQueryEnv             -- ^ used for query sqlite
    }
makeClassy ''JsonRpcEnv
