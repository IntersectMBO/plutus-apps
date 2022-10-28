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
    (
    module Marconi.Api.Types,
    module Export,
    ) where

import Cardano.Api (AddressAny, NetworkId, anyAddressInShelleyBasedEra)
import Control.Concurrent.QSemN (QSemN)
import Control.Lens (makeClassy)
import Data.Aeson (ToJSON (toEncoding, toJSON), defaultOptions, genericToEncoding)
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (Settings)

import Marconi.Index.Utxo (UtxoRow (UtxoRow))
import Marconi.Types as Export (CurrentEra, TargetAddresses, TxOutRef)

-- | Type represents http port for JSON-RPC
type RpcPortNumber = Int

data CliArgs = CliArgs
  { socket          :: FilePath        -- ^ POSIX socket file to communicate with cardano node
  , dbPath          :: FilePath        -- ^ filepath to local sqlite for utxo index table
  , httpPort        :: Maybe Int       -- ^ optional tcp/ip port number for JSON-RPC http server
  , networkId       :: NetworkId       -- ^ cardano network id
  , targetAddresses :: TargetAddresses -- ^ white-space sepparated list of Bech32 Cardano Shelley addresses
  } deriving (Show)

newtype DBConfig = DBConfig {
    _utxoConn ::  Connection
    }
makeClassy ''DBConfig

data DBQueryEnv = DBQueryEnv
    { _dbConf         :: DBConfig        -- ^ path to dqlite db
    , _queryQSem      :: QSemN           -- ^ used to serialize addess to sqlite
    , _queryAddresses :: TargetAddresses -- ^ user provided addresses to filter
    }
makeClassy ''DBQueryEnv

-- | JSON-RPC configuration
data JsonRpcEnv = JsonRpcEnv {
    _httpSettings :: Settings               -- ^ HTTP server setting
    , _queryEnv   :: DBQueryEnv             -- ^ used for query sqlite
    }
makeClassy ''JsonRpcEnv

newtype UtxoRowWrapper = UtxoRowWrapper UtxoRow deriving Generic

instance Ord UtxoRowWrapper where
    compare (UtxoRowWrapper (UtxoRow a _) ) ( UtxoRowWrapper (UtxoRow b _)) =  compare a b

instance Eq UtxoRowWrapper where
    (UtxoRowWrapper  (UtxoRow a1 t1) ) == ( UtxoRowWrapper (UtxoRow a2 t2) ) = a1 == a2 &&  t1 == t2

instance ToJSON AddressAny where
  toJSON = toJSON . anyAddressInShelleyBasedEra @CurrentEra

instance ToJSON UtxoRowWrapper where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON UtxoRow where
    toEncoding = genericToEncoding defaultOptions
