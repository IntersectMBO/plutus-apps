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
module Marconi.Api.Types
    (TargetAddresses
    , RpcPortNumber
    , CliArgs (..)
    , DBConfig (..)
    , DBQueryEnv (..)
    , HasDBQueryEnv (..)
    , JsonRpcEnv (..)
    , HasJsonRpcEnv (..)
    , UtxoRowWrapper (..)
    , UtxoTxOutReport (..)
    , UtxoQueryComm (..)
    , HasUtxoQueryComm (..)
    , QueryExceptions (..)
                         )  where
import Cardano.Api (AddressAny, NetworkId, anyAddressInShelleyBasedEra)
import Control.Exception (Exception)
import Control.Lens (makeClassy)
import Data.Aeson (ToJSON (toEncoding, toJSON), defaultOptions, genericToEncoding)
import Data.Set (Set)
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import Marconi.Index.Utxo (UtxoRow (UtxoRow))
import Marconi.Indexers (HasUtxoQueryComm (indexer, queryReq), UtxoQueryComm (UtxoQueryComm, _Indexer, _QueryReq))
import Marconi.Types as Export (CurrentEra, TargetAddresses, TxOutRef)
import Network.Wai.Handler.Warp (Settings)

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
    utxoConn ::  Connection
    }

data DBQueryEnv = DBQueryEnv
    { _DbConf         :: DBConfig               -- ^ path to dqlite db
    , _QueryComm      :: UtxoQueryComm
    , _QueryAddresses :: TargetAddresses        -- ^ user provided addresses to filter
    , _Network        :: Cardano.Api.NetworkId  -- ^ cardano network id
    }
makeClassy ''DBQueryEnv

-- | JSON-RPC configuration
data JsonRpcEnv = JsonRpcEnv
    { _HttpSettings :: Settings               -- ^ HTTP server setting
    , _QueryEnv     :: DBQueryEnv             -- ^ used for query sqlite
    }
makeClassy ''JsonRpcEnv

data UtxoTxOutReport = UtxoTxOutReport
    { bech32Address :: Text
    , txOutRefs     :: Set TxOutRef
    } deriving (Eq, Ord, Generic)

instance ToJSON UtxoTxOutReport where
    toEncoding = genericToEncoding defaultOptions

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

data QueryExceptions
    = AddressNotInListError QueryExceptions
    | AddressConversionError QueryExceptions
    | TxRefConversionError QueryExceptions
    | QueryError String
    deriving stock Show
    deriving anyclass  Exception
