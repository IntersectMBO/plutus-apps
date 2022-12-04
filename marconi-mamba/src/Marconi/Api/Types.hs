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
    , DBQueryEnv (..)
    , HasDBQueryEnv (..)
    , JsonRpcEnv (..)
    , HasJsonRpcEnv (..)
    , UtxoRowWrapper (..)
    , UtxoTxOutReport (..)
    , UtxoQueryTMVar (..)
    , QueryExceptions (..)
                         )  where
import Control.Exception (Exception)
import Control.Lens (makeClassy)
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (Settings)

import Cardano.Api (NetworkId)
import Marconi.Index.Utxo (UtxoRow)
import Marconi.Indexers (UtxoQueryTMVar (UtxoQueryTMVar, unUtxoIndex))
import Marconi.Types as Export (TargetAddresses)

-- | Type represents http port for JSON-RPC
type RpcPortNumber = Int

data CliArgs = CliArgs
  { socket          :: FilePath             -- ^ POSIX socket file to communicate with cardano node
  , dbPath          :: FilePath             -- ^ filepath to local sqlite for utxo index table
  , httpPort        :: Maybe Int            -- ^ optional tcp/ip port number for JSON-RPC http server
  , networkId       :: NetworkId            -- ^ cardano network id
  , targetAddresses :: TargetAddresses      -- ^ white-space sepparated list of Bech32 Cardano Shelley addresses
  } deriving (Show)


data DBQueryEnv = DBQueryEnv
    { _queryTMVar     :: UtxoQueryTMVar
    , _queryAddresses :: TargetAddresses        -- ^ user provided addresses to filter
    }
makeClassy ''DBQueryEnv

-- | JSON-RPC configuration
data JsonRpcEnv = JsonRpcEnv
    { _httpSettings :: Settings               -- ^ HTTP server setting
    , _queryEnv     :: DBQueryEnv             -- ^ used for query sqlite
    }
makeClassy ''JsonRpcEnv

data UtxoTxOutReport = UtxoTxOutReport
    { bech32Address :: Text
    , utxoReport    :: [UtxoRow]
    } deriving (Eq, Ord, Generic)

instance ToJSON UtxoTxOutReport where
    toEncoding = genericToEncoding defaultOptions

newtype UtxoRowWrapper = UtxoRowWrapper UtxoRow deriving (Eq, Ord, Show, Generic)

-- instance ToJSON AddressAny where toJSON = toJSON . anyAddressInShelleyBasedEra @CurrentEra

instance ToJSON UtxoRowWrapper where
    toEncoding = genericToEncoding defaultOptions

-- instance ToJSON UtxoRow where toEncoding = genericToEncoding defaultOptions

data QueryExceptions
    = AddressNotInListError QueryExceptions
    | AddressConversionError QueryExceptions
    | TxRefConversionError QueryExceptions
    | QueryError String
    deriving stock Show
    deriving anyclass  Exception
