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
module Marconi.Sidechain.Api.Types  where

import Control.Concurrent.STM.TMVar (TMVar)
import Control.Exception (Exception)
import Control.Lens (makeClassy)
import Data.Aeson (FromJSON, ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (Settings)

import Cardano.Api qualified as C
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Types as Export (TargetAddresses)

-- | Type represents http port for JSON-RPC

data CliArgs = CliArgs
  { socket          :: FilePath -- ^ POSIX socket file to communicate with cardano node
  , dbDir           :: FilePath -- ^ Directory path containing the SQLite database files
  , httpPort        :: Maybe Int -- ^ optional tcp/ip port number for JSON-RPC http server
  , networkId       :: C.NetworkId -- ^ cardano network id
  , targetAddresses :: Maybe TargetAddresses -- ^ white-space sepparated list of Bech32 Cardano Shelley addresses
  } deriving (Show)

-- | Should contain all the indexers required by Sidechain
newtype IndexerWrapper = IndexerWrapper
    { unWrapUtxoIndexer :: TMVar Utxo.UtxoIndexer     -- ^ for query thread to access in-memory utxos
    }

data IndexerEnv = IndexerEnv
    { _uiIndexer    :: IndexerWrapper
    , _uiQaddresses :: Maybe TargetAddresses        -- ^ user provided addresses to filter
    }
makeClassy ''IndexerEnv

-- | JSON-RPC as well as the Query Indexer Env
data SidechainEnv = SidechainEnv
    { _httpSettings :: Settings               -- ^ HTTP server setting
    , _queryEnv     :: IndexerEnv         -- ^ used for query sqlite
    }
makeClassy ''SidechainEnv

data QueryExceptions
    = AddressConversionError QueryExceptions
    | QueryError String
    deriving stock Show
    deriving anyclass  Exception

data UtxoQueryResult = UtxoQueryResult
    { uqAddress :: Text
    , uqResults :: ![Utxo.UtxoRow]
    } deriving (Eq, Ord, Generic, Show)

instance ToJSON UtxoQueryResult where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON UtxoQueryResult
