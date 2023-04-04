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

-- | This module provides support for writing handlers for JSON-RPC endpoints.
module Marconi.Sidechain.Api.Types where

import Cardano.Api qualified as C
import Control.Concurrent.STM.TMVar (TMVar)
import Control.Exception (Exception)
import Control.Lens (makeLenses)
import Marconi.ChainIndex.Indexers.EpochState (EpochStateHandle)
import Marconi.ChainIndex.Indexers.Utxo (UtxoHandle)
import Marconi.ChainIndex.Types as Export (TargetAddresses)
import Marconi.Core.Storable (State, StorableQuery)
import Network.Wai.Handler.Warp (Settings)

-- | Type represents http port for JSON-RPC

data CliArgs = CliArgs
  { socket          :: !FilePath -- ^ POSIX socket file to communicate with cardano node
  , nodeConfigPath  :: !FilePath -- ^ Path to the node config
  , dbDir           :: !FilePath -- ^ Directory path containing the SQLite database files
  , httpPort        :: !(Maybe Int) -- ^ optional tcp/ip port number for JSON-RPC http server
  , networkId       :: !C.NetworkId -- ^ cardano network id
  , targetAddresses :: !(Maybe TargetAddresses) -- ^ white-space sepparated list of Bech32 Cardano Shelley addresses
  } deriving (Show)

-- | JSON-RPC as well as the Query Indexer Env
data SidechainEnv = SidechainEnv
    { _sidechainEnvHttpSettings :: !Settings -- ^ HTTP server setting
    , _sidechainEnvIndexers     :: !SidechainIndexers -- ^ Used for query the indexers
    }

-- | Should contain all the indexers required by Sidechain.
data SidechainIndexers = SidechainIndexers
    { _sidechainAddressUtxoIndexer :: !AddressUtxoIndexerEnv
    -- ^ For query thread to access in-memory utxos
    , _sidechainEpochStateIndexer  :: !EpochStateIndexerEnv
    -- ^ For query thread to access in-memory epoch state data
    }

data AddressUtxoIndexerEnv = AddressUtxoIndexerEnv
    { _addressUtxoIndexerEnvTargetAddresses :: !(Maybe TargetAddresses)
    , _addressUtxoIndexerEnvIndexer         :: !(TMVar (State UtxoHandle))
    }

newtype EpochStateIndexerEnv = EpochStateIndexerEnv
    { _epochStateIndexerEnvIndexer         :: TMVar (State EpochStateHandle)
    }

data QueryExceptions
    = AddressConversionError !QueryExceptions
    | QueryError !String
    | UnexpectedQueryResult !(StorableQuery UtxoHandle)
    deriving stock Show
    deriving anyclass  Exception

makeLenses ''SidechainEnv
makeLenses ''SidechainIndexers
makeLenses ''AddressUtxoIndexerEnv
makeLenses ''EpochStateIndexerEnv
