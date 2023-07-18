{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutus.ChainIndex.Config(
  ChainIndexConfig(..),
  DecodeConfigException(..),
  defaultConfig,
  -- * Lenses
  socketPath,
  dbPath,
  port,
  networkId,
  securityParam,
  slotConfig,
  storeFrom,
  appendTransactionQueueSize
  ) where

import Cardano.Api (BlockNo (BlockNo), NetworkId)
import Cardano.Node.Emulator.Internal.Node.TimeSlot (SlotConfig (SlotConfig, scSlotLength, scSlotZeroTime))
import Control.Exception (Exception)
import Control.Lens (makeLensesFor)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Test (testnet)
import Numeric.Natural (Natural)
import Ouroboros.Network.Magic (NetworkMagic)
import Prettyprinter (Pretty (pretty), viaShow, vsep, (<+>))

data ChainIndexConfig = ChainIndexConfig
  { cicSocketPath                 :: String
  , cicDbPath                     :: String
  , cicPort                       :: Int
  , cicNetworkId                  :: NetworkId
  , cicSecurityParam              :: Int -- ^ The number of blocks after which a transaction cannot be rolled back anymore
  , cicSlotConfig                 :: SlotConfig
  , cicStoreFrom                  :: BlockNo -- ^ Only store transactions from this block number onward
  , cicAppendTransactionQueueSize :: Natural -- ^ The size of the queue and a number of transactions to collect before writing to the database
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | For some reason these are not defined anywhere, and these are the
--   reason for the -Wno-orphans option.
deriving anyclass instance FromJSON NetworkId
deriving anyclass instance ToJSON NetworkId
deriving anyclass instance FromJSON NetworkMagic
deriving anyclass instance ToJSON NetworkMagic
deriving anyclass instance FromJSON BlockNo
deriving anyclass instance ToJSON BlockNo

-- | These settings work with the main testnet
defaultConfig :: ChainIndexConfig
defaultConfig = ChainIndexConfig
  { cicSocketPath = "testnet/node.sock"
  , cicDbPath     = "/tmp/chain-index.db"
  , cicPort       = 9083
  , cicNetworkId  = testnet
  , cicSecurityParam = 2160
  , cicSlotConfig =
      SlotConfig
        { scSlotZeroTime = 1596059091000
        , scSlotLength   = 1000
        }
  , cicStoreFrom = BlockNo 0
  , cicAppendTransactionQueueSize = 500
  }

instance Pretty ChainIndexConfig where
  pretty ChainIndexConfig{cicSocketPath, cicDbPath, cicPort, cicNetworkId, cicSecurityParam, cicStoreFrom, cicAppendTransactionQueueSize} =
    vsep [ "Socket:" <+> pretty cicSocketPath
         , "Db:" <+> pretty cicDbPath
         , "Port:" <+> pretty cicPort
         , "Network Id:" <+> viaShow cicNetworkId
         , "Security Param:" <+> pretty cicSecurityParam
         , "Store from:" <+> viaShow cicStoreFrom
         , "Append transaction queue size:" <+> viaShow cicAppendTransactionQueueSize
         ]

makeLensesFor [
  ("cicSocketPath", "socketPath"),
  ("cicDbPath", "dbPath"),
  ("cicPort", "port"),
  ("cicNetworkId", "networkId"),
  ("cicSecurityParam", "securityParam"),
  ("cicSlotConfig", "slotConfig"),
  ("cicStoreFrom", "storeFrom"),
  ("cicAppendTransactionQueueSize", "appendTransactionQueueSize")
  ] 'ChainIndexConfig

newtype DecodeConfigException = DecodeConfigException String
  deriving stock Show
  deriving anyclass Exception
