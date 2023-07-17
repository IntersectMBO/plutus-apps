{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-| This module exports data types for logging, events and configuration
-}
module Cardano.Node.Socket.Emulator.Types where

import Cardano.Api (NetworkId, Value)
import Cardano.Node.Emulator.Internal.Node (ChainControlEffect, ChainEffect, ChainEvent, SlotConfig, fromBlockchain,
                                            testnet, unsafeMakeValid)
import Cardano.Node.Socket.Emulator.Chain (MockNodeServerChainState, fromEmulatorChainState)
import Control.Lens (makeLenses)
import Control.Monad.Freer.Extras.Log (LogMessage, LogMsg)
import Control.Monad.Freer.State qualified as Eff
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default, def)
import Data.Map qualified as Map
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 qualified as F
import Data.Time.Units (Millisecond)
import Data.Time.Units.Extra ()
import GHC.Generics (Generic)
import Ledger.Address (CardanoAddress)
import Ledger.CardanoWallet
import Ledger.Index (createGenesisTransaction)
import Prettyprinter (Pretty, pretty, viaShow, vsep, (<+>))
import Servant.Client (BaseUrl (BaseUrl, baseUrlPort), Scheme (Http))


-- | Node server configuration
data NodeServerConfig =
    NodeServerConfig
        { nscBaseUrl                    :: BaseUrl
        -- ^ base url of the service
        , nscInitialTxWallets           :: [WalletNumber]
        -- ^ The wallets that receive money from the initial transaction.
        , nscSocketPath                 :: FilePath
        -- ^ Path to the socket used to communicate with the server.
        , nscKeptBlocks                 :: Integer
        -- ^ The number of blocks to keep for replaying to newly connected clients
        , nscSlotConfig                 :: SlotConfig
        -- ^ Beginning of slot 0.
        , nscNetworkId                  :: NetworkId
        -- ^ NetworkId that's used with the CardanoAPI.
        , nscProtocolParametersJsonPath :: Maybe FilePath
        -- ^ Path to a JSON file containing the protocol parameters
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

defaultNodeServerConfig :: NodeServerConfig
defaultNodeServerConfig =
    NodeServerConfig
      -- See Note [pab-ports] in 'test/full/Plutus/PAB/CliSpec.hs'.
      { nscBaseUrl = BaseUrl Http "localhost" 9082 ""
      , nscInitialTxWallets =
          [ WalletNumber 1
          , WalletNumber 2
          , WalletNumber 3
          , WalletNumber 4
          , WalletNumber 5
          , WalletNumber 6
          , WalletNumber 7
          , WalletNumber 8
          , WalletNumber 9
          , WalletNumber 10
          ]
      , nscSocketPath = "/tmp/node-server.sock"
      , nscKeptBlocks = 100
      , nscSlotConfig = def
      , nscNetworkId = testnet
      , nscProtocolParametersJsonPath = Nothing
      }

instance Default NodeServerConfig where
  def = defaultNodeServerConfig

instance Pretty NodeServerConfig where
  pretty NodeServerConfig{ nscBaseUrl, nscSocketPath, nscNetworkId, nscKeptBlocks } =
    vsep [ "Socket:" <+> pretty nscSocketPath
         , "Network Id:" <+> viaShow nscNetworkId
         , "Port:" <+> viaShow (baseUrlPort nscBaseUrl)
         , "Security Param:" <+> pretty nscKeptBlocks
         ]


type NodeServerEffects m
     = '[ ChainControlEffect
        , ChainEffect
        , Eff.State MockNodeServerChainState
        , Eff.State AppState
        , LogMsg ChainEvent
        , m]

-- | Application State
data AppState =
    AppState
        { _chainState   :: MockNodeServerChainState -- ^ blockchain state
        , _eventHistory :: [LogMessage ChainEvent] -- ^ history of all log messages
        }
    deriving (Show)

makeLenses 'AppState

-- | 'ChainState' with initial values
initialChainState :: MonadIO m => Map.Map CardanoAddress Value -> m MockNodeServerChainState
initialChainState =
    fromEmulatorChainState . fromBlockchain . pure . pure . unsafeMakeValid . createGenesisTransaction


-- Logging ------------------------------------------------------------------------------------------------------------

-- | Top-level logging data type for structural logging
-- inside the CNSE server.
data CNSEServerLogMsg =
    StartingSlotCoordination UTCTime Millisecond
    | StartingCNSEServer Int
    | ProcessingChainEvent ChainEvent
    deriving (Generic, Show, ToJSON, FromJSON)

instance Pretty CNSEServerLogMsg where
    pretty = \case
        StartingSlotCoordination initialSlotTime slotLength  ->
            "Starting slot coordination thread."
            <+> "Initial slot time:" <+> pretty (F.iso8601Show initialSlotTime)
            <+> "Slot length:" <+> viaShow slotLength
        StartingCNSEServer p   -> "Starting Cardano Node Emulator on port" <+> pretty p
        ProcessingChainEvent e -> "Processing chain event" <+> pretty e
