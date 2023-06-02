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
module Cardano.Node.Types
    (
      -- * Logging types
      PABServerLogMsg (..)

     -- * Effects
    , ChainSyncHandle

    -- * Config types
    , PABServerConfig (..)
    , NodeMode (..)
    , _MockNode
    , _AlonzoNode
    )
        where

import Cardano.BM.Data.Tracer (ToObject)
import Cardano.BM.Data.Tracer.Extras (Tagged (Tagged), mkObjectStr)
import Cardano.Node.Emulator.Internal.Node (ChainEvent, SlotConfig, pNetworkId, testnet)
import Cardano.Node.Socket.Emulator.Chain (MockNodeServerChainState, fromEmulatorChainState)
import Cardano.Protocol.Socket.Client qualified as Client
import Control.Lens (makeLenses, makePrisms, view)
import Control.Monad.Freer.Extras.Log (LogMessage)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default, def)
import Data.Either (fromRight)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 qualified as F
import Data.Time.Units (Millisecond)
import Data.Time.Units.Extra ()
import GHC.Generics (Generic)
import Ledger (Block)
import Ledger.CardanoWallet (WalletNumber)
import Plutus.Contract.Trace qualified as Trace
import Prettyprinter (Pretty, pretty, viaShow, vsep, (<+>))
import Servant.Client (BaseUrl (BaseUrl, baseUrlPort), Scheme (Http))
import Wallet.Emulator (Wallet, WalletNumber (WalletNumber))
import Wallet.Emulator qualified as EM
import Wallet.Emulator.MultiAgent qualified as MultiAgent

import Cardano.Api qualified as C
import Cardano.Api.NetworkId.Extra (NetworkIdWrapper (unNetworkIdWrapper), testnetNetworkId)
import Cardano.BM.Tracing (toObject)
import Plutus.PAB.Arbitrary ()

-- Configuration ------------------------------------------------------------------------------------------------------

{- Note [Slot numbers in mock node]

The mock node has an internal clock that generates new slots in a regular
interval. Slots are identified by consecutive integers. What should the
initial slot number be? We can either set it to 0, so that the slot number
is the number of intervals that have passed since the process was started.
Or we can define an initial timestamp, so that the slot number is the number
of intervals since that timestamp.

The first option of counting from 0 is useful for integration tests where we
want the test outcome to be independent of when the test was run. This approach
is used in the PAB simulator.
The second option, counting from a timestamp, is more realistic and it is
useful for frontends that need to convert the slot number back to a timestamp.
We use this approach for the "proper" pab executable.

-}

-- | Which node we're connecting to
data NodeMode =
    MockNode -- ^ Connect to the PAB mock node.
    | AlonzoNode -- ^ Connect to an Alonzo node
    | NoChainSyncEvents -- ^ Do not connect to any node for chain sync events. Connect to Alonzo node for slot notifications.
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

makePrisms ''NodeMode

-- | Node server configuration
data PABServerConfig =
    PABServerConfig
        { pscBaseUrl                    :: BaseUrl
        -- ^ base url of the service
        , pscInitialTxWallets           :: [WalletNumber]
        -- ^ The wallets that receive money from the initial transaction.
        , pscSocketPath                 :: FilePath
        -- ^ Path to the socket used to communicate with the server.
        , pscKeptBlocks                 :: Integer
        -- ^ The number of blocks to keep for replaying to a newly connected clients
        , pscSlotConfig                 :: SlotConfig
        -- ^ Beginning of slot 0.
        , pscNetworkId                  :: NetworkIdWrapper
        -- ^ NetworkId that's used with the CardanoAPI.
        , pscProtocolParametersJsonPath :: Maybe FilePath
        -- ^ Path to a JSON file containing the protocol parameters
        , pscPassphrase                 :: Maybe Text
        -- ^ Wallet passphrase
        , pscNodeMode                   :: NodeMode
        -- ^ Whether to connect to an Alonzo node or a mock node
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)


defaultPABServerConfig :: PABServerConfig
defaultPABServerConfig =
    PABServerConfig
      -- See Note [pab-ports] in 'test/full/Plutus/PAB/CliSpec.hs'.
      { pscBaseUrl = BaseUrl Http "localhost" 9082 ""
      , pscInitialTxWallets =
          [ WalletNumber 1
          , WalletNumber 2
          , WalletNumber 3
          ]
      , pscSocketPath = "./node-server.sock"
      , pscKeptBlocks = 100
      , pscSlotConfig = def
      , pscNetworkId = testnetNetworkId
      , pscProtocolParametersJsonPath = Nothing
      , pscPassphrase = Nothing
      , pscNodeMode  = MockNode
      }

instance Default PABServerConfig where
  def = defaultPABServerConfig

instance Pretty PABServerConfig where
  pretty PABServerConfig{ pscBaseUrl, pscSocketPath, pscNetworkId, pscKeptBlocks } =
    vsep [ "Socket:" <+> pretty pscSocketPath
         , "Network Id:" <+> viaShow (unNetworkIdWrapper pscNetworkId)
         , "Port:" <+> viaShow (baseUrlPort pscBaseUrl)
         , "Security Param:" <+> pretty pscKeptBlocks
         ]

-- | The types of handles varies based on the type of clients (mocked or
-- real nodes) and we need a generic way of handling either type of response.
type ChainSyncHandle = Either (Client.ChainSyncHandle Block) (Client.ChainSyncHandle Client.ChainSyncEvent)

-- Logging ------------------------------------------------------------------------------------------------------------

-- | Top-level logging data type for structural logging
-- inside the PAB server.
data PABServerLogMsg =
    StartingSlotCoordination UTCTime Millisecond
    | StartingPABServer Int
    | ProcessingChainEvent ChainEvent
    deriving (Generic, Show, ToJSON, FromJSON)

instance Pretty PABServerLogMsg where
    pretty = \case
        StartingPABServer p      -> "Starting PAB Server on port" <+> pretty p
        StartingSlotCoordination initialSlotTime slotLength  ->
            "Starting slot coordination thread."
            <+> "Initial slot time:" <+> pretty (F.iso8601Show initialSlotTime)
            <+> "Slot length:" <+> viaShow slotLength
        ProcessingChainEvent e    -> "Processing chain event" <+> pretty e

instance ToObject PABServerLogMsg where
    toObject _ = \case
        StartingPABServer p      ->  mkObjectStr "Starting PAB Server on port " (Tagged @"port" p)
        StartingSlotCoordination i l  -> mkObjectStr "Starting slot coordination thread" (Tagged @"initial-slot-time" (F.iso8601Show  i), Tagged @"slot-length" l)
        ProcessingChainEvent e    ->  mkObjectStr "Processing chain event" (Tagged @"event" e)
