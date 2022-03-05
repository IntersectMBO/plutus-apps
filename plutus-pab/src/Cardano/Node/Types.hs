{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

{-| This module exports data types for logging, events and configuration
-}
module Cardano.Node.Types
    (
      -- * Logging types
      PABServerLogMsg (..)

     -- * Event types
    , BlockEvent (..)

     -- * Effects
    , NodeServerEffects
    , ChainSyncHandle

     -- *  State types
    , AppState (..)
    , initialAppState
    , initialChainState

    -- * Lens functions
    , chainState
    , eventHistory

    -- * Config types
    , PABServerConfig (..)
    , NodeMode (..)
    , _MockNode
    , _AlonzoNode

    -- * newtype wrappers
    , NodeUrl (..)
    )
        where

import Cardano.BM.Data.Tracer (ToObject)
import Cardano.BM.Data.Tracer.Extras (Tagged (Tagged), mkObjectStr)
import Cardano.Chain (MockNodeServerChainState, fromEmulatorChainState)
import Cardano.Protocol.Socket.Client qualified as Client
import Cardano.Protocol.Socket.Mock.Client qualified as Client
import Control.Lens (makeLenses, makePrisms, view)
import Control.Monad.Freer.Extras.Log (LogMessage, LogMsg)
import Control.Monad.Freer.Reader (Reader)
import Control.Monad.Freer.State (State)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default, def)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 qualified as F
import Data.Time.Units (Millisecond)
import Data.Time.Units.Extra ()
import GHC.Generics (Generic)
import Ledger (Block, Tx, txId)
import Ledger.CardanoWallet (WalletNumber)
import Ledger.TimeSlot (SlotConfig)
import Plutus.Contract.Trace qualified as Trace
import Prettyprinter (Pretty, pretty, viaShow, vsep, (<+>))
import Servant.Client (BaseUrl (BaseUrl, baseUrlPort), Scheme (Http))
import Wallet.Emulator (Wallet, WalletNumber (WalletNumber))
import Wallet.Emulator qualified as EM
import Wallet.Emulator.Chain (ChainControlEffect, ChainEffect, ChainEvent)
import Wallet.Emulator.MultiAgent qualified as MultiAgent

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

newtype NodeUrl = NodeUrl BaseUrl
    deriving (Show, Eq) via BaseUrl

-- | Which node we're connecting to
data NodeMode =
    MockNode -- ^ Connect to the PAB mock node.
    | AlonzoNode -- ^ Connect to an Alonzo node
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
    | NoRandomTxGeneration
    | StartingRandomTx
    | KeepingOldBlocks
    | RemovingOldBlocks
    | StartingPABServer Int
    | ProcessingChainEvent ChainEvent
    | BlockOperation BlockEvent
    | CreatingRandomTransaction
    | TxSendCalledWithoutMock
    deriving (Generic, Show, ToJSON, FromJSON)

instance Pretty PABServerLogMsg where
    pretty = \case
        NoRandomTxGeneration      -> "Not creating random transactions"
        StartingRandomTx          -> "Starting random transaction generation thread"
        KeepingOldBlocks          -> "Not starting block reaper thread (old blocks will be retained in-memory forever"
        RemovingOldBlocks         -> "Starting block reaper thread (old blocks will be removed)"
        StartingPABServer p      -> "Starting PAB Server on port" <+> pretty p
        StartingSlotCoordination initialSlotTime slotLength  ->
            "Starting slot coordination thread."
            <+> "Initial slot time:" <+> pretty (F.iso8601Show initialSlotTime)
            <+> "Slot length:" <+> viaShow slotLength
        ProcessingChainEvent e    -> "Processing chain event" <+> pretty e
        BlockOperation e          -> "Block operation" <+> pretty e
        CreatingRandomTransaction -> "Generating a random transaction"
        TxSendCalledWithoutMock   -> "Cannot send transaction without a mocked environment."

instance ToObject PABServerLogMsg where
    toObject _ = \case
        NoRandomTxGeneration      ->  mkObjectStr "Not creating random transactions" ()
        StartingRandomTx          ->  mkObjectStr "Starting random transaction generation thread" ()
        KeepingOldBlocks          ->  mkObjectStr "Not starting block reaper thread (old blocks will be retained in-memory forever" ()
        RemovingOldBlocks         ->  mkObjectStr "Starting block reaper thread (old blocks will be removed)" ()
        StartingPABServer p      ->  mkObjectStr "Starting PAB Server on port " (Tagged @"port" p)
        StartingSlotCoordination i l  -> mkObjectStr "Starting slot coordination thread" (Tagged @"initial-slot-time" (F.iso8601Show  i), Tagged @"slot-length" l)
        ProcessingChainEvent e    ->  mkObjectStr "Processing chain event" (Tagged @"event" e)
        BlockOperation e          ->  mkObjectStr "Block operation" (Tagged @"event" e)
        CreatingRandomTransaction ->  mkObjectStr "Creating random transaction" ()
        TxSendCalledWithoutMock   ->  mkObjectStr "Cannot send transaction without a mocked environment." ()

data BlockEvent = NewSlot
    | NewTransaction Tx
    deriving (Generic, Show, ToJSON, FromJSON)

instance Pretty BlockEvent where
    pretty = \case
        NewSlot          -> "Adding a new slot"
        NewTransaction t -> "Adding a transaction " <+> pretty (Ledger.txId t)


-- State --------------------------------------------------------------------------------------------------------------

-- | Application State
data AppState =
    AppState
        { _chainState   :: MockNodeServerChainState -- ^ blockchain state
        , _eventHistory :: [LogMessage PABServerLogMsg] -- ^ history of all log messages
        }
    deriving (Show)

makeLenses 'AppState

-- | 'AppState' with an initial transaction that pays some Ada to
--   the wallets.
initialAppState :: MonadIO m => [Wallet] -> m AppState
initialAppState wallets = do
    initialState <- initialChainState (Trace.defaultDistFor wallets)
    pure $ AppState
        { _chainState = initialState
        , _eventHistory = mempty
        }

-- | 'ChainState' with initial values
initialChainState :: MonadIO m => Trace.InitialDistribution -> m MockNodeServerChainState
initialChainState =
    fromEmulatorChainState . view EM.chainState .
    MultiAgent.emulatorStateInitialDist . Map.mapKeys EM.mockWalletPaymentPubKeyHash

-- Effects -------------------------------------------------------------------------------------------------------------

type NodeServerEffects m
     = '[ ChainControlEffect
        , ChainEffect
        , State MockNodeServerChainState
        , LogMsg PABServerLogMsg
        , Reader (Maybe Client.TxSendHandle)
        , State AppState
        , LogMsg PABServerLogMsg
        , m]
