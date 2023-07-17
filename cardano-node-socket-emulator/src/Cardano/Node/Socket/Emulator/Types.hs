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
import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Ledger.Block qualified as CL
import Cardano.Ledger.Era qualified as CL
import Cardano.Ledger.Shelley.API (extractTx, unsafeMakeValidated)
import Cardano.Node.Emulator.Internal.Node (ChainControlEffect, ChainEffect, ChainEvent, SlotConfig, fromBlockchain,
                                            testnet, unsafeMakeValid)
import Cardano.Node.Emulator.Internal.Node.Chain qualified as EC
import Codec.Serialise (DeserialiseFailure)
import Codec.Serialise qualified as CBOR
import Control.Concurrent.STM
import Control.Lens (makeLenses)
import Control.Monad (forever)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadTimer (MonadDelay (threadDelay), MonadTimer)
import Control.Monad.Freer.Extras.Log (LogMessage, LogMsg)
import Control.Monad.Freer.State qualified as Eff
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash (SHA256, hash)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Extras qualified as JSON
import Data.ByteArray qualified as BA
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as BS
import Data.Default (Default, def)
import Data.Foldable (toList)
import Data.Functor (void)
import Data.Map qualified as Map
import Data.Sequence.Strict (fromList)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 qualified as F
import Data.Time.Units (Millisecond)
import Data.Time.Units.Extra ()
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger (Block, CardanoTx, OnChainTx (..), Slot (..))
import Ledger.Address (CardanoAddress)
import Ledger.CardanoWallet
import Ledger.Index (UtxoIndex, createGenesisTransaction)
import Ledger.Test (testNetworkMagic)
import Network.TypedProtocol.Codec (Codec)
import Ouroboros.Consensus.Byron.Ledger qualified as Byron
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, CodecConfig (..))
import Ouroboros.Consensus.Cardano.Block qualified as OC
import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import Ouroboros.Consensus.Network.NodeToClient (ClientCodecs, cChainSyncCodec, cTxSubmissionCodec, clientCodecs)
import Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToClientVersion, supportedNodeToClientVersions)
import Ouroboros.Consensus.Protocol.Praos.Header qualified as Praos
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import Ouroboros.Consensus.Shelley.Ledger qualified as Shelley
import Ouroboros.Network.Block (Point)
import Ouroboros.Network.Block qualified as Ouroboros
import Ouroboros.Network.Mux
import Ouroboros.Network.NodeToClient (NodeToClientVersion (..), NodeToClientVersionData (..))
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync
import Ouroboros.Network.Protocol.LocalTxSubmission.Type qualified as TxSubmission
import Ouroboros.Network.Util.ShowProxy
import Prettyprinter (Pretty, pretty, viaShow, vsep, (<+>))
import Prettyprinter.Extras (PrettyShow (PrettyShow))
import Servant.Client (BaseUrl (BaseUrl, baseUrlPort), Scheme (Http))

type Tip = Ouroboros.Tip (CardanoBlock StandardCrypto)

type TxPool = [CardanoTx]

data MockNodeServerChainState = MockNodeServerChainState
  { _txPool      :: TxPool
  , _index       :: UtxoIndex
  , _currentSlot :: Slot
  , _channel     :: TChan Block
  , _tip         :: Tip
  } deriving (Generic)

makeLenses ''MockNodeServerChainState

instance Show MockNodeServerChainState where
    -- Skip showing the full chain
    show MockNodeServerChainState {_txPool, _index, _currentSlot, _tip} =
        "MockNodeServerChainState { " <> show _txPool
                        <> ", " <> show _index
                        <> ", " <> show _currentSlot
                        <> ", " <> show _tip <> " }"

-- | Build a CNSE ChainState from a emulator ChainState
fromEmulatorChainState :: MonadIO m => EC.ChainState -> m MockNodeServerChainState
fromEmulatorChainState EC.ChainState {EC._txPool, EC._index, EC._chainCurrentSlot, EC._chainNewestFirst} = do
    ch <- liftIO $ atomically newTChan
    void $ liftIO $
        mapM_ (atomically . writeTChan ch) _chainNewestFirst
    pure $ MockNodeServerChainState { _channel     = ch
                      , _txPool      = _txPool
                      , _index       = _index
                      , _currentSlot = _chainCurrentSlot
                      , _tip         = Ouroboros.TipGenesis
                      }

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

-- | The node protocols require a block header type.
newtype BlockId = BlockId { getBlockId :: BS.ShortByteString }
  deriving (Eq, Ord, Generic)
  deriving newtype (CBOR.Serialise)
  deriving Pretty via (PrettyShow BlockId)

instance Show BlockId where
    show = Text.unpack . JSON.encodeByteString . BS.fromShort . getBlockId

-- | A hash of the block's contents.
blockId :: Block -> BlockId
blockId = BlockId
        . BS.toShort
        . BA.convert
        . hash @_ @SHA256
        . BSL.toStrict
        . CBOR.serialise

-- | Protocol versions
nodeToClientVersion :: NodeToClientVersion
nodeToClientVersion = NodeToClientV_13

-- | A temporary definition of the protocol version. This will be moved as an
-- argument to the client connection function in a future PR (the network magic
-- number matches the one in the test net created by scripts)
nodeToClientVersionData :: NodeToClientVersionData
nodeToClientVersionData = NodeToClientVersionData { networkMagic = testNetworkMagic }

-- | A protocol client that will never leave the initial state.
doNothingInitiatorProtocol
  :: MonadTimer m => RunMiniProtocol 'InitiatorMode BSL.ByteString m a Void
doNothingInitiatorProtocol =
    InitiatorProtocolOnly $ MuxPeerRaw $
    const $ forever $ threadDelay 1e6

doNothingResponderProtocol
  :: MonadTimer m => RunMiniProtocol 'ResponderMode BSL.ByteString m Void a
doNothingResponderProtocol =
  ResponderProtocolOnly $ MuxPeerRaw $
  const $ forever $ threadDelay 1e6

-- | Boilerplate codecs used for protocol serialisation.

-- | The number of epochSlots is specific to each blockchain instance. This value
-- is what the cardano main and testnet uses. Only applies to the Byron era.
epochSlots :: EpochSlots
epochSlots = EpochSlots 21600

codecVersion :: BlockNodeToClientVersion (CardanoBlock StandardCrypto)
codecVersion = versionMap Map.! nodeToClientVersion
  where
    versionMap =
      supportedNodeToClientVersions
        (Proxy @(CardanoBlock StandardCrypto))

codecConfig :: CodecConfig (CardanoBlock StandardCrypto)
codecConfig =
  CardanoCodecConfig
    (Byron.ByronCodecConfig epochSlots)
    Shelley.ShelleyCodecConfig
    Shelley.ShelleyCodecConfig
    Shelley.ShelleyCodecConfig
    Shelley.ShelleyCodecConfig
    Shelley.ShelleyCodecConfig

nodeToClientCodecs
  :: forall m. MonadST m
  => ClientCodecs (CardanoBlock StandardCrypto) m
nodeToClientCodecs =
  clientCodecs codecConfig codecVersion nodeToClientVersion

-- | These codecs are currently used in the mock nodes and will
--   probably soon get removed as the mock nodes are phased out.
chainSyncCodec
  :: (block ~ CardanoBlock StandardCrypto)
  => Codec (ChainSync.ChainSync block (Point block) Tip)
           DeserialiseFailure
           IO BSL.ByteString
chainSyncCodec = cChainSyncCodec nodeToClientCodecs

txSubmissionCodec
  :: (block ~ CardanoBlock StandardCrypto)
  => Codec (TxSubmission.LocalTxSubmission (Shelley.GenTx block) (ApplyTxErr block))
           DeserialiseFailure IO BSL.ByteString
txSubmissionCodec = cTxSubmissionCodec nodeToClientCodecs

toCardanoBlock :: Praos.Header StandardCrypto -> Block -> CardanoBlock StandardCrypto
toCardanoBlock header block = OC.BlockBabbage (Shelley.mkShelleyBlock $ CL.Block header $ CL.toTxSeq $ fromList $ extractTx . getOnChainTx <$> block)

fromCardanoBlock :: CardanoBlock StandardCrypto -> Block
fromCardanoBlock (OC.BlockBabbage (Shelley.ShelleyBlock (CL.Block _ txSeq) _)) = map (OnChainTx . unsafeMakeValidated) . toList $ CL.fromTxSeq txSeq
fromCardanoBlock _ = []
