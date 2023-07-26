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
import Cardano.Node.Emulator.API (EmulatorLogs, EmulatorMsg, EmulatorState, emptyEmulatorStateWithInitialDist,
                                  esChainState)
import Cardano.Node.Emulator.Internal.Node.Chain qualified as EC
import Cardano.Node.Emulator.Internal.Node.Params (testnet)
import Cardano.Node.Emulator.Internal.Node.TimeSlot (SlotConfig)
import Codec.Serialise (DeserialiseFailure)
import Codec.Serialise qualified as CBOR
import Control.Concurrent (MVar, modifyMVar_, readMVar)
import Control.Concurrent.STM
import Control.Lens (makeLenses, view, (&), (.~), (^.))
import Control.Monad (forever)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadTimer (MonadDelay (threadDelay), MonadTimer)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash (SHA256, hash)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Extras qualified as JSON
import Data.ByteArray qualified as BA
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as BS
import Data.Coerce (coerce)
import Data.Default (Default, def)
import Data.Foldable (toList)
import Data.Functor (void, (<&>))
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Sequence.Strict (fromList)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 qualified as F
import Data.Time.Units (Millisecond)
import Data.Time.Units.Extra ()
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger (Block, CardanoTx, OnChainTx (..))
import Ledger.Address (CardanoAddress)
import Ledger.CardanoWallet
import Ledger.Test (testNetworkMagic)
import Network.TypedProtocol.Codec (Codec)
import Ouroboros.Consensus.Byron.Ledger qualified as Byron
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, CodecConfig (..))
import Ouroboros.Consensus.Cardano.Block qualified as OC
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraHash (..))
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

data SocketEmulatorState = SocketEmulatorState
  { _emulatorState :: EmulatorState
  , _channel       :: TChan Block
  , _tip           :: Tip
  } deriving (Generic)

makeLenses ''SocketEmulatorState

instance Show SocketEmulatorState where
    -- Skip showing the full chain
    show SocketEmulatorState {_emulatorState, _tip} =
        "SocketEmulatorState { " <> show _emulatorState
                        <> ", " <> show _tip <> " }"

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

-- | Application State
data AppState =
    AppState
        { _socketEmulatorState :: SocketEmulatorState -- ^ blockchain state
        , _emulatorLogs        :: EmulatorLogs -- ^ history of all log messages
        }
    deriving (Show)

makeLenses 'AppState

fromEmulatorChainState :: MonadIO m => EmulatorState -> m SocketEmulatorState
fromEmulatorChainState state = do
    ch <- liftIO $ atomically newTChan
    let chainNewestFirst = view (esChainState . EC.chainNewestFirst) state
    let currentSlot = view (esChainState . EC.chainCurrentSlot) state
    void $ liftIO $
        mapM_ (atomically . writeTChan ch) chainNewestFirst
    pure $ SocketEmulatorState
        { _channel       = ch
        , _emulatorState = state
        , _tip           = case listToMaybe chainNewestFirst of
                              Nothing -> Ouroboros.TipGenesis
                              Just block -> Ouroboros.Tip (fromIntegral currentSlot) (coerce $ blockId block) (fromIntegral currentSlot)
        }

-- | 'ChainState' with initial values
initialChainState :: MonadIO m => Map.Map CardanoAddress Value -> m SocketEmulatorState
initialChainState = fromEmulatorChainState . emptyEmulatorStateWithInitialDist

getChannel :: MonadIO m => MVar AppState -> m (TChan Block)
getChannel mv = liftIO (readMVar mv) <&> view (socketEmulatorState . channel)

-- Get the current tip.
getTip :: MonadIO m => MVar AppState -> m Tip
getTip mv = liftIO (readMVar mv) <&> view (socketEmulatorState . tip)

-- Set the new tip
setTip :: MonadIO m => MVar AppState -> Block -> m ()
setTip mv block = liftIO $ modifyMVar_ mv $ \oldState -> do
  let slot = oldState ^. socketEmulatorState . emulatorState . esChainState . EC.chainCurrentSlot
  pure $ oldState & socketEmulatorState . tip .~ Ouroboros.Tip (fromIntegral slot) (coerce $ blockId block) (fromIntegral slot)

-- Logging ------------------------------------------------------------------------------------------------------------

-- | Top-level logging data type for structural logging
-- inside the CNSE server.
data CNSEServerLogMsg =
    StartingSlotCoordination UTCTime Millisecond
    | StartingCNSEServer Int
    | ProcessingEmulatorMsg EmulatorMsg
    deriving (Generic, Show, ToJSON, FromJSON)

instance Pretty CNSEServerLogMsg where
    pretty = \case
        StartingSlotCoordination initialSlotTime slotLength  ->
            "Starting slot coordination thread."
            <+> "Initial slot time:" <+> pretty (F.iso8601Show initialSlotTime)
            <+> "Slot length:" <+> viaShow slotLength
        StartingCNSEServer p   -> "Starting Cardano Node Emulator on port" <+> pretty p
        ProcessingEmulatorMsg e -> "Processing emulator event:" <+> pretty e

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
