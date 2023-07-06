{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-orphans           #-}
module Cardano.Protocol.Socket.Type where

import Codec.Serialise.Class (Serialise)
import Control.Monad (forever)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadTimer (MonadDelay (threadDelay), MonadTimer)
import Crypto.Hash (SHA256, hash)
import Data.Aeson.Extras qualified as JSON
import Data.ByteArray qualified as BA
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as BS
import Data.Foldable (toList)
import Data.Map ((!))
import Data.Sequence.Strict (fromList)
import Data.Text qualified as Text
import Data.Time.Units.Extra ()
import Data.Void (Void)

import GHC.Generics
import NoThunks.Class (NoThunks (noThunks, showTypeOf, wNoThunks))

import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Ledger.Block qualified as CL
import Cardano.Ledger.Era qualified as CL
import Cardano.Ledger.Shelley.API (extractTx, unsafeMakeValidated)
import Codec.Serialise (DeserialiseFailure)
import Codec.Serialise qualified as CBOR
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

import PlutusTx.Builtins qualified as PlutusTx
import Prettyprinter.Extras (Pretty, PrettyShow (PrettyShow))

import Ledger (Block, OnChainTx (..), TxId (..))
import Ledger.Test (testNetworkMagic)

type Tip = Ouroboros.Tip (CardanoBlock StandardCrypto)

-- | The node protocols require a block header type.
newtype BlockId = BlockId { getBlockId :: BS.ShortByteString }
  deriving (Eq, Ord, Generic)
  deriving newtype (Serialise, NoThunks)
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

instance NoThunks PlutusTx.BuiltinByteString where
  noThunks ctx b = noThunks ctx (PlutusTx.fromBuiltin b)
  wNoThunks ctx b = wNoThunks ctx (PlutusTx.fromBuiltin b)
  showTypeOf _ = showTypeOf (Proxy @BS.ShortByteString)

deriving newtype instance NoThunks TxId

-- | Limits for the protocols we use.
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits {
        maximumIngressQueue = maxBound
    }

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

type Offset = Integer

-- | Boilerplate codecs used for protocol serialisation.

-- | The number of epochSlots is specific to each blockchain instance. This value
-- is what the cardano main and testnet uses. Only applies to the Byron era.
epochSlots :: EpochSlots
epochSlots = EpochSlots 21600

codecVersion :: BlockNodeToClientVersion (CardanoBlock StandardCrypto)
codecVersion = versionMap ! nodeToClientVersion
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

