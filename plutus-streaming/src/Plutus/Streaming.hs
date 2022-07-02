module Plutus.Streaming
  ( withChainSyncEventStream,
    ledgerStateEvents,
    ChainSyncEvent (..),
    ChainSyncEventException (..),
    ApplyBlockException (..),
  )
where

import Cardano.Api (Block (Block), BlockHeader (BlockHeader), BlockInMode (BlockInMode), CardanoMode,
                    ChainPoint (ChainPoint, ChainPointAtGenesis), ChainSyncClient (ChainSyncClient), ChainTip,
                    ConsensusModeParams (CardanoModeParams), Env, EpochSlots (EpochSlots), LedgerEvent, LedgerState,
                    LedgerStateError, LocalChainSyncClient (LocalChainSyncClient),
                    LocalNodeClientProtocols (LocalNodeClientProtocols, localChainSyncClient, localStateQueryClient, localTxMonitoringClient, localTxSubmissionClient),
                    LocalNodeConnectInfo (LocalNodeConnectInfo, localConsensusModeParams, localNodeNetworkId, localNodeSocketPath),
                    NetworkId, SlotNo, ValidationMode, applyBlock, connectToLocalNode, envSecurityParam,
                    renderLedgerStateError)
import Cardano.Api.ChainSync.Client (ClientStIdle (SendMsgFindIntersect, SendMsgRequestNext),
                                     ClientStIntersect (ClientStIntersect, recvMsgIntersectFound, recvMsgIntersectNotFound),
                                     ClientStNext (ClientStNext, recvMsgRollBackward, recvMsgRollForward))
import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.STM (TChan, atomically, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Exception (Exception, throw)
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S

data ChainSyncEvent a
  = RollForward a ChainTip
  | RollBackward ChainPoint ChainTip
  deriving (Show, Functor, Generic)

data ChainSyncEventException
  = NoIntersectionFound
  deriving (Show)

instance Exception ChainSyncEventException

newtype ApplyBlockException = ApplyBlockException LedgerStateError

instance Show ApplyBlockException where
  show (ApplyBlockException e) = Text.unpack (renderLedgerStateError e)

instance Exception ApplyBlockException

type History a = Seq (SlotNo, a)

-- | `withChainSyncEventStream` uses the chain-sync mini-protocol to
-- connect to a locally running node and fetch blocks from the given
-- starting point.
withChainSyncEventStream ::
  -- | Path to the node socket
  FilePath ->
  NetworkId ->
  -- | The point on the chain to start streaming from
  ChainPoint ->
  -- | Stream consumer
  (Stream (Of (ChainSyncEvent (BlockInMode CardanoMode))) IO r -> IO b) ->
  IO b
withChainSyncEventStream socketPath networkId point consumer = do
  -- The chain-sync client runs in a different thread and it will send us
  -- block through this channel.

  -- By using newBroadcastTChan, messages can be garbage collected after
  -- clients have seen them, preventing pile up. The only way to read a
  -- broadcast channel is to duplicate it with dupTChan. See note at
  -- `newBroadcastTChan`.
  chan <- newBroadcastTChanIO
  readerChannel <- atomically $ dupTChan chan

  let client = chainSyncStreamingClient point chan

      localNodeClientProtocols =
        LocalNodeClientProtocols
          { localChainSyncClient = LocalChainSyncClient client,
            localTxSubmissionClient = Nothing,
            localStateQueryClient = Nothing,
            localTxMonitoringClient = Nothing
          }

      connectInfo =
        LocalNodeConnectInfo
          { localConsensusModeParams = CardanoModeParams epochSlots,
            localNodeNetworkId = networkId,
            localNodeSocketPath = socketPath
          }

      -- FIXME this comes from the config file but Cardano.Api does not expose readNetworkConfig!
      epochSlots = EpochSlots 21600

      clientThread = do
        connectToLocalNode connectInfo localNodeClientProtocols
        -- the only reason connectToLocalNode can terminate successfully is if it
        -- doesn't find an intersection, we report that case to the
        -- consumer as an exception
        throw NoIntersectionFound

  withAsync clientThread $ \a -> do
    -- Make sure all exceptions in the client thread are passed to the consumer thread
    link a
    -- Run the consumer
    consumer $ S.repeatM $ atomically (readTChan readerChannel)

-- | `chainSyncStreamingClient` is the client that connects to a local node
-- and runs the chain-sync mini-protocol. This client is fire-and-forget
-- and does not require any control.
--
-- Blocks obtained from the chain-sync mini-protocol are passed to a
-- consumer through a channel.
--
-- If the starting point is such that an intersection cannot be found, this
-- client will throw a NoIntersectionFound exception.
chainSyncStreamingClient ::
  ChainPoint ->
  TChan (ChainSyncEvent e) ->
  ChainSyncClient e ChainPoint ChainTip IO ()
chainSyncStreamingClient point chan =
  ChainSyncClient $ pure $ SendMsgFindIntersect [point] onIntersect
  where
    onIntersect =
      ClientStIntersect
        { recvMsgIntersectFound = \_ _ ->
            ChainSyncClient sendRequestNext,
          recvMsgIntersectNotFound = \_ ->
            ChainSyncClient $
              -- There is nothing we can do here
              throw NoIntersectionFound
        }

    sendRequestNext =
      pure $ SendMsgRequestNext onNext (pure onNext)
      where
        onNext =
          ClientStNext
            { recvMsgRollForward = \bim ct ->
                ChainSyncClient $ do
                  atomically $ writeTChan chan (RollForward bim ct)
                  sendRequestNext,
              recvMsgRollBackward = \cp ct ->
                ChainSyncClient $ do
                  atomically $ writeTChan chan (RollBackward cp ct)
                  sendRequestNext
            }

-- | This function works under the assumption that the stream of blocks it
-- receives is valid. The function will trigger an exception if
-- 1. a block it receives does not apply on top of the ledger state
-- 2. a rollback goes past the security parameter
-- FIXME, for the moment I kept this function pure but it requires us to do
-- some up-front IO to obtain the initial ledger state from the network
-- config file.
ledgerStateEvents ::
  forall m r.
  Monad m =>
  Env ->
  LedgerState ->
  ValidationMode ->
  Stream (Of (ChainSyncEvent (BlockInMode CardanoMode))) m r ->
  Stream (Of (ChainSyncEvent (BlockInMode CardanoMode), (LedgerState, [LedgerEvent]))) m r
ledgerStateEvents env ls0 vm =
  S.scanned step initialHistory projection
  where
    step ::
      (History LedgerState, [LedgerEvent]) ->
      ChainSyncEvent (BlockInMode CardanoMode) ->
      (History LedgerState, [LedgerEvent])
    step (history, _) (RollForward (BlockInMode blk _) _) =
      unsafePushBlock history blk
    step _ (RollBackward ChainPointAtGenesis _) =
      initialHistory
    step (history, _) (RollBackward (ChainPoint sn _) _) =
      unsafeRollback history sn

    initialHistory :: (History LedgerState, [LedgerEvent])
    initialHistory = (Seq.singleton (0, ls0), [])

    -- This function is unsafe because it might result in an empty history,
    -- breaking the assumption of unsafePushBlock and projection
    unsafeRollback :: History LedgerState -> SlotNo -> (History LedgerState, [LedgerEvent])
    unsafeRollback history sn =
      let history' = Seq.dropWhileL ((> sn) . fst) history
       in (history', [])

    -- This function is unsafe because it will assume the given block will
    -- successfully apply on top of the ledger state.
    unsafePushBlock :: History LedgerState -> Block era -> (History LedgerState, [LedgerEvent])
    unsafePushBlock history@((_, ls) :<| _) blk@(Block (BlockHeader sn _ _) _) =
      case applyBlock env ls vm blk of
        Left e ->
          throw $ ApplyBlockException e
        Right (ls', lse) ->
          let history' = fst $ Seq.splitAt (fromIntegral $ envSecurityParam env + 1) ((sn, ls') :<| history)
           in (history', lse)
    unsafePushBlock Seq.Empty _ = error "Impossible! History should never be empty"

    projection :: (History LedgerState, [LedgerEvent]) -> (LedgerState, [LedgerEvent])
    projection ((_, ls) :<| _, lse) = (ls, lse)
    projection (Seq.Empty, _)       = error "Impossible! History should never be empty"
