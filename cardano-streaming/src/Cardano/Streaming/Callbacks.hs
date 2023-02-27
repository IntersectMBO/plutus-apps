{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Cardano.Streaming.Callbacks where

import Control.Exception (throw)
import Data.Word (Word32)

import Cardano.Api qualified as C
import Cardano.Slotting.Slot (WithOrigin (At, Origin))
import Network.TypedProtocol.Pipelined (N (Z), Nat (Succ, Zero))
import Ouroboros.Network.Protocol.ChainSync.Client qualified as CS
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined qualified as CSP
import Ouroboros.Network.Protocol.ChainSync.PipelineDecision (PipelineDecision (Collect), pipelineDecisionMax)

import Cardano.Streaming.Helpers qualified as H
import Ouroboros.Consensus.Util.ResourceRegistry qualified as Util
import Ouroboros.Consensus.Util.Args qualified as Util
import Ouroboros.Consensus.Storage.ImmutableDB qualified as ImmutableDB
import Ouroboros.Consensus.Storage.ChainDB qualified as ChainDB
import Ouroboros.Consensus.Storage.FS.API qualified as FS

-- * Raw chain-sync clients using callback

blocksCallbackPipelined
  :: Word32 -> C.LocalNodeConnectInfo C.CardanoMode -> C.ChainPoint
  -> (H.ChainSyncEvent (C.BlockInMode C.CardanoMode) -> IO ())
  -> IO ()
blocksCallbackPipelined n con point callback =
  C.connectToLocalNode con $ C.LocalNodeClientProtocols
    { C.localChainSyncClient    = C.LocalChainSyncClientPipelined $ CSP.ChainSyncClientPipelined $ pure $ CSP.SendMsgFindIntersect [point] onIntersect
    , C.localTxSubmissionClient = Nothing
    , C.localStateQueryClient   = Nothing
    , C.localTxMonitoringClient = Nothing
    }
  where
    onIntersect =
      CSP.ClientPipelinedStIntersect
        { CSP.recvMsgIntersectFound = \_ _ -> pure $ work n
        , CSP.recvMsgIntersectNotFound = throw H.NoIntersectionFound
        }

    work :: Word32 -> CSP.ClientPipelinedStIdle 'Z (C.BlockInMode C.CardanoMode) C.ChainPoint C.ChainTip IO ()
    work pipelineSize = requestMore Origin Origin Zero
      where
          requestMore -- was clientIdle_RequestMoreN
            :: WithOrigin C.BlockNo -> WithOrigin C.BlockNo -> Nat n
            -> CSP.ClientPipelinedStIdle n (C.BlockInMode C.CardanoMode) C.ChainPoint C.ChainTip IO ()
          requestMore clientTip serverTip rqsInFlight = let
            in case pipelineDecisionMax pipelineSize rqsInFlight clientTip serverTip of
                -- handle a response
                Collect -> case rqsInFlight of
                  Succ predN -> CSP.CollectResponse Nothing (clientNextN predN)
                -- fire more requests
                _ -> CSP.SendMsgRequestNextPipelined (requestMore clientTip serverTip (Succ rqsInFlight))

          clientNextN
            :: Nat n
            -> CSP.ClientStNext n (C.BlockInMode C.CardanoMode) C.ChainPoint C.ChainTip IO ()
          clientNextN rqsInFlight = CSP.ClientStNext
            { CSP.recvMsgRollForward = \bim ct -> do
                callback $ H.RollForward bim ct
                return $ requestMore (At $ H.bimBlockNo bim) (H.fromChainTip ct) rqsInFlight
            , CSP.recvMsgRollBackward = \cp ct -> do
                callback $ H.RollBackward cp ct
                return $ requestMore Origin (H.fromChainTip ct) rqsInFlight
            }

blocksCallback
  :: C.LocalNodeConnectInfo C.CardanoMode -> C.ChainPoint
  -> (H.ChainSyncEvent (C.BlockInMode C.CardanoMode) -> IO ())
  -> IO ()
blocksCallback con point callback =
  C.connectToLocalNode con $ C.LocalNodeClientProtocols
    { C.localChainSyncClient    = C.LocalChainSyncClient $ CS.ChainSyncClient $ pure $ CS.SendMsgFindIntersect [point] onIntersect
    , C.localTxSubmissionClient = Nothing
    , C.localStateQueryClient   = Nothing
    , C.localTxMonitoringClient = Nothing
    }
  where
    onIntersect =
      CS.ClientStIntersect
        { CS.recvMsgIntersectFound = \_ _ -> CS.ChainSyncClient sendRequestNext
        , CS.recvMsgIntersectNotFound = throw H.NoIntersectionFound
        }
    sendRequestNext = pure $ CS.SendMsgRequestNext onNext (pure onNext)
      where
        onNext = CS.ClientStNext
            { CS.recvMsgRollForward = \bim ct -> CS.ChainSyncClient $ do
                callback $ H.RollForward bim ct
                sendRequestNext
            , CS.recvMsgRollBackward = \cp ct -> CS.ChainSyncClient $ do
                callback $ H.RollBackward cp ct
                sendRequestNext
            }

blocksFromChainDbCallback :: C.ChainPoint -> (C.BlockInMode C.CardanoMode -> IO ()) -> IO ()
blocksFromChainDbCallback _point _callback = undefined

run :: IO ()
run = do
 let
   args = ImmutableDB.defaultArgs :: FS.SomeHasFS IO -> ImmutableDB.ImmutableDbArgs Util.Defaults IO (C.BlockInMode C.CardanoMode)
   args_ = args $ mkFS $ ChainDB.RelativeMountPoint "immutable"
--   _ = ImmutableDB.openDB (args u)
   open :: IO (ImmutableDB.ImmutableDB IO (C.BlockInMode C.CardanoMode))
   open = u
   action :: ImmutableDB.ImmutableDB IO (C.BlockInMode C.CardanoMode) -> IO ()
   action = printBlockHashes
 ImmutableDB.withDB open action

-- | Print the hash of each block in the ImmutableDB
printBlockHashes
  :: ImmutableDB.ImmutableDB IO (C.BlockInMode C.CardanoMode) -> IO ()
printBlockHashes db = do
  -- create an iterator over the ImmutableDB
  eitherIterator <- let
    resourceRegistry = u :: Util.ResourceRegistry m
    blockComponent = u :: ChainDB.BlockComponent (C.BlockInMode C.CardanoMode) b
    streamFrom = u :: ChainDB.StreamFrom (C.BlockInMode C.CardanoMode)
    streamTo = u :: ChainDB.StreamTo (C.BlockInMode C.CardanoMode)
    -- -> m (Either (MissingBlock blk) (Iterator m blk b))
    in ImmutableDB.stream db resourceRegistry blockComponent streamFrom streamTo

  case eitherIterator of
    Left (_e :: ImmutableDB.MissingBlock (C.BlockInMode C.CardanoMode)) -> do
      putStrLn "MissingBlock, exiting"
      return () -- todo
    Right (iterator :: ImmutableDB.Iterator IO (C.BlockInMode C.CardanoMode) b) ->
      let
        loop = do
          r :: ImmutableDB.IteratorResult (C.BlockInMode C.CardanoMode) <- ImmutableDB.iteratorNext iterator
          case r of
            ImmutableDB.IteratorResult (blk :: C.BlockInMode C.CardanoMode) -> print $ H.bimSlotNo blk
            ImmutableDB.IteratorExhausted -> pure ()
        in loop

u :: a
u = undefined
