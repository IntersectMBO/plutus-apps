{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Cardano.Streaming.Helpers where

import Control.Monad.Primitive (PrimState)
import Control.Monad.Trans.Class (lift)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Streaming.Prelude qualified as S

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as CS
import Cardano.Chain.Genesis qualified
import Cardano.Crypto (ProtocolMagicId (unProtocolMagicId), RequiresNetworkMagic (RequiresMagic, RequiresNoMagic))
import Cardano.Ledger.Shelley.LedgerState qualified as SL
import Cardano.Slotting.Slot (WithOrigin (At, Origin))
import Control.Concurrent.Async qualified as IO
import Control.Exception qualified as IO
import Data.SOP.Strict (NP ((:*)))
import GHC.Generics (Generic)
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.Cardano.CanHardFork qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras qualified as HFC
import Ouroboros.Consensus.HardFork.Combinator.Basics qualified as HFC
import Ouroboros.Consensus.Shelley.Ledger qualified as O

-- * ChainSyncEvent

data ChainSyncEvent a
  = RollForward a C.ChainTip
  | RollBackward C.ChainPoint C.ChainTip
  deriving (Show, Functor, Generic)

data ChainSyncEventException
  = NoIntersectionFound
  deriving (Show)

instance IO.Exception ChainSyncEventException

data RollbackException = RollbackLocationNotFound C.ChainPoint C.ChainTip
  deriving (Eq, Show)
instance IO.Exception RollbackException

-- * Orphans

instance IO.Exception C.LedgerStateError

instance IO.Exception C.FoldBlocksError
deriving instance Show C.FoldBlocksError

instance IO.Exception C.InitialLedgerStateError
deriving instance Show C.InitialLedgerStateError
deriving instance Show CS.GenesisConfigError

-- * Block

bimBlockNo :: C.BlockInMode C.CardanoMode -> C.BlockNo
bimBlockNo (C.BlockInMode (C.Block (C.BlockHeader _ _ blockNo) _) _) = blockNo

bimSlotNo :: C.BlockInMode C.CardanoMode -> C.SlotNo
bimSlotNo (C.BlockInMode (C.Block (C.BlockHeader slotNo _ _) _) _) = slotNo

getEpochNo :: C.LedgerState -> Maybe CS.EpochNo
getEpochNo ledgerState' = case ledgerState' of
  C.LedgerStateByron _st                   -> Nothing
  C.LedgerStateShelley st                  -> fromState st
  C.LedgerStateAllegra st                  -> fromState st
  C.LedgerStateMary st                     -> fromState st
  C.LedgerStateAlonzo st                   -> fromState st
  CS.LedgerState (O.LedgerStateBabbage st) -> fromState st -- TODO pattern missing from cardano-node: is it there on master? if not create PR.
  where
    fromState = Just . SL.nesEL . O.shelleyLedgerState

fromChainTip :: C.ChainTip -> WithOrigin C.BlockNo
fromChainTip ct = case ct of
  C.ChainTipAtGenesis -> Origin
  C.ChainTip _ _ bno  -> At bno

-- * IO

linkedAsync :: IO a -> IO ()
linkedAsync action = IO.link =<< IO.async action

-- * LocalNodeConnectInfo

mkLocalNodeConnectInfo :: C.NetworkId -> FilePath -> C.LocalNodeConnectInfo C.CardanoMode
mkLocalNodeConnectInfo networkId socketPath = C.LocalNodeConnectInfo
  { C.localConsensusModeParams = C.CardanoModeParams epochSlots
  , C.localNodeNetworkId = networkId
  , C.localNodeSocketPath = socketPath
  }
  -- This a parameter needed only for the Byron era. Since the Byron
  -- era is over and the parameter has never changed it is ok to
  -- hardcode this. See comment on `Cardano.Api.ConsensusModeParams` in
  -- cardano-node.
  where epochSlots = C.EpochSlots 21600 -- TODO: is this configurable? see below

-- | Derive LocalNodeConnectInfo from Env.
mkConnectInfo :: C.Env -> FilePath -> C.LocalNodeConnectInfo C.CardanoMode
mkConnectInfo env socketPath = C.LocalNodeConnectInfo
  { C.localConsensusModeParams = cardanoModeParams
  , C.localNodeNetworkId       = networkId'
  , C.localNodeSocketPath      = socketPath
  }
  where
    -- Derive the NetworkId as described in network-magic.md from the
    -- cardano-ledger-specs repo.
    byronConfig
      = (\(Consensus.WrapPartialLedgerConfig (Consensus.ByronPartialLedgerConfig bc _) :* _) -> bc)
      . HFC.getPerEraLedgerConfig
      . HFC.hardForkLedgerConfigPerEra
      $ C.envLedgerConfig env

    networkMagic
      = C.NetworkMagic
      $ unProtocolMagicId
      $ Cardano.Chain.Genesis.gdProtocolMagicId
      $ Cardano.Chain.Genesis.configGenesisData byronConfig

    networkId' = case Cardano.Chain.Genesis.configReqNetMagic byronConfig of
      RequiresNoMagic -> C.Mainnet
      RequiresMagic   -> C.Testnet networkMagic

    cardanoModeParams = C.CardanoModeParams . C.EpochSlots $ 10 * C.envSecurityParam env

-- | Ring buffer that handles rollbacks, throws if point of rollback
-- is not found.
rollbackRingBuffer :: forall a r . Int -> S.Stream (S.Of (ChainSyncEvent a)) IO r -> S.Stream (S.Of a) IO r
rollbackRingBuffer bufferSize chainSyncEvents = do
  vector :: VG.Mutable V.Vector (PrimState IO) (a, C.ChainTip) <- lift (VGM.new bufferSize)

  let
    -- Fill phase, don't yield anything. Consume ChainSyncEvents and
    -- roll back to an earlier location in the vector in case of
    -- rollback.
    fill :: Int -> Int -> S.Stream (S.Of (ChainSyncEvent a)) IO r -> S.Stream (S.Of a) IO r
    fill i j source = lift (S.next source) >>= \case
      Left r -> pure r
      Right (chainSyncEvent, source') -> case chainSyncEvent of
        RollForward a ct -> do
          lift $ VGM.unsafeWrite vector i (a, ct)
          let i' = (i + 1) `rem` bufferSize
              j' = j + 1
          if j' == bufferSize
            then fillYield i' source'
            else fill i' j' source'
        RollBackward cp ct -> rewind (cp, ct) i j source'

    -- Fill & yield phase. Buffer is full in the beginning, but will
    -- need to be refilled when a rollback occurs.
    fillYield :: Int -> S.Stream (S.Of (ChainSyncEvent a)) IO r -> S.Stream (S.Of a) IO r
    fillYield i source = lift (S.next source) >>= \case
      Left r -> pure r
      Right (chainSyncEvent, source') -> case chainSyncEvent of
        RollForward a ct -> do
          (a', _) <- lift $ VGM.exchange vector i (a, ct)
          S.yield a'
          fillYield (i `rem` bufferSize) source'
        RollBackward cp ct -> rewind (cp, ct) i bufferSize source'

    rewind :: (C.ChainPoint, C.ChainTip) -> Int -> Int -> S.Stream (S.Of (ChainSyncEvent a)) IO r -> S.Stream (S.Of a) IO r
    rewind (cp, ct) i j source' = case cp of
      C.ChainPointAtGenesis -> fill 0 0 source'
      _ -> do
        maybeIndex <- lift $ VG.findIndex ((ct ==) . snd) <$> VG.unsafeFreeze vector
        case calculateRewind maybeIndex bufferSize i j of
          Just (i', j') -> fill i' j' source'
          _             -> lift $ IO.throwIO $ RollbackLocationNotFound cp ct

  if | bufferSize > 0 -> fill 0 0 chainSyncEvents
     | otherwise      -> ignoreRollbacks chainSyncEvents

calculateRewind :: Maybe Int -> Int -> Int -> Int -> Maybe (Int, Int)
calculateRewind maybeIndex vectorSize i j = case maybeIndex of
  Just lastValidI -> let
    i' = lastValidI + 1
    j' = j - (if i > i' then i - i' else i + (j - i'))
    in Just (i' `rem` vectorSize, j')
  _ -> Nothing


-- | Ignore rollback events in the chainsync event stream. Useful for
-- monitor which blocks has been seen by the node, regardless whether
-- they are permanent.
ignoreRollbacks :: Monad m => S.Stream (S.Of (ChainSyncEvent a)) m r -> S.Stream (S.Of a) m r
ignoreRollbacks = S.mapMaybe (\case RollForward e _ -> Just e; _ -> Nothing)
