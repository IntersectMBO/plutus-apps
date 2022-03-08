module Plutus.Streaming.LedgerState
  ( ledgerState,
    LedgerState (..),
    LedgerEvent (..),
  )
where

import Cardano.Api
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Plutus.Streaming
import Streaming
import Streaming.Prelude qualified as S
import Unsafe.Coerce (unsafeCoerce)

data LedgerStateEvents = LedgerStateEvents LedgerState [LedgerEvent]

applyBlock' ::
  Env ->
  LedgerState ->
  ValidationMode ->
  Block era ->
  Either LedgerStateError LedgerStateEvents
applyBlock' = unsafeCoerce applyBlock

type History a = Seq (SlotNo, a)

-- | This function works under the assumption that the stream of blocks it
-- receives is valid. The function will trigger an exception if
-- 1. a block it receives does not apply on top of the ledger state
-- 2. a rollback goes past the security parameter
-- FIXME, for the moment I kept this function pure but it requires us to do
-- some up-front IO to obtain the initial ledger state from the network
-- config file.
ledgerState ::
  forall m r.
  Monad m =>
  Env ->
  LedgerState ->
  ValidationMode ->
  Stream (Of SimpleChainSyncEvent) m r ->
  Stream (Of (LedgerState, [LedgerEvent])) m r
ledgerState env ls0 vm =
  S.scan step initialHistory projection
  where
    step ::
      (History LedgerState, [LedgerEvent]) ->
      SimpleChainSyncEvent ->
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
      case applyBlock' env ls vm blk of
        Left e ->
          error $ "applyBlock failed " <> show e
        Right (LedgerStateEvents ls' lse) ->
          let history' = fst $ Seq.splitAt (fromIntegral $ envSecurityParam env + 1) ((sn, ls') :<| history)
           in (history', lse)
    unsafePushBlock Seq.Empty _ = error "Impossible! History should never be empty"

    projection :: (History LedgerState, [LedgerEvent]) -> (LedgerState, [LedgerEvent])
    projection ((_, ls) :<| _, lse) = (ls, lse)
    projection (Seq.Empty, _)       = error "Impossible! History should never be empty"
