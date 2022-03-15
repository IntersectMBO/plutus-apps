{-# OPTIONS_GHC -Wno-orphans #-}

import Cardano.Api
import Control.Monad.Except (runExceptT)
import Plutus.Streaming (ChainSyncEvent (..), EventStreamResult, SimpleChainSyncEvent, withSimpleChainSyncEventStream)
import Plutus.Streaming.LedgerState (ledgerState')
import Streaming.Prelude qualified as S

deriving instance Show InitialLedgerStateError

deriving instance Show GenesisConfigError

main :: IO ()
main = do
  ils <- runExceptT (initialLedgerState "/home/andrea/work/cardano-mainnet/config/mainnet-config.json")
  case ils of
    (Left e) -> error $ show e
    (Right (env, ls)) -> do
      r <-
        withSimpleChainSyncEventStream
          "/home/andrea/work/cardano-mainnet/socket/node.socket"
          Mainnet
          ChainPointAtGenesis $
          consume . ledgerState' env ls FullValidation
      print r

consume ::
  S.Stream (S.Of (SimpleChainSyncEvent, (LedgerState, [LedgerEvent]))) IO EventStreamResult ->
  IO EventStreamResult
consume = S.mapM_ g

g :: (SimpleChainSyncEvent, (LedgerState, [LedgerEvent])) -> IO ()
g (RollForward (BlockInMode blk _eim) _, _) = print cp
  where
    (Block (BlockHeader sn ha _bn) _) = blk
    cp = ChainPoint sn ha
g (rb@(RollBackward _ _), _) = print rb
