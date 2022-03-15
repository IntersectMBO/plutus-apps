import Cardano.Api
import Plutus.Streaming (ChainSyncEvent, ChainSyncEventWithLedgerState, withChainSyncEventStreamWithLedgerState)
import Streaming.Prelude qualified as S

main :: IO ()
main = do
  r <-
    withChainSyncEventStreamWithLedgerState
      "/home/andrea/work/cardano-mainnet/config/mainnet-config.json"
      "/home/andrea/work/cardano-mainnet/socket/node.socket"
      Mainnet
      ChainPointAtGenesis
      $ S.print . S.map f
  print r

f :: ChainSyncEventWithLedgerState -> ChainSyncEvent ChainPoint
f = fmap g

g :: (BlockInMode CardanoMode, Either LedgerStateError (LedgerState, [LedgerEvent])) -> ChainPoint
g (BlockInMode (Block (BlockHeader sn ha _bn) _eim) _ct, _) = ChainPoint sn ha
