{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import Cardano.Api (Block (Block), BlockHeader (BlockHeader), BlockInMode (BlockInMode), CardanoMode,
                    NetworkId (Mainnet), SlotNo)
import Cardano.Api qualified as C
import Cardano.BM.Trace (nullTracer)
import Cardano.Index.Datum (DatumIndex)
import Cardano.Index.Datum qualified as Ix
import Cardano.Protocol.Socket.Client (ChainSyncEvent (Resume, RollBackward, RollForward), runChainSync)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map (assocs)
import Ledger.TimeSlot (SlotConfig (..))
import Plutus.ChainIndex.Tx (ChainIndexTx (..))
import Plutus.Contract.CardanoAPI (fromCardanoTx)
import Plutus.Script.Utils.V1.Scripts (Datum, DatumHash)

-- Connecting to cardano node.

-- We only care about the mainnet
slotConfig :: SlotConfig
slotConfig =
  SlotConfig
    { scSlotZeroTime = 1596059091000
    , scSlotLength   = 1000
    }

networkId :: NetworkId
networkId = Mainnet

getDatums :: BlockInMode CardanoMode -> [(SlotNo, (DatumHash, Datum))]
getDatums (BlockInMode (Block (BlockHeader slotNo _ _) txs) era) =
  case era of
    C.ByronEraInCardanoMode   -> concatMap (go era) txs
    C.ShelleyEraInCardanoMode -> concatMap (go era) txs
    C.AllegraEraInCardanoMode -> concatMap (go era) txs
    C.MaryEraInCardanoMode    -> concatMap (go era) txs
    C.AlonzoEraInCardanoMode  -> concatMap (go era) txs
  where
    go :: C.IsCardanoEra era
       => C.EraInMode era C.CardanoMode
       -> C.Tx era
       -> [(SlotNo, (DatumHash, Datum))]
    go era' tx =
      let hashes = either (const []) (assocs . _citxData) $ fromCardanoTx era' tx
      in  map (\h -> (slotNo, h)) hashes

processBlock :: IORef DatumIndex -> ChainSyncEvent -> IO ()
processBlock ixref = \case
  -- Not supported
  Resume point         -> putStrLn ("resume " <> show point) >> pure ()
  RollForward blk@(BlockInMode (Block (BlockHeader slotNo _ blockNo) _txs) _era) _tip -> do
    putStrLn $ show slotNo <> " / " <> show blockNo
    ix     <- readIORef ixref
    nextIx <- Ix.insert (getDatums blk) ix
    writeIORef ixref nextIx
  -- Ignore this, for now.
  RollBackward point tip -> putStrLn ("rollback " <> show point <> " " <> show tip) >> pure ()

main :: IO ()
main = do
  tix <- Ix.open "datum.sqlite" 2000 >>= newIORef
  _ <- runChainSync "/tmp/node.sock"
                    nullTracer
                    slotConfig
                    networkId
                    []
                    (processBlock tix)
  forever $ threadDelay 1000000000
