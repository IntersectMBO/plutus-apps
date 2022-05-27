{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TupleSections  #-}

module Main where

import Cardano.Api (Block (Block), BlockHeader (BlockHeader), BlockInMode (BlockInMode), BlockNo (BlockNo), CardanoMode,
                    ChainPoint, NetworkId (Mainnet), SlotNo, chainPointToSlotNo)
import Cardano.Api qualified as C
import Cardano.BM.Trace (nullTracer)
import Cardano.Index.Datum (DatumIndex)
import Cardano.Index.Datum qualified as Ix
import Cardano.Protocol.Socket.Client (ChainSyncEvent (Resume, RollBackward, RollForward), runChainSync)
import Control.Concurrent (threadDelay)
import Control.Lens.Operators ((^.))
import Control.Monad (forever, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (findIndex)
import Data.Map (assocs)
import Data.Maybe (fromMaybe)
import Index.VSplit qualified as Ix
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
      in  map (slotNo,) hashes

processBlock :: IORef DatumIndex -> ChainSyncEvent -> IO ()
processBlock ixref = \case
  -- Not supported
  Resume point         -> putStrLn ("resume " <> show point) >> pure ()
  RollForward blk@(BlockInMode (Block (BlockHeader slotNo _ blockNo@(BlockNo b)) _txs) _era) _tip -> do
    when (b `rem` 1000 == 0) $
      putStrLn $ show slotNo <> " / " <> show blockNo
    ix     <- readIORef ixref
    nextIx <- Ix.insert (getDatums blk) ix
    writeIORef ixref nextIx
  RollBackward point _tip -> rollbackToPoint point ixref

rollbackToPoint
  :: ChainPoint -> IORef DatumIndex -> IO ()
rollbackToPoint point ixref = do
  ix     <- readIORef ixref
  events <- Ix.getEvents (ix ^. Ix.storage)
  let ix' = fromMaybe ix $ rollbackOffset events ix
  writeIORef ixref ix'
  where
    rollbackOffset :: [Ix.Event] -> DatumIndex -> Maybe DatumIndex
    rollbackOffset events ix = do
      slot   <- chainPointToSlotNo point
      offset <- findIndex (any (\(s, _) -> s < slot)) events
      Ix.rewind offset ix

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
