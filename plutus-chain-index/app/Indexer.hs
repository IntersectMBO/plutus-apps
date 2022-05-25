{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}

module Main where

import Cardano.Api (NetworkId(Mainnet), BlockInMode(BlockInMode), Block(Block), CardanoMode, ChainTip, ChainPoint)
import Cardano.Api qualified as C
import Cardano.BM.Trace (nullTracer)
import Data.Maybe (catMaybes)
import Data.Map (assocs)
import Ledger.TimeSlot (SlotConfig(..))
import Cardano.Protocol.Socket.Client (ChainSyncEvent(Resume, RollForward, RollBackward), runChainSync)
import Cardano.Index.Datum (DatumIndex)
import Cardano.Index.Datum qualified as Ix
import Plutus.Script.Utils.V1.Scripts (Datum, DatumHash)
import Plutus.Contract.CardanoAPI (fromCardanoTx)
import Plutus.ChainIndex.Tx (ChainIndexTx(..))

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

getDatums :: BlockInMode CardanoMode -> [(DatumHash, Datum)]
getDatums (BlockInMode (Block _ txs) era) =
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
       -> [(DatumHash, Datum)]
    go era' tx =
      concat $ assocs . _citxData <$> catMaybes [either (const Nothing) Just (fromCardanoTx era' tx)]


processBlock :: DatumIndex -> ChainSyncEvent -> IO ()
processBlock ix = \case
  -- Not supported
  Resume _         -> pure ()
  RollForward blk _  -> Ix.insert ix $ getDatums blk
  -- Ignore this, for now.
  RollBackward _ _ -> pure ()

main :: IO ()
main = do
  ix <- Ix.open "datum.sqlite" 2000
  _ <- runChainSync "/tmp/node.sock"
                    nullTracer
                    slotConfig
                    networkId
                    []
                    (processBlock ix)
  pure ()
