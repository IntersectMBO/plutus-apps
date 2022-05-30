{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TupleSections  #-}

module Main where

import Cardano.Api (Block (Block), BlockHeader (BlockHeader), BlockInMode (BlockInMode), BlockNo (BlockNo), CardanoMode,
                    ChainPoint (ChainPoint), Hash, NetworkId (Mainnet), SlotNo (SlotNo), chainPointToSlotNo,
                    deserialiseFromRawBytesHex, proxyToAsType)
import Cardano.Api qualified as C
import Cardano.BM.Trace (nullTracer)
import Cardano.Protocol.Socket.Client (ChainSyncEvent (Resume, RollBackward, RollForward), runChainSync)
import Control.Concurrent (threadDelay)
import Control.Lens.Operators ((^.))
import Control.Monad (forever, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (findIndex)
import Data.Map (assocs)
import Data.Maybe (fromJust, fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Index.VSplit qualified as Ix
import Ledger.TimeSlot (SlotConfig (..))
import Marconi.Index.Datum (DatumIndex)
import Marconi.Index.Datum qualified as Ix
import Options.Applicative (Parser, execParser, fullDesc, header, help, helper, info, long, metavar, progDesc,
                            strOption, (<**>))
import Plutus.ChainIndex.Tx (ChainIndexTx (..))
import Plutus.Contract.CardanoAPI (fromCardanoTx)
import Plutus.Script.Utils.V1.Scripts (Datum, DatumHash)

{- | This executable is meant to exercise a set of indexers (for now datumhash -> datum)
     against the mainnet (meant to be used for testing).

     In case you want to access the results of the datumhash indexer you need to query
     the resulting database:
     $ sqlite3 datums.sqlite
     > select slotNo, datumHash, datum from kv_datumhsh_datum where slotNo = 39920450;
     39920450|679a55b523ff8d61942b2583b76e5d49498468164802ef1ebe513c685d6fb5c2|X(002f9787436835852ea78d3c45fc3d436b324184
-}

-- Options
data Options = Options
  { socketPath :: FilePath
  , dbPath     :: FilePath
  }

options :: Parser Options
options = Options
      <$> strOption
          ( long "socket"
         <> metavar "SOCKET"
         <> help "Path to node socket." )
      <*> strOption
          ( long "database"
         <> metavar "DATABASE"
         <> help "Path to database." )

-- We only care about the mainnet
slotConfig :: SlotConfig
slotConfig =
  SlotConfig
    { scSlotZeroTime = 1596059091000
    , scSlotLength   = 1000
    }

networkId :: NetworkId
networkId = Mainnet

-- We don't generally need to sync blocks earlier than the Goguen era (other than
-- testing for memory leaks) so we may want to start synchronising from a slot that
-- is closer to Goguen era.
closeToGoguen :: ChainPoint
closeToGoguen =
  ChainPoint
    (SlotNo 39795032)
    (fromJust $ parseHash "3e6f6450f85962d651654ee66091980b2332166f5505fd10b97b0520c9efac90")

parseHash :: String -> Maybe (Hash BlockHeader)
parseHash hash =
  deserialiseFromRawBytesHex (proxyToAsType Proxy) (encodeUtf8 $ pack hash)

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
  -- Not really supported
  Resume point         -> putStrLn ("resume " <> show point) >> pure ()
  RollForward blk@(BlockInMode (Block (BlockHeader slotNo _ blockNo@(BlockNo b)) _txs) _era) _tip -> do
    when (b `rem` 1000 == 0) $
      putStrLn $ show slotNo <> " / " <> show blockNo
    ix  <- readIORef ixref
    ix' <- Ix.insert (getDatums blk) ix
    writeIORef ixref ix'
  RollBackward point tip -> do
    putStrLn ("rollback to " <> show tip)
    rollbackToPoint point ixref

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
  options' <- execParser opts
  tix <- Ix.open (dbPath options') (Ix.Depth 2160) >>= newIORef
  _ <- runChainSync (socketPath options')
                    nullTracer
                    slotConfig
                    networkId
                    [closeToGoguen]
                    (processBlock tix)
  forever $ threadDelay 1000000000
  where
    opts = info (options <**> helper)
             ( fullDesc
            <> progDesc "Synchronise datums with mainnet"
            <> header "indexer - an indexing proof of concept" )
