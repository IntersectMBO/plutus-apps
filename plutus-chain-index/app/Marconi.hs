{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import Cardano.Api (Block (Block), BlockHeader (BlockHeader), BlockInMode (BlockInMode), CardanoMode,
                    ChainPoint (ChainPoint, ChainPointAtGenesis), Hash, NetworkId (Mainnet, Testnet),
                    NetworkMagic (NetworkMagic), SlotNo (SlotNo), Tx (Tx), chainPointToSlotNo,
                    deserialiseFromRawBytesHex, proxyToAsType)
import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logError)
import Cardano.BM.Tracing (defaultConfigStdout)
import Control.Exception (catch)
import Control.Lens.Operators ((^.))
import Data.ByteString.Char8 qualified as C8
import Data.List (findIndex)
import Data.Map (assocs)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Index.VSplit qualified as Ix
import Ledger.Tx.CardanoAPI (scriptDataFromCardanoTxBody)
import Marconi.Index.Datum (DatumIndex)
import Marconi.Index.Datum qualified as Ix
import Marconi.Logging (logging)
import Options.Applicative (Parser, auto, execParser, flag', help, helper, info, long, maybeReader, metavar, option,
                            readerError, strOption, (<**>), (<|>))
import Plutus.Script.Utils.V1.Scripts (Datum, DatumHash)
import Plutus.Streaming (ChainSyncEvent (RollBackward, RollForward), ChainSyncEventException (NoIntersectionFound),
                         withChainSyncEventStream)
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)
import Streaming.Prelude qualified as S

-- | This executable is meant to exercise a set of indexers (for now datumhash -> datum)
--     against the mainnet (meant to be used for testing).
--
--     In case you want to access the results of the datumhash indexer you need to query
--     the resulting database:
--     $ sqlite3 datums.sqlite
--     > select slotNo, datumHash, datum from kv_datumhsh_datum where slotNo = 39920450;
--     39920450|679a55b523ff8d61942b2583b76e5d49498468164802ef1ebe513c685d6fb5c2|X(002f9787436835852ea78d3c45fc3d436b324184
data Options = Options
  { optionsSocketPath   :: String,
    optionsNetworkId    :: NetworkId,
    optionsChainPoint   :: ChainPoint,
    optionsDatabasePath :: FilePath
  }
  deriving (Show)

parseOptions :: IO Options
parseOptions = execParser $ info (optionsParser <**> helper) mempty

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption (long "socket-path" <> help "Path to node socket.")
    <*> networkIdParser
    <*> chainPointParser
    <*> strOption (long "database-path" <> help "Path to database.")

networkIdParser :: Parser NetworkId
networkIdParser =
  pMainnet <|> pTestnet
  where
    pMainnet =
      flag'
        Mainnet
        ( long "mainnet"
            <> help "Use the mainnet magic id."
        )

    pTestnet =
      Testnet . NetworkMagic
        <$> option
          auto
          ( long "testnet-magic"
              <> metavar "NATURAL"
              <> help "Specify a testnet magic id."
          )

chainPointParser :: Parser ChainPoint
chainPointParser =
  pure ChainPointAtGenesis
    <|> ( ChainPoint
            <$> option (SlotNo <$> auto) (long "slot-no" <> metavar "SLOT-NO")
            <*> option
              (maybeReader maybeParseHashBlockHeader <|> readerError "Malformed block hash")
              (long "block-hash" <> metavar "BLOCK-HASH")
        )

getDatums :: BlockInMode CardanoMode -> [(SlotNo, (DatumHash, Datum))]
getDatums (BlockInMode (Block (BlockHeader slotNo _ _) txs) _) = concatMap extractDatumsFromTx txs
  where
    extractDatumsFromTx
      :: Tx era
      -> [(SlotNo, (DatumHash, Datum))]
    extractDatumsFromTx (Tx txBody _) =
      let hashes = assocs . fst $ scriptDataFromCardanoTxBody txBody
       in map (slotNo,) hashes

main :: IO ()
main = do
  Options {optionsSocketPath, optionsNetworkId, optionsChainPoint, optionsDatabasePath} <- parseOptions

  let initial :: IO DatumIndex
      initial = Ix.open optionsDatabasePath (Ix.Depth 2160)

      step :: DatumIndex -> ChainSyncEvent (BlockInMode CardanoMode) -> IO DatumIndex
      step index (RollForward blk _ct) =
        Ix.insert (getDatums blk) index
      step index (RollBackward cp _ct) = do
        events <- Ix.getEvents (index ^. Ix.storage)
        return $
          fromMaybe index $ do
            slot <- chainPointToSlotNo cp
            offset <- findIndex (any (\(s, _) -> s < slot)) events
            Ix.rewind offset index

      finish :: DatumIndex -> IO ()
      finish _index = pure () -- Nothing to do here, perhaps we should use this to close the database?
  c <- defaultConfigStdout

  withTrace c "marconi" $ \trace ->
    withChainSyncEventStream
      optionsSocketPath
      optionsNetworkId
      optionsChainPoint
      (S.foldM_ step initial finish . logging trace)
      `catch` \NoIntersectionFound ->
        logError trace $
          renderStrict $
            layoutPretty defaultLayoutOptions $
              "No intersection found when looking for the chain point" <+> pretty optionsChainPoint <> "."
                <+> "Please check the slot number and the block hash do belong to the chain"

maybeParseHashBlockHeader :: String -> Maybe (Hash BlockHeader)
maybeParseHashBlockHeader = deserialiseFromRawBytesHex (proxyToAsType Proxy) . C8.pack
