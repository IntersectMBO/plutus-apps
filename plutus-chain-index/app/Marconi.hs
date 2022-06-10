{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import Cardano.Api (Block (Block), BlockHeader (BlockHeader), BlockInMode (BlockInMode), CardanoMode,
                    ChainPoint (ChainPoint, ChainPointAtGenesis), Hash, NetworkId (Mainnet, Testnet),
                    NetworkMagic (NetworkMagic), SlotNo (SlotNo), Tx (Tx), chainPointToSlotNo,
                    deserialiseFromRawBytesHex, proxyToAsType)
import Cardano.Api qualified as C
import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logError)
import Cardano.BM.Tracing (defaultConfigStdout)
import Control.Exception (catch)
import Control.Lens.Operators ((&), (<&>), (^.))
import Data.ByteString.Char8 qualified as C8
import Data.Foldable (foldl')
import Data.List (findIndex)
import Data.Map (assocs)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString)
import Index.VSplit qualified as Ix
import Ledger (TxIn (..), TxOut (..), TxOutRef (..))
import Ledger.Tx.CardanoAPI (fromCardanoTxId, fromCardanoTxIn, fromCardanoTxOut, fromTxScriptValidity,
                             scriptDataFromCardanoTxBody)
import Marconi.Index.Datum (DatumIndex)
import Marconi.Index.Datum qualified as Datum
import Marconi.Index.Utxo (UtxoIndex, UtxoUpdate (..))
import Marconi.Index.Utxo qualified as Utxo
import Marconi.Logging (logging)
import Options.Applicative (Mod, OptionFields, Parser, auto, execParser, flag, flag', help, helper, info, long,
                            maybeReader, metavar, option, readerError, strOption, (<**>), (<|>))
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
  { optionsSocketPath :: String,
    optionsNetworkId  :: NetworkId,
    optionsChainPoint :: ChainPoint,
    optionsUtxoPath   :: Maybe FilePath,
    optionsDatumPath  :: Maybe FilePath
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
    <*> optStrParser (long "utxo-db")
    <*> optStrParser (long "datum-db")

optStrParser :: IsString a => Mod OptionFields a -> Parser (Maybe a)
optStrParser fields = Just <$> strOption fields <|> pure Nothing

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

-- DatumIndexer
getDatums :: BlockInMode CardanoMode -> [(SlotNo, (DatumHash, Datum))]
getDatums (BlockInMode (Block (BlockHeader slotNo _ _) txs) _) = concatMap extractDatumsFromTx txs
  where
    extractDatumsFromTx
      :: Tx era
      -> [(SlotNo, (DatumHash, Datum))]
    extractDatumsFromTx (Tx txBody _) =
      let hashes = assocs . fst $ scriptDataFromCardanoTxBody txBody
       in map (slotNo,) hashes

datumIndexer
  :: FilePath
  -> S.Stream (S.Of (ChainSyncEvent (BlockInMode CardanoMode))) IO r
  -> IO ()
datumIndexer path = S.foldM_ step initial finish
  where
    initial :: IO DatumIndex
    initial = Datum.open path (Datum.Depth 2160)

    step :: DatumIndex -> ChainSyncEvent (BlockInMode CardanoMode) -> IO DatumIndex
    step index (RollForward blk _ct) =
      Ix.insert (getDatums blk) index
    step index (RollBackward cp _ct) = do
      events <- Ix.getEvents (index ^. Ix.storage)
      return $
        fromMaybe index $ do
          slot   <- chainPointToSlotNo cp
          offset <- findIndex (any (\(s, _) -> s < slot)) events
          Ix.rewind offset index

    finish :: DatumIndex -> IO ()
    finish _index = pure ()

-- UtxoIndexer
getOutputs
  :: C.Tx era
  -> Maybe [(TxOut, TxOutRef)]
getOutputs (C.Tx txBody@(C.TxBody C.TxBodyContent{C.txOuts}) _) = do
  outs <- either (const Nothing) Just $ traverse fromCardanoTxOut txOuts
  pure $ outs
    &  zip ([0..] :: [Integer])
   <&> (\(ix, out) -> (out, TxOutRef { txOutRefId  = fromCardanoTxId (C.getTxId txBody)
                                     , txOutRefIdx = ix
                                     }))

getInputs
  :: C.Tx era
  -> Set TxOutRef
getInputs (C.Tx (C.TxBody C.TxBodyContent{C.txIns, C.txScriptValidity, C.txInsCollateral}) _) =
  let isTxScriptValid = fromTxScriptValidity txScriptValidity
      inputs = if isTxScriptValid
                  then fst <$> txIns
                  else case txInsCollateral of
                    C.TxInsCollateralNone     -> []
                    C.TxInsCollateral _ txins -> txins
  in Set.fromList $ fmap (txInRef . (`TxIn` Nothing) . fromCardanoTxIn) inputs

getUtxoUpdate
  :: SlotNo
  -> [C.Tx era]
  -> UtxoUpdate
getUtxoUpdate slot txs =
  let ins  = foldl' Set.union Set.empty $ getInputs <$> txs
      outs = concat . catMaybes $ getOutputs <$> txs
  in  UtxoUpdate { _inputs  = ins
                 , _outputs = outs
                 , _slotNo  = slot
                 }

utxoIndexer
  :: FilePath
  -> S.Stream (S.Of (ChainSyncEvent (BlockInMode CardanoMode))) IO r
  -> IO ()
utxoIndexer path = S.foldM_ step initial finish
  where
    initial :: IO UtxoIndex
    initial = Utxo.open path (Utxo.Depth 2160)

    step :: UtxoIndex -> ChainSyncEvent (BlockInMode CardanoMode) -> IO UtxoIndex
    step index (RollForward (BlockInMode (Block (BlockHeader slotNo _ _) txs) _) _ct) =
      Ix.insert (getUtxoUpdate slotNo txs) index
    step index (RollBackward cp _ct) = do
      events <- Ix.getEvents (index ^. Ix.storage)
      return $
        fromMaybe index $ do
          slot   <- chainPointToSlotNo cp
          offset <- findIndex (\u -> (u ^. Utxo.slotNo) < slot) events
          Ix.rewind offset index

    finish :: UtxoIndex -> IO ()
    finish _index = pure ()

main :: IO ()
main = do
  Options { optionsSocketPath
          , optionsNetworkId
          , optionsChainPoint
          , optionsUtxoPath
          , optionsDatumPath } <- parseOptions

  c <- defaultConfigStdout

  let processor = undefined
        -- case optionsIndexerType of
        --   DatumIndexer -> datumIndexer optionsDatabasePath
        --   UtxoIndexer  -> utxoIndexer  optionsDatabasePath

  withTrace c "marconi" $ \trace ->
    withChainSyncEventStream
      optionsSocketPath
      optionsNetworkId
      optionsChainPoint
      (processor . logging trace)
      `catch` \NoIntersectionFound ->
        logError trace $
          renderStrict $
            layoutPretty defaultLayoutOptions $
              "No intersection found when looking for the chain point" <+> pretty optionsChainPoint <> "."
                <+> "Please check the slot number and the block hash do belong to the chain"

maybeParseHashBlockHeader :: String -> Maybe (Hash BlockHeader)
maybeParseHashBlockHeader = deserialiseFromRawBytesHex (proxyToAsType Proxy) . C8.pack
