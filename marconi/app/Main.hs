{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Main where

import Control.Exception (catch)
import Data.ByteString.Char8 qualified as C8
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString)
import Options.Applicative (Mod, OptionFields, Parser, auto, execParser, flag', help, helper, info, long, maybeReader,
                            metavar, option, readerError, strOption, (<**>), (<|>))
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)

import Cardano.Api (BlockHeader, ChainPoint (ChainPoint, ChainPointAtGenesis), Hash, NetworkId (Mainnet, Testnet),
                    NetworkMagic (NetworkMagic), SlotNo (SlotNo), deserialiseFromRawBytesHex, proxyToAsType)
import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logError)
import Cardano.BM.Tracing (defaultConfigStdout)

import Marconi.Indexers (combinedIndexer)
import Marconi.Logging (logging)
import Plutus.Streaming (ChainSyncEventException (NoIntersectionFound), withChainSyncEventStream)



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
    optionsUtxoPath     :: Maybe FilePath,
    optionsDatumPath    :: Maybe FilePath,
    optionsScriptTxPath :: Maybe FilePath
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
    <*> optStrParser (long "utxo-db" <> help "Path to the utxo database.")
    <*> optStrParser (long "datum-db" <> help "Path to the datum database.")
    <*> optStrParser (long "script-tx-db" <> help "Path to the script transactions' database.")

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
  where
    maybeParseHashBlockHeader :: String -> Maybe (Hash BlockHeader)
    maybeParseHashBlockHeader = deserialiseFromRawBytesHex (proxyToAsType Proxy) . C8.pack



main :: IO ()
main = do
  Options { optionsSocketPath
          , optionsNetworkId
          , optionsChainPoint
          , optionsUtxoPath
          , optionsDatumPath
          , optionsScriptTxPath } <- parseOptions

  c <- defaultConfigStdout

  withTrace c "marconi" $ \trace ->
    withChainSyncEventStream
      optionsSocketPath
      optionsNetworkId
      optionsChainPoint
      (combinedIndexer optionsUtxoPath optionsDatumPath optionsScriptTxPath . logging trace)
      `catch` \NoIntersectionFound ->
        logError trace $
          renderStrict $
            layoutPretty defaultLayoutOptions $
              "No intersection found when looking for the chain point" <+> pretty optionsChainPoint <> "."
                <+> "Please check the slot number and the block hash do belong to the chain"
