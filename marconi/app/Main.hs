{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (catch)
import Data.String (IsString)
import Options.Applicative (Mod, OptionFields, Parser, execParser, help, helper, info, long, strOption, (<**>))
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)

import Cardano.Api (ChainPoint, NetworkId)
import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logError)
import Cardano.BM.Tracing (defaultConfigStdout)
import Cardano.Streaming (ChainSyncEventException (NoIntersectionFound), withChainSyncEventStream)
import Control.Applicative (optional)
import Marconi.CLI (chainPointParser, multiString, pNetworkId)
import Marconi.Indexers (combinedIndexer)
import Marconi.Logging (logging)
import Marconi.Types (CardanoAddress, TargetAddresses)


-- | This executable is meant to exercise a set of indexers (for now datumhash -> datum)
--     against the mainnet (meant to be used for testing).
--
--     In case you want to access the results of the datumhash indexer you need to query
--     the resulting database:
--     $ sqlite3 datums.sqlite
--     > select slotNo, datumHash, datum from kv_datumhsh_datum where slotNo = 39920450;
--     39920450|679a55b523ff8d61942b2583b76e5d49498468164802ef1ebe513c685d6fb5c2|X(002f9787436835852ea78d3c45fc3d436b324184

data Options = Options
  { optionsSocketPath      :: String,
    optionsNetworkId       :: NetworkId,
    optionsChainPoint      :: ChainPoint,
    optionsUtxoPath        :: Maybe FilePath,
    optionsDatumPath       :: Maybe FilePath,
    optionsScriptTxPath    :: Maybe FilePath,
    optionsTargetAddresses :: Maybe TargetAddresses
  }
  deriving (Show)

parseOptions :: IO Options
parseOptions = execParser $ info (optionsParser <**> helper) mempty

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption (long "socket-path" <> help "Path to node socket.")
    <*> pNetworkId
    <*> chainPointParser
    <*> optStrParser (long "utxo-db" <> help "Path to the utxo database.")
    <*> optStrParser (long "datum-db" <> help "Path to the datum database.")
    <*> optStrParser (long "script-tx-db" <> help "Path to the script transactions' database.")
    <*> optAddressesParser (long "addresses-to-index"
                            <> help ("Becch32 Shelley addresses to index."
                                   <> " i.e \"--address-to-index address-1 --address-to-index address-2 ...\"" ) )

optAddressesParser ::Mod OptionFields [CardanoAddress] -> Parser (Maybe TargetAddresses)
optAddressesParser =  optional . multiString

optStrParser :: IsString a => Mod OptionFields a -> Parser (Maybe a)
optStrParser  = optional . strOption

main :: IO ()
main = do
  Options { optionsSocketPath
          , optionsNetworkId
          , optionsChainPoint
          , optionsUtxoPath
          , optionsDatumPath
          , optionsScriptTxPath
          , optionsTargetAddresses } <- parseOptions

  c <- defaultConfigStdout

  withTrace c "marconi" $ \trace ->
    withChainSyncEventStream
      optionsSocketPath
      optionsNetworkId
      optionsChainPoint
      (combinedIndexer optionsUtxoPath optionsDatumPath optionsScriptTxPath optionsTargetAddresses . logging trace)
      `catch` \NoIntersectionFound ->
        logError trace $
          renderStrict $
            layoutPretty defaultLayoutOptions $
              "No intersection found when looking for the chain point" <+> pretty optionsChainPoint <> "."
                <+> "Please check the slot number and the block hash do belong to the chain"
