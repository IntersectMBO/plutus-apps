module Marconi.MambaCli where

import Options.Applicative (Parser, ParserInfo, auto, execParser, fullDesc, header, help, helper, info, long, metavar,
                            option, optional, strOption, (<**>))

import Marconi.Api.Types (CliArgs (CliArgs))
import Marconi.CLI (multiString, pNetworkId)


-- | parse cli arguments
--
parserCliArgs :: Parser CliArgs
parserCliArgs = CliArgs
  <$> strOption (long "socket-path" <> metavar "FILE" <> help "Socket path to node")
  <*> strOption (long "utxo-db" <> metavar "FILE" <> help "Path to the utxo database.")
  <*> (optional . option  auto) (
        long "http-port" <> metavar "HTTP-PORT" <> help "JSON-RPC http port number, default is port 3000.")
  <*> pNetworkId
  <*> multiString (long "addresses-to-index"
                        <> help ("Bech32 Shelley addresses to index."
                                 <> " i.e \"--address-to-index address-1 --address-to-index address-2 ...\"" ) )

parserOpts :: ParserInfo CliArgs
parserOpts = info (parserCliArgs <**> helper)
    ( fullDesc <> header "marconi-mamba - Cardano blockchain indexer" )

parseCli :: IO CliArgs
parseCli = execParser parserOpts
