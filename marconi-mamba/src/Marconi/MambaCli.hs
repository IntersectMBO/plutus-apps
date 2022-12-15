module Marconi.MambaCli where

import Data.Maybe (fromMaybe)
import Options.Applicative (Parser, ParserInfo, auto, execParser, fullDesc, header, help, helper, info, infoOption,
                            long, metavar, option, optional, progDesc, strOption)
import System.Environment (lookupEnv)

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

parserOpts  :: String -> ParserInfo CliArgs
parserOpts sha =
    info (helper
          <*> versionOption
          <*> parserCliArgs)
    ( fullDesc
      <> progDesc "marconi-mamba"
      <> header
          "marconi - a lightweight customizable solution for indexing and querying the Cardano blockchain"
    )
    where
        versionOption  =
            infoOption sha (long "version"
                            <> help "Show git SHA")

parseCli :: IO CliArgs
parseCli = do
    maybeSha <- lookupEnv "GITHUB_SHA"
    let sha = fromMaybe "GIHUB_SHA environment variable not set!" maybeSha
    execParser $ parserOpts sha
