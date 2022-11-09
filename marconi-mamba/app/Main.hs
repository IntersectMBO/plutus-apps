{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Options.Applicative qualified as Opt

import Control.Applicative (optional)
import Control.Concurrent.Async (race_)
import Marconi.Api.Types (CliArgs (CliArgs))
import Marconi.Bootstrap (bootstrapHttp, bootstrapJsonRpc, bootstrapUtxoIndexers)
import Marconi.CLI (multiString, pNetworkId)


args :: Opt.Parser CliArgs
args = CliArgs
  <$> Opt.strOption (Opt.long "socket-path" <> Opt.metavar "FILE" <> Opt.help "Socket path to node")
  <*> Opt.strOption (Opt.long "utxo-db" <> Opt.metavar "FILE" <> Opt.help "Path to the utxo database.")
  <*> (optional . Opt.option  Opt.auto) (
        Opt.long "http-port" <> Opt.metavar "HTTP-PORT" <> Opt.help "JSON-RPC http port number, default is port 3000.")
  <*> pNetworkId
  <*> multiString (Opt.long "addresses-to-index"
                        <> Opt.help ("Becch32 Shelley addresses to index."
                                 <> " i.e \"--address-to-index address-1 --address-to-index address-2 ...\"" ) )


opts :: Opt.ParserInfo CliArgs
opts = Opt.info (args Opt.<**> Opt.helper)
    ( Opt.fullDesc <> Opt.header "marconi-mamba - Cardano blockchain indexer" )

-- | concurrently start:
-- JSON-RPC server
-- marconi utxo worker
-- Exceptions in either thread will end the program
--
main :: IO ()
main = do
    cli@(CliArgs _ utxoDbPath maybePort nId tAddress)  <- Opt.execParser opts
    rpcEnv <- bootstrapJsonRpc utxoDbPath maybePort tAddress nId
    race_
       (bootstrapHttp rpcEnv)                            -- start http server
       (bootstrapUtxoIndexers cli rpcEnv)
