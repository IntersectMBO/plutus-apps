{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Options.Applicative qualified as Opt

import Cardano.Api qualified as C
import Control.Applicative (optional)
import Control.Concurrent.Async (race_)
import Marconi.Api.Types (CliArgs (CliArgs), TargetAddresses)
import Marconi.Bootstrap (bootstrapHttp, bootstrapJsonRpc, bootstrapUtxoIndexers, targetAddressParser)


args :: Opt.Parser CliArgs
args = CliArgs
  <$> Opt.strOption (Opt.long "socket-path" <> Opt.metavar "FILE" <> Opt.help "Socket path to node")
  <*> Opt.strOption (Opt.long "utxo-db" <> Opt.metavar "FILE" <> Opt.help "Path to the utxo database.")
  <*> (optional . Opt.option  Opt.auto) (
        Opt.long "http-port" <> Opt.metavar "HTTP-PORT" <> Opt.help "JSON-RPC http port number, default is port 3000.")
  <*> pNetworkId
  <*> pAddressesParser
    -- TODO: `pNetworkId` and `pTestnetMagic` are copied from
    -- https://github.com/input-output-hk/cardano-node/blob/988c93085022ed3e2aea5d70132b778cd3e622b9/cardano-cli/src/Cardano/CLI/Shelley/Parsers.hs#L2009-L2027
    -- Use them from there whenever they are exported.
pNetworkId :: Opt.Parser C.NetworkId
pNetworkId = pMainnet Opt.<|> fmap C.Testnet pTestnetMagic

pAddressesParser :: Opt.Parser TargetAddresses
pAddressesParser = targetAddressParser <$> Opt.strOption
    (Opt.long "addresses-to-index"
     <> Opt.metavar "Address"
     <> Opt.help ("White space separated list of addresses to index."
                  <>  " i.e \"address-1 address-2 address-3 ...\"" ) )

pMainnet :: Opt.Parser C.NetworkId
pMainnet = Opt.flag' C.Mainnet (Opt.long "mainnet" <> Opt.help "Use the mainnet magic id.")

pTestnetMagic :: Opt.Parser C.NetworkMagic
pTestnetMagic = C.NetworkMagic <$> Opt.option Opt.auto
    (Opt.long "testnet-magic"
     <> Opt.metavar "NATURAL"
     <> Opt.help "Specify a testnet magic id.")

opts :: Opt.ParserInfo CliArgs
opts = Opt.info (args Opt.<**> Opt.helper)
    ( Opt.fullDesc <> Opt.header "marconi-mamba - Cardano blockchain indexer" )

main :: IO ()
main = do
    cli@(CliArgs _ utxoDbPath maybePort _ tAddress)  <- Opt.execParser opts
    putStrLn $ "Processing addresses:\n " <> show tAddress <> "\n"
    rpcEnv <- bootstrapJsonRpc utxoDbPath maybePort tAddress
    race_
       (bootstrapHttp rpcEnv)                            -- start http server
       (bootstrapUtxoIndexers cli rpcEnv)
