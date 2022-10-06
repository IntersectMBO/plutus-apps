{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Options.Applicative qualified as Opt

import Cardano.Api qualified as C
import Cardano.Streaming (withChainSyncEventStream)
import Control.Applicative (optional)
import Control.Concurrent.Async (race_)
import Marconi.Bootstrap (bootstrapJsonRpc, jsonRpcEnv)
import Marconi.Indexers qualified as I
import Marconi.IndexersHotStore (TargetAddresses, bootstrapHotStore, targetAddressParser)

data Args = Args
  { socket          :: FilePath         -- ^ POSIX socket file to communicate with cardano node
  , dbPath          :: FilePath         -- ^ filepath to local sqlite for utxo index table
  , httpPort        :: Maybe Int        -- ^ optional tcp/ip port number for JSON-RPC http server
  , networkId       :: C.NetworkId      -- ^ cardano network id
  , targetAddresses :: TargetAddresses  -- ^ white-space sepparated list of Bech32 plutus addresses
  } deriving (Show)

args :: Opt.Parser Args
args = Args
  <$> Opt.strOption (Opt.long "socket-path" <> Opt.metavar "FILE" <> Opt.help "Socket path to node")
  <*> Opt.strOption (Opt.long "utxo-db" <> Opt.metavar "FILE" <> Opt.help "Path to the utxo database.")
  <*> (optional . Opt.option  Opt.auto) (
        Opt.long "http-port" <> Opt.metavar "HTTP-PORT" <> Opt.help "JSON-RPC http port number, default is port 3000.")
  <*> pNetworkId
  <*> pAddressesParser
  where
    -- TODO: `pNetworkId` and `pTestnetMagic` are copied from
    -- https://github.com/input-output-hk/cardano-node/blob/988c93085022ed3e2aea5d70132b778cd3e622b9/cardano-cli/src/Cardano/CLI/Shelley/Parsers.hs#L2009-L2027
    -- Use them from there whenever they are exported.
    pNetworkId :: Opt.Parser C.NetworkId
    pNetworkId =
      pMainnet Opt.<|> fmap C.Testnet pTestnetMagic

    pAddressesParser = builtinDataAddresses <$> Opt.strOption
        (Opt.long "addresses-to-index" <> Opt.metavar "Address"
        <> Opt.help ("White space separated list of addresses to index."
                      <>  " i.e \"address-1 address-2 address-3 ...\"" ) )
    builtinDataAddresses :: String -> TargetAddresses
    builtinDataAddresses = targetAddressParser
    pMainnet :: Opt.Parser C.NetworkId
    pMainnet =
        Opt.flag' C.Mainnet (Opt.long "mainnet" <> Opt.help "Use the mainnet magic id.")
    pTestnetMagic :: Opt.Parser C.NetworkMagic
    pTestnetMagic =
      C.NetworkMagic <$>
        Opt.option Opt.auto
          (Opt.long "testnet-magic"
          <> Opt.metavar "NATURAL"
          <> Opt.help "Specify a testnet magic id."
          )

opts :: Opt.ParserInfo Args
opts = Opt.info (args Opt.<**> Opt.helper)
  ( Opt.fullDesc
 <> Opt.header "marconi-mamba - Cardano blockchain indexer" )

-- |  marconi cardano blockchain indexer
marconiIndexer :: Args -> IO ()
marconiIndexer (Args socket dbPath _ networkId targetAddresses ) = do
    hotStore <- bootstrapHotStore
    let
      indexers = I.combineIndexers [( I.utxoHotStoreWorker hotStore targetAddresses , dbPath)] --TODO should be hotStore
      chainPoint = C.ChainPointAtGenesis
    withChainSyncEventStream socket networkId chainPoint indexers

main :: IO ()
main = do
    a@(Args _ _ maybePort _ _)  <- Opt.execParser opts
    jsonRpcEnvironment <- jsonRpcEnv maybePort -- marconi JSON-RPC server
    race_
       (bootstrapJsonRpc jsonRpcEnvironment)    -- start http server
       (marconiIndexer a)                       -- start marconiIndexer
