{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Cardano.Api qualified as C
import Control.Applicative (optional)
import Control.Concurrent.Async (race_)
import Marconi.Bootstrap (bootstrapJsonRpc, jsonRpcEnv, targetAddressParser)
import Marconi.IndexerCache (TargetAddresses, initCache)
import Marconi.Indexers qualified as I
import Options.Applicative qualified as Opt
import Plutus.Streaming (withChainSyncEventStream)

data Args = Args
  { socket          :: FilePath
  , dbPath          :: FilePath
  , httpPort        :: Maybe Int
  , networkId       :: C.NetworkId
  , targetAddresses :: TargetAddresses

  } deriving (Show)

args :: Opt.Parser Args
args = Args
  <$> Opt.strOption (Opt.long "socket" <> Opt.metavar "FILE" <> Opt.help "Socket path to node")
  <*> Opt.strOption (Opt.long "db" <> Opt.metavar "FILE" <> Opt.help "Path to the utxo database.")
  <*> (optional . Opt.option  Opt.auto) (
        Opt.long "port" <> Opt.metavar "HTTP-PORT" <> Opt.help "JSON-RPC http port number.")
  <*> pNetworkId
  <*> pAddressesParser
  where
    pAddressesParser = builtinDataAddresses <$> Opt.strOption
        (Opt.long "addresses-to-index" <> Opt.metavar "Address"
        <> Opt.help ("White space separated list of addresses to index."
                      <>  " i.e \"address-1 address-2 address-3 ...\"" ) )
    builtinDataAddresses :: String -> TargetAddresses
    builtinDataAddresses = targetAddressParser

    pNetworkId :: Opt.Parser C.NetworkId
    pNetworkId =
      pMainnet Opt.<|> fmap C.Testnet pTestnetMagic
     where
       pMainnet :: Opt.Parser C.NetworkId
       pMainnet =
        Opt.flag' C.Mainnet
          (  Opt.long "mainnet"
          <> Opt.help "Use the mainnet magic id."
          )

    pTestnetMagic :: Opt.Parser C.NetworkMagic
    pTestnetMagic =
      C.NetworkMagic <$>
        Opt.option Opt.auto
          (  Opt.long "testnet-magic"
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
    cache <- initCache targetAddresses
    let
      indexers = I.combineIndexers [(I.utxoWorker(cache) , dbPath)] --TODO should be cache
      chainPoint = C.ChainPointAtGenesis
    withChainSyncEventStream socket networkId chainPoint indexers

main :: IO ()
main = do
    a@(Args _ _ port _ targetAddresses)  <- Opt.execParser opts
    jsonRpcEnvironment <- jsonRpcEnv port targetAddresses -- marconi JSON-RPC server
    race_
       (bootstrapJsonRpc jsonRpcEnvironment) -- marconi JSON-RPC server
       (marconiIndexer a) -- start marconiIndexer
