{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Cardano.Api qualified as C
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM.TVar (newTVarIO)
import Data.Map qualified
import Marconi.Api.HttpServer qualified as Http
import Marconi.Api.Types (HttpEnv (HttpEnv))
import Marconi.Indexers qualified as I
import Options.Applicative qualified as Opt
import Plutus.Streaming (withChainSyncEventStream)

data Args = Args
  { socket    :: FilePath
  , dbPath    :: FilePath
  , httpPort  :: Int
  , networkId :: C.NetworkId
  } deriving (Show)

args :: Opt.Parser Args
args = Args
  <$> Opt.strOption (Opt.long "socket" <> Opt.metavar "FILE" <> Opt.help "Socket path to node")
  <*> Opt.strOption (Opt.long "db" <> Opt.metavar "FILE" <> Opt.help "Path to the utxo database.")
  <*> Opt.option  Opt.auto (Opt.long "port" <> Opt.metavar "HTTP-PORT" <> Opt.help "JSON-RPC http port number.")
  <*> pNetworkId
  where
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
marconiIndexer (Args socket dbPath _ networkId) =
  let
      indexers = I.combineIndexers [(I.utxoWorker Nothing, dbPath)]
      chainPoint = C.ChainPointAtGenesis
  in
      withChainSyncEventStream socket networkId chainPoint indexers

main :: IO ()
main = do
    a@(Args _ _ port _)  <- Opt.execParser opts
    cache <- newTVarIO Data.Map.empty
    let httpEnv = HttpEnv port cache
    race_  (Http.httpMain httpEnv) (marconiIndexer a) -- start marconiIndexer  & marconi JSON-RPC server
