{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Cardano.Api qualified as C
import Marconi.CLI qualified as M
import Marconi.Indexers qualified as I
import Options.Applicative qualified as Opt
import Plutus.Streaming (withChainSyncEventStream)

data Args = Args
  { socket     :: FilePath
  , dbPath     :: FilePath
  , networkId  :: C.NetworkId
  , chainPoint :: C.ChainPoint
  } deriving (Show)

args :: Opt.Parser Args
args = Args
  <$> Opt.strOption (Opt.long "socket" <> Opt.metavar "FILE" <> Opt.help "Socket path to node")
  <*> Opt.strOption (Opt.long "db" <> Opt.metavar "FILE" <> Opt.help "Path to the utxo database.")
  <*> pNetworkId
  <*> M.chainPointParser
  where
    -- TODO: `pNetworkId` and `pTestnetMagic` are copied from
    -- https://github.com/input-output-hk/cardano-node/blob/988c93085022ed3e2aea5d70132b778cd3e622b9/cardano-cli/src/Cardano/CLI/Shelley/Parsers.hs#L2009-L2027
    -- Use them from there whenever they are exported.
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

main :: IO ()
main = do
  Args {socket, dbPath, networkId, chainPoint} <- Opt.execParser opts
  let indexers = I.combineIndexers [(I.utxoWorker Nothing, dbPath)]

  withChainSyncEventStream socket networkId chainPoint indexers

-- TODO: remove this line -- triggering a CI rerun.
