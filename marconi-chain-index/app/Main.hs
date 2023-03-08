{-# LANGUAGE OverloadedStrings #-}
module Main where

import Cardano.Api qualified as C
import Marconi.ChainIndex.CLI qualified as Cli
import Marconi.ChainIndex.Indexers (filterIndexers, mkIndexerStream, runIndexers, startIndexers)
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  o <- Cli.parseOptions
  createDirectoryIfMissing True (Cli.optionsDbPath o)
  let indexers = filterIndexers (Cli.utxoDbPath o)
                                (Cli.addressDatumDbPath o)
                                (Cli.datumDbPath o)
                                (Cli.scriptTxDbPath o)
                                (Cli.epochStakepoolSizeDbPath o)
                                (Cli.mintBurnDbPath o)
                                (Cli.optionsTargetAddresses o)
                                (Cli.optionsNodeConfigPath o)
  (returnedCp, coordinator) <- startIndexers indexers
  -- If the user specifies the chain point then use that,
  -- otherwise use what the indexers provide.
  let preferredChainPoints = case Cli.optionsChainPoint o of
        C.ChainPointAtGenesis -> returnedCp
        cliCp                 -> [cliCp]

  runIndexers
    (Cli.optionsSocketPath o)
    (Cli.optionsNetworkId o)
    preferredChainPoints
    (mkIndexerStream coordinator)
    "marconi"
