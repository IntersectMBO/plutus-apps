{-# LANGUAGE OverloadedStrings #-}
module Main where

import Marconi.ChainIndex.CLI qualified as Cli
import Marconi.ChainIndex.Indexers qualified as Indexers
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  o <- Cli.parseOptions
  createDirectoryIfMissing True (Cli.optionsDbPath o)
  let
    maybeTargetAddresses = Cli.optionsTargetAddresses o
    indexers =
      [ (Indexers.utxoWorker (\_ -> pure ()) maybeTargetAddresses, Cli.utxoDbPath o)
      , (Indexers.addressDatumWorker (\_ -> pure []) maybeTargetAddresses, Cli.addressDatumDbPath o)
      , (Indexers.datumWorker, Cli.datumDbPath o)
      , (Indexers.scriptTxWorker (\_ -> pure []), Cli.scriptTxDbPath o)
      , (Indexers.mintBurnWorker (\_ -> pure ()), Cli.mintBurnDbPath o)
      ] <> case Cli.optionsNodeConfigPath o of
      Just configPath ->
        [(Indexers.epochStateWorker configPath (\_ -> pure ()), Cli.epochStateDbPath o)]
      Nothing         -> []

  Indexers.runIndexers
    (Cli.optionsSocketPath o)
    (Cli.optionsNetworkId o)
    (Cli.optionsChainPoint o)
    "marconi-chain-index"
    indexers
