{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (catch)
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)

import Cardano.Api qualified as C
import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logError)
import Cardano.BM.Tracing (defaultConfigStdout)
import Cardano.Streaming (ChainSyncEventException (NoIntersectionFound), withChainSyncEventStream)
import Marconi.CLI qualified as Cli
import Marconi.Indexers (filterIndexers, mkIndexerStream, startIndexers)
import Marconi.Logging (logging)
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  o <- Cli.parseOptions
  createDirectoryIfMissing True (Cli.optionsDbPath o)
  c <- defaultConfigStdout
  withTrace c "marconi" $ \trace -> do
    let indexers = filterIndexers (Cli.utxoDbPath o)
                                  (Cli.datumDbPath o)
                                  (Cli.scriptTxDbPath o)
                                  (Cli.epochStakepoolSizeDbPath o)
                                  (Cli.optionsTargetAddresses o)
                                  (Cli.optionsNodeConfigPath o)
    (cp, coordinator) <- startIndexers indexers
    let preferredChainPoints =
          -- If the user specifies the chain point then use that,
          -- otherwise use what the indexers provide.
          if Cli.optionsChainPoint o == C.ChainPointAtGenesis
             then cp
             else [Cli.optionsChainPoint o]
    withChainSyncEventStream
      (Cli.optionsSocketPath o)
      (Cli.optionsNetworkId o)
      preferredChainPoints
      (mkIndexerStream coordinator . logging trace)
      `catch` \NoIntersectionFound ->
        logError trace $
          renderStrict $
            layoutPretty defaultLayoutOptions $
              "No intersection found when looking for the chain point" <+> (pretty . Cli.optionsChainPoint) o <> "."
                <+> "Please check the slot number and the block hash do belong to the chain"
