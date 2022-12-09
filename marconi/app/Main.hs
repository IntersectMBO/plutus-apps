{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (catch)
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)

import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logError)
import Cardano.BM.Tracing (defaultConfigStdout)
import Cardano.Streaming (ChainSyncEventException (NoIntersectionFound), withChainSyncEventStream)
import Marconi.CLI qualified as Cli
import Marconi.Indexers (combinedIndexer)
import Marconi.Logging (logging)
import System.Directory (createDirectoryIfMissing)
main :: IO ()
main = do
  o <- Cli.parseOptions
  createDirectoryIfMissing True (Cli.optionsDbPath o)
  c <- defaultConfigStdout
  withTrace c "marconi" $ \trace ->
    withChainSyncEventStream
      (Cli.optionsSocketPath o)
      (Cli.optionsNetworkId o)
      [Cli.optionsChainPoint o]
      (combinedIndexer (Cli.utxoDbPath o) (Cli.datumDbPath o) (Cli.scriptTxDbPath o) (Cli.optionsTargetAddresses o ) . logging trace)
      `catch` \NoIntersectionFound ->
        logError trace $
          renderStrict $
            layoutPretty defaultLayoutOptions $
              "No intersection found when looking for the chain point" <+> (pretty . Cli.optionsChainPoint) o <> "."
                <+> "Please check the slot number and the block hash do belong to the chain"
