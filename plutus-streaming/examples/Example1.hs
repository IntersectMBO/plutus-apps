module Main where

import Cardano.Api qualified
import Common (Options (Options, optionsChainPoint, optionsNetworkId, optionsSocketPath), parseOptions)
import Data.Aeson.Text qualified as Aeson
import Data.Text.Lazy qualified as TL
import Orphans ()
import Plutus.Streaming (ChainSyncEvent (RollBackward, RollForward), withChainSyncEventStream)
import Streaming.Prelude qualified as S

--
-- Main
--

main :: IO ()
main = do
  Options {optionsSocketPath, optionsNetworkId, optionsChainPoint} <- parseOptions

  withChainSyncEventStream optionsSocketPath optionsNetworkId optionsChainPoint $
    S.stdoutLn
      . S.map
        ( \case
            RollForward (Cardano.Api.BlockInMode (Cardano.Api.Block header _txs) _era) _ct ->
              "RollForward, header: " <> TL.unpack (Aeson.encodeToLazyText header)
            RollBackward cp _ct ->
              "RollBackward, point: " <> TL.unpack (Aeson.encodeToLazyText cp)
        )
