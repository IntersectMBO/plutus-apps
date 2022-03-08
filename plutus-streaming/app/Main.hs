{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cardano.Api
import Cardano.Api.Extras ()
import Data.Maybe qualified as Maybe
import Options.Applicative hiding (header)
import Plutus.Streaming
import Plutus.Streaming.ChainIndex
import Streaming
import Streaming.Prelude qualified as S
import Text.Pretty.Simple (pPrint)

--
-- Options parsing
--

data Example
  = Print
  | HowManyBlocksBeforeRollback
  | HowManyBlocksBeforeRollbackImpure
  | ComposePureAndImpure
  | ChainIndex
  deriving (Show, Read)

data Options
  = Simple
      { optionsSocketPath :: String,
        optionsChainPoint :: ChainPoint,
        optionsExample    :: Example
      }
  | WithLedgerState
      { optionsNetworkConfigPath :: String,
        optionsSocketPath        :: String,
        optionsChainPoint        :: ChainPoint
      }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  subparser
    ( command "simple" (info simple (progDesc "simple"))
        <> command "with-ledger-state" (info withLedgerState (progDesc "withLedgerSate"))
    )
  where
    simple =
      Simple
        <$> strOption (long "socket-path" <> help "Node socket path")
        <*> chainPointParser
        <*> option auto (long "example" <> value Print)

    withLedgerState =
      WithLedgerState
        <$> strOption (long "network-config-path" <> help "Node config path")
        <*> strOption (long "socket-path" <> help "Node socket path")
        <*> chainPointParser

chainPointParser :: Parser ChainPoint
chainPointParser =
  pure ChainPointAtGenesis
    <|> ( ChainPoint
            <$> option (SlotNo <$> auto) (long "slot-no" <> metavar "SLOT-NO")
            <*> option str (long "block-hash" <> metavar "BLOCK-HASH")
        )

--
-- Utilities
--

pPrintStream :: (Show a, MonadIO m) => Stream (Of a) m r -> m r
pPrintStream = S.mapM_ pPrint

--
-- Example consumers
--

howManyBlocksBeforeRollback ::
  Monad m =>
  Stream (Of SimpleChainSyncEvent) m r ->
  Stream (Of Int) m r
howManyBlocksBeforeRollback =
  S.scan
    ( \acc ->
        \case
          RollForward _ _  -> acc + 1
          RollBackward _ _ -> acc
    )
    0
    id

howManyBlocksBeforeRollbackImpure ::
  (Monad m, MonadIO m) =>
  Stream (Of SimpleChainSyncEvent) m r ->
  Stream (Of Int) m r
howManyBlocksBeforeRollbackImpure =
  S.scanM
    ( \acc ->
        \case
          RollForward _ _ ->
            pure $ acc + 1
          RollBackward _ _ -> do
            liftIO $ putStrLn $ "Rollback after " ++ show acc ++ " blocks"
            pure acc
    )
    (pure 0)
    pure

-- composePureAndImpure ::
--   Stream (Of SimpleChainSyncEvent) IO r ->
--   IO r
-- composePureAndImpure =
--   (pPrintStream . howManyBlocksBeforeRollbackImpure)
--     . (pPrintStream . howManyBlocksBeforeRollback)
--     . S.copy

--
-- Main
--

main :: IO ()
main = do
  options <- execParser $ info (optionsParser <**> helper) mempty

  case options of
    Simple {optionsSocketPath, optionsChainPoint, optionsExample} ->
      withSimpleChainSyncEventStream
        optionsSocketPath
        Mainnet
        optionsChainPoint
        (doSimple optionsExample)
        >>= print
    WithLedgerState {optionsNetworkConfigPath, optionsSocketPath, optionsChainPoint} ->
      withChainSyncEventStreamWithLedgerState
        optionsNetworkConfigPath
        optionsSocketPath
        Mainnet
        optionsChainPoint
        pPrintStream
        >>= print

doSimple ::
  Example ->
  Stream (Of SimpleChainSyncEvent) IO r ->
  IO r
doSimple Print =
  S.print
    . S.map
      ( \case
          RollForward (BlockInMode (Block header _txs) _era) _ct -> "RollForward, header: " <> show header
          RollBackward cp _ct                                    -> "RollBackward, point: " <> show cp
      )
doSimple HowManyBlocksBeforeRollback =
  S.print . howManyBlocksBeforeRollback
doSimple HowManyBlocksBeforeRollbackImpure =
  S.print . howManyBlocksBeforeRollbackImpure
doSimple ComposePureAndImpure =
  error "Not implemented"
doSimple ChainIndex =
  S.print . utxoState

--
-- Utilities for development
--

nthBlock :: Int -> IO (BlockInMode CardanoMode)
nthBlock = nthBlockAt ChainPointAtGenesis

nthBlockAt :: ChainPoint -> Int -> IO (BlockInMode CardanoMode)
nthBlockAt point n = do
  withSimpleChainSyncEventStream
    "/tmp/node.socket"
    Mainnet
    point
    ( fmap Maybe.fromJust
        . S.head_
        . S.drop n
        . S.catMaybes
        . S.drop n
        . S.map (\case RollForward bim _ -> Just bim; _ -> Nothing)
    )
