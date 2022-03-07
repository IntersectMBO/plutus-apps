{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cardano.Api
import Cardano.Api.Extras ()
import Control.Monad ((>=>))
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
  Stream (Of Int) m ()
howManyBlocksBeforeRollback =
  S.scan
    ( \acc ->
        \case
          RollForward _ _  -> acc + 1
          RollBackward _ _ -> acc
    )
    0
    id
    . S.take 100

howManyBlocksBeforeRollbackImpure ::
  (Monad m, MonadIO m) =>
  Stream (Of SimpleChainSyncEvent) m r ->
  Stream (Of Int) m ()
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
    . S.take 100

composePureAndImpure ::
  Stream (Of SimpleChainSyncEvent) IO r ->
  IO ()
composePureAndImpure =
  (pPrintStream . howManyBlocksBeforeRollbackImpure)
    . (pPrintStream . howManyBlocksBeforeRollback)
    . S.copy

--
-- Main
--

deriving instance Show BlockHeader

main :: IO ()
main = do
  options <- execParser $ info (optionsParser <**> helper) mempty

  case options of
    Simple {optionsSocketPath, optionsChainPoint, optionsExample} -> do
      withSimpleChainSyncEventStream
        optionsSocketPath
        Mainnet
        optionsChainPoint
        $ case optionsExample of
          Print ->
            S.stdoutLn . S.map (
              \case
                RollForward (BlockInMode (Block header _txs) _era) _ct -> "RollForward, header: " <> show header
                RollBackward cp _ct                                    -> "RollBackward, point: " <> show cp
            ) . S.take 10 >=> print
          HowManyBlocksBeforeRollback ->
            pPrintStream . howManyBlocksBeforeRollback >=> print
          HowManyBlocksBeforeRollbackImpure ->
            pPrintStream . howManyBlocksBeforeRollbackImpure >=> print
          ComposePureAndImpure ->
            composePureAndImpure >=> print
          ChainIndex ->
            -- pPrintStream  . utxoState . S.print . S.map (fmap f) . S.copy . S.take 10 >=> print
            pPrintStream  . utxoState . S.take 10 >=> print
    WithLedgerState {optionsNetworkConfigPath, optionsSocketPath, optionsChainPoint} ->
      withChainSyncEventStreamWithLedgerState
        optionsNetworkConfigPath
        optionsSocketPath
        Mainnet
        optionsChainPoint
        (pPrintStream . S.take 10 >=> print)

deriving instance Show LedgerState

deriving instance Show LedgerEvent

deriving instance Show MIRDistributionDetails

deriving instance Show PoolReapDetails

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
