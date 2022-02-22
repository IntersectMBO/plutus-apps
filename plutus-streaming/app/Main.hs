{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Cardano.Api
import Cardano.Api.Extras ()
import Data.Maybe qualified as Maybe
import Options.Applicative
import Plutus.ChainIndex.Tx
import Plutus.Contract.CardanoAPI
import Plutus.Streaming
import Streaming
import Streaming.Prelude qualified as S

--
-- Options parsing
--

data Example
  = JustBlocks
  | HowManyBlocksBeforeRollback
  | HowManyBlocksBeforeRollbackImpure
  | ComposePureAndImpure
  deriving (Show, Read)

data Options = Options
  { optionsSocketPath :: String,
    optionsChainPoint :: ChainPoint,
    optionsExample    :: Example
  }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption (long "socket-path" <> help "Node socket path")
    <*> chainPointParser
    <*> option auto (long "example" <> value JustBlocks)

chainPointParser :: Parser ChainPoint
chainPointParser =
  pure ChainPointAtGenesis
    <|> ( ChainPoint
            <$> option (SlotNo <$> auto) (long "slot-no" <> metavar "SLOT-NO")
            <*> option str (long "block-hash" <> metavar "BLOCK-HASH")
        )

--
-- Example consumers
--

justBlocks ::
  Stream (Of ChainSyncEvent) IO () ->
  Stream (Of (Either FromCardanoError [ChainIndexTx])) IO ()
justBlocks =
  --
  -- read the following back to front
  --

  -- format
  S.map fromCardanoBlock
    -- filter out rollbacks (Do we have optics?)
    . S.catMaybes
    . S.map (\case RollForward bim _ -> Just bim; _ -> Nothing)
    -- take 10 blocks
    . S.take 10

howManyBlocksBeforeRollback ::
  Monad m =>
  Stream (Of ChainSyncEvent) m () ->
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
  Stream (Of ChainSyncEvent) m () ->
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
  Stream (Of ChainSyncEvent) IO () ->
  IO ()
composePureAndImpure =
    (S.print . howManyBlocksBeforeRollbackImpure)
    . (S.print . howManyBlocksBeforeRollback)
    . S.copy

--
-- Main
--

main :: IO ()
main = do
  Options {optionsSocketPath, optionsChainPoint, optionsExample} <-
    execParser $
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "hello - a test for optparse-applicative"
        )

  withChainSyncEventStream
    optionsSocketPath
    Mainnet
    optionsChainPoint
    $ case optionsExample of
      JustBlocks                        -> S.print . justBlocks
      HowManyBlocksBeforeRollback       -> S.print . howManyBlocksBeforeRollback
      HowManyBlocksBeforeRollbackImpure -> S.print . howManyBlocksBeforeRollbackImpure
      ComposePureAndImpure              -> composePureAndImpure

--
-- Utilities for development
--

nthBlock :: Int -> IO (BlockInMode CardanoMode)
nthBlock = nthBlockAt ChainPointAtGenesis

nthBlockAt :: ChainPoint -> Int -> IO (BlockInMode CardanoMode)
nthBlockAt point n = do
  withChainSyncEventStream
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

