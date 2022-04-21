{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cardano.Api (Block (Block), BlockInMode (BlockInMode), ChainPoint (ChainPoint, ChainPointAtGenesis),
                    NetworkId (Mainnet, Testnet), NetworkMagic (NetworkMagic), SlotNo (SlotNo))
import Cardano.Api.Extras ()
import Data.Aeson.Text qualified as Aeson
import Data.Text.Lazy qualified as TL
import Options.Applicative (Alternative ((<|>)), Parser, auto, execParser, flag', help, helper, info, long, metavar,
                            option, str, strOption, (<**>))
import Plutus.Streaming (ChainSyncEvent (RollBackward, RollForward), withSimpleChainSyncEventStream)
import Streaming.Prelude qualified as S

--
-- Options parsing
--

data Options = Options
  { optionsSocketPath :: String,
    optionsNetworkId  :: NetworkId,
    optionsChainPoint :: ChainPoint
  }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption (long "socket-path" <> help "Node socket path")
    <*> networkIdParser
    <*> chainPointParser

networkIdParser :: Parser NetworkId
networkIdParser =
  pMainnet' <|> fmap Testnet testnetMagicParser
  where
    pMainnet' :: Parser NetworkId
    pMainnet' =
      flag'
        Mainnet
        ( long "mainnet"
            <> help "Use the mainnet magic id."
        )

testnetMagicParser :: Parser NetworkMagic
testnetMagicParser =
  NetworkMagic
    <$> option
      auto
      ( long "testnet-magic"
          <> metavar "NATURAL"
          <> help "Specify a testnet magic id."
      )

chainPointParser :: Parser ChainPoint
chainPointParser =
  pure ChainPointAtGenesis
    <|> ( ChainPoint
            <$> option (SlotNo <$> auto) (long "slot-no" <> metavar "SLOT-NO")
            <*> option str (long "block-hash" <> metavar "BLOCK-HASH")
        )

--
-- Main
--

main :: IO ()
main = do
  Options {optionsSocketPath, optionsNetworkId, optionsChainPoint} <-
    execParser $ info (optionsParser <**> helper) mempty

  withSimpleChainSyncEventStream
    optionsSocketPath
    optionsNetworkId
    optionsChainPoint
    $ S.stdoutLn
      . S.map
        ( \case
            RollForward (BlockInMode (Block header _txs) _era) _ct ->
              "RollForward, header: " <> TL.unpack (Aeson.encodeToLazyText header)
            RollBackward cp _ct ->
              "RollBackward, point: " <> TL.unpack (Aeson.encodeToLazyText cp)
        )
