{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
module Common where

import Cardano.Api qualified
import Data.Aeson qualified
import Data.ByteString.Lazy.Char8 qualified
import Options.Applicative (Alternative ((<|>)), Parser, auto, execParser, flag', help, helper, info, long, metavar,
                            option, str, strOption, (<**>))
import Orphans ()
import Streaming.Prelude qualified as S

--
-- Options parsing
--

{-
cabal v2-run plutus-streaming:plutus-streaming-example-1 -- \
  --socket-path $HOME/cardano/db/node.socket \
  --mainnet \
  --slot-no 672322 \
  --block-hash 7da97f50c78204bf38dcd6cc80f05c4c751bf24bc7e4f1d00fb43b20851783fa

cardano-node run \
--config $HOME/cardano/mainnet-config.json \
--database-path $HOME/cardano/db/ \
--socket-path $HOME/cardano/db/node.socket \
--host-addr 127.0.0.1 \
--port 1337 \
--topology $HOME/cardano/mainnet-topology.json

-}

data Options = Options
  { optionsSocketPath :: String,
    optionsNetworkId  :: Cardano.Api.NetworkId,
    optionsChainPoint :: Cardano.Api.ChainPoint
  }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption (long "socket-path" <> help "Node socket path")
    <*> networkIdParser
    <*> chainPointParser

networkIdParser :: Parser Cardano.Api.NetworkId
networkIdParser =
  pMainnet' <|> fmap Cardano.Api.Testnet testnetMagicParser
  where
    pMainnet' :: Parser Cardano.Api.NetworkId
    pMainnet' =
      flag'
        Cardano.Api.Mainnet
        ( long "mainnet"
            <> help "Use the mainnet magic id."
        )

testnetMagicParser :: Parser Cardano.Api.NetworkMagic
testnetMagicParser =
  Cardano.Api.NetworkMagic
    <$> option
      auto
      ( long "testnet-magic"
          <> metavar "NATURAL"
          <> help "Specify a testnet magic id."
      )

chainPointParser :: Parser Cardano.Api.ChainPoint
chainPointParser =
  pure Cardano.Api.ChainPointAtGenesis
    <|> ( Cardano.Api.ChainPoint
            <$> option (Cardano.Api.SlotNo <$> auto) (long "slot-no" <> metavar "SLOT-NO")
            <*> option str (long "block-hash" <> metavar "BLOCK-HASH")
        )

parseOptions :: IO Options
parseOptions = execParser $ info (optionsParser <**> helper) mempty

printJson :: Data.Aeson.ToJSON a => S.Stream (S.Of a) IO r -> IO r
printJson = S.mapM_ Data.ByteString.Lazy.Char8.putStrLn . S.map Data.Aeson.encode

-- https://github.com/input-output-hk/cardano-node/pull/3665
workaround ::
  (Cardano.Api.IsCardanoEra era => Cardano.Api.EraInMode era Cardano.Api.CardanoMode -> a) ->
  Cardano.Api.EraInMode era Cardano.Api.CardanoMode ->
  a
workaround k Cardano.Api.ByronEraInCardanoMode   = k Cardano.Api.ByronEraInCardanoMode
workaround k Cardano.Api.ShelleyEraInCardanoMode = k Cardano.Api.ShelleyEraInCardanoMode
workaround k Cardano.Api.AllegraEraInCardanoMode = k Cardano.Api.AllegraEraInCardanoMode
workaround k Cardano.Api.MaryEraInCardanoMode    = k Cardano.Api.MaryEraInCardanoMode
workaround k Cardano.Api.AlonzoEraInCardanoMode  = k Cardano.Api.AlonzoEraInCardanoMode
