module Marconi.CLI
    (chainPointParser
    , multiString
    , parseCardanoAddresses
    , pNetworkId
    ) where

import Cardano.Api qualified as C
import Control.Applicative (some)
import Data.ByteString.Char8 qualified as C8
import Data.List (nub)
import Data.List.NonEmpty (fromList)
import Data.Proxy (Proxy (Proxy))
import Data.Text (pack)
import Options.Applicative qualified as Opt

import Marconi.Types (CardanoAddress, TargetAddresses)

chainPointParser :: Opt.Parser C.ChainPoint
chainPointParser =
  pure C.ChainPointAtGenesis
    Opt.<|> ( C.ChainPoint
            <$> Opt.option (C.SlotNo <$> Opt.auto) (Opt.long "slot-no" <> Opt.metavar "SLOT-NO")
            <*> Opt.option
              (Opt.maybeReader maybeParseHashBlockHeader Opt.<|> Opt.readerError "Malformed block hash")
              (Opt.long "block-hash" <> Opt.metavar "BLOCK-HASH")
        )
  where
    maybeParseHashBlockHeader :: String -> Maybe (C.Hash C.BlockHeader)
    maybeParseHashBlockHeader =
      either (const Nothing) Just
      . C.deserialiseFromRawBytesHex (C.proxyToAsType Proxy)
      . C8.pack

-- | Exit program with error
-- Note, if the targetAddress parser fails, or is empty, there is nothing to do for the hotStore.
-- In such case we should fail fast
fromJustWithError :: (Show e) => Either e a -> a
fromJustWithError v = case v of
    Left e ->
        error $ "\n!!!\n Abnormal Termination with Error: " <> show e <> "\n!!!\n"
    Right accounts -> accounts
-- TODO: `pNetworkId` and `pTestnetMagic` are copied from
-- https://github.com/input-output-hk/cardano-node/blob/988c93085022ed3e2aea5d70132b778cd3e622b9/cardano-cli/src/Cardano/CLI/Shelley/Parsers.hs#L2009-L2027
-- Use them from there whenever they are exported.
pNetworkId :: Opt.Parser C.NetworkId
pNetworkId = pMainnet Opt.<|> fmap C.Testnet pTestnetMagic

pMainnet :: Opt.Parser C.NetworkId
pMainnet = Opt.flag' C.Mainnet (Opt.long "mainnet" <> Opt.help "Use the mainnet magic id.")

pTestnetMagic :: Opt.Parser C.NetworkMagic
pTestnetMagic = C.NetworkMagic <$> Opt.option Opt.auto
    (Opt.long "testnet-magic"
     <> Opt.metavar "NATURAL"
     <> Opt.help "Specify a testnet magic id.")

-- | parses CLI params to valid NonEmpty list of Shelley addresses
-- We error out if there are any invalid addresses
multiString :: Opt.Mod Opt.OptionFields [CardanoAddress] -> Opt.Parser TargetAddresses
multiString desc = fromList . concat <$> some single
  where
    single = Opt.option (Opt.str >>= (pure . parseCardanoAddresses)) desc

parseCardanoAddresses :: String -> [CardanoAddress]
parseCardanoAddresses =  nub
    . fromJustWithError
    . traverse (deserializeToCardano . pack)
    . words
    where
        deserializeToCardano = C.deserialiseFromBech32 (C.proxyToAsType Proxy)
