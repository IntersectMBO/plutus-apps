module Marconi.CLI where

import Cardano.Api qualified as C
import Data.ByteString.Char8 qualified as C8
import Data.List.NonEmpty (fromList, nub)
import Data.Proxy (Proxy (Proxy))
import Data.Text (pack)
import Options.Applicative qualified as Opt

import Marconi.Types (TargetAddresses)

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

-- | parses a white space separated address list
-- Note, duplicate addresses are rmoved
targetAddressParser
    :: String           -- ^ contains white spece delimeted lis of addresses
    -> TargetAddresses  -- ^ a non empty list of valid addresses
targetAddressParser =
    nub
    . fromList
    . fromJustWithError
    . traverse (deserializeToCardano . pack)
    . words
    where
        deserializeToCardano = C.deserialiseFromBech32 (C.proxyToAsType Proxy)

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

pAddressesParser :: Opt.Parser TargetAddresses
pAddressesParser = targetAddressParser <$> Opt.strOption
    (Opt.long "addresses-to-index"
     <> Opt.metavar "Address"
     <> Opt.help ("White space separated list of addresses to index."
                  <>  " i.e \"address-1 address-2 address-3 ...\"" ) )

pMainnet :: Opt.Parser C.NetworkId
pMainnet = Opt.flag' C.Mainnet (Opt.long "mainnet" <> Opt.help "Use the mainnet magic id.")

pTestnetMagic :: Opt.Parser C.NetworkMagic
pTestnetMagic = C.NetworkMagic <$> Opt.option Opt.auto
    (Opt.long "testnet-magic"
     <> Opt.metavar "NATURAL"
     <> Opt.help "Specify a testnet magic id.")
