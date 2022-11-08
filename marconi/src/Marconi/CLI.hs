module Marconi.CLI
    ( multiString
    , chainPointParser
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


-- | parses CLI params to valid NonEmpty list of Shelley addresses
-- We error out if there are any invalid addresses
multiString :: Opt.Mod Opt.OptionFields [CardanoAddress] -> Opt.Parser TargetAddresses
multiString desc = fromList . concat <$> some single
  where
    single = Opt.option (Opt.str >>= parseStringList) desc

parseStringList :: Monad m => String -> m [CardanoAddress]
parseStringList = return
    . nub
    . fromJustWithError
    . traverse (deserializeToCardano . pack)
    . words
    where
        deserializeToCardano = C.deserialiseFromBech32 (C.proxyToAsType Proxy)
