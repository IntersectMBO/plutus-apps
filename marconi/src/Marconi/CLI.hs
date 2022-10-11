module Marconi.CLI where

import Cardano.Api qualified as C
import Data.ByteString.Char8 qualified as C8
import Data.Proxy (Proxy (Proxy))
import Options.Applicative qualified as Opt

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
