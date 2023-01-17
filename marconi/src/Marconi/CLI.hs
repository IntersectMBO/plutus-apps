{-# LANGUAGE PolyKinds #-}

module Marconi.CLI
    (chainPointParser
    , multiString
    , parseCardanoAddresses
    , pNetworkId
    , Options (..)
    , optionsParser
    , parseOptions
    , utxoDbPath
    , datumDbPath
    , scriptTxDbPath
    , epochStakepoolSizeDbPath
    ) where

import Control.Applicative (optional, some)
import Data.ByteString.Char8 qualified as C8
import Data.List (nub)
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text (pack)
import Options.Applicative qualified as Opt
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import Cardano.Api (ChainPoint, NetworkId)
import Cardano.Api qualified as C
import Marconi.Types (TargetAddresses)

chainPointParser :: Opt.Parser C.ChainPoint
chainPointParser =
  pure C.ChainPointAtGenesis
    Opt.<|> ( C.ChainPoint
            <$> Opt.option (C.SlotNo <$> Opt.auto) (Opt.long "slot-no"
                                                    <> Opt.short 'n'
                                                    <> Opt.metavar "SLOT-NO")
            <*> Opt.option
              (Opt.maybeReader maybeParseHashBlockHeader Opt.<|> Opt.readerError "Malformed block hash")
              (Opt.long "block-hash"
               <> Opt.short 'b'
               <> Opt.metavar "BLOCK-HASH")
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
multiString :: Opt.Mod Opt.OptionFields [C.Address C.ShelleyAddr] -> Opt.Parser TargetAddresses
multiString desc = fromList . concat <$> some single
  where
    single = Opt.option (Opt.str >>= (pure . parseCardanoAddresses)) desc

parseCardanoAddresses :: String -> [C.Address C.ShelleyAddr]
parseCardanoAddresses =  nub
    . fromJustWithError
    . traverse (deserializeToCardano . pack)
    . words
    where
        deserializeToCardano = C.deserialiseFromBech32 (C.proxyToAsType Proxy)


-- | This executable is meant to exercise a set of indexers (for now datumhash -> datum)
--     against the mainnet (meant to be used for testing).
--
--     In case you want to access the results of the datumhash indexer you need to query
--     the resulting database:
--     $ sqlite3 datums.sqlite
--     > select slotNo, datumHash, datum from kv_datumhsh_datum where slotNo = 39920450;
--     39920450|679a55b523ff8d61942b2583b76e5d49498468164802ef1ebe513c685d6fb5c2|X(002f9787436835852ea78d3c45fc3d436b324184

data Options = Options
  { optionsSocketPath           :: String,
    optionsNetworkId            :: NetworkId,
    optionsChainPoint           :: ChainPoint,
    optionsDbPath               :: FilePath,    -- ^ SQLite database directory path
    optionsDisableUtxo          :: Bool,
    optionsDisableDatum         :: Bool,
    optionsDisableScript        :: Bool,
    optionsDisableStakepoolSize :: Bool,
    optionsTargetAddresses      :: Maybe TargetAddresses,
    optionsNodeConfigPath       :: Maybe FilePath
  }
  deriving (Show)

parseOptions :: IO Options
parseOptions = do
    maybeSha <- lookupEnv "GITHUB_SHA"
    let sha = fromMaybe "GIHUB_SHA environment variable not set!" maybeSha
    Opt.execParser (programParser sha)
    where
        programParser sha =
            Opt.info (Opt.helper
                      <*> (versionOption sha)
                      <*> optionsParser)
            (Opt.fullDesc
                <> Opt.progDesc "marconi"
                <> Opt.header
                    "marconi - a lightweight customizable solution for indexing and querying the Cardano blockchain"
            )
        versionOption sha = Opt.infoOption sha (Opt.long "version" <> Opt.help "Show git SHA")

optionsParser :: Opt.Parser Options
optionsParser =
  Options
    <$> Opt.strOption (Opt.long "socket-path"
                       <> Opt.short 's'
                       <> Opt.help "Path to node socket.")
    <*> pNetworkId
    <*> chainPointParser
    <*> Opt.strOption (Opt.long "database-directory-path"
                      <> Opt.short 'd'
                      <> Opt.help "Dirctory Path for SQLite database.")
    <*> Opt.switch (Opt.long "disable-utxo"
                      <> Opt.help "disable utxo indexers."
                      <> Opt.showDefault
                     )
    <*> Opt.switch (Opt.long "disable-datum"
                      <> Opt.help "disable datum indexers."
                      <> Opt.showDefault
                     )
    <*> Opt.switch (Opt.long "disable-script-tx"
                      <> Opt.help "disable script-tx indexers."
                      <> Opt.showDefault
                     )
    <*> Opt.switch (Opt.long "disable-epoch-stakepool-size"
                      <> Opt.help "disable epoch stakepool size indexers."
                      <> Opt.showDefault
                     )
    <*> optAddressesParser (Opt.long "addresses-to-index"
                            <> Opt.short 'a'
                            <> Opt.help ("Becch32 Shelley addresses to index."
                                   <> " i.e \"--address-to-index address-1 --address-to-index address-2 ...\"" ) )
    <*> (optional $ Opt.strOption
         $ Opt.long "node-config-path"
          <> Opt.help "Path to node configuration which you are connecting to.")

optAddressesParser :: Opt.Mod Opt.OptionFields [C.Address C.ShelleyAddr] -> Opt.Parser (Maybe TargetAddresses)
optAddressesParser =  optional . multiString

-- * Database names and paths

utxoDbName :: FilePath
utxoDbName = "utxo.db"

datumDbName :: FilePath
datumDbName = "datum.db"

scriptTxDbName :: FilePath
scriptTxDbName = "scripttx.db"

epochStakepoolSizeDbName :: FilePath
epochStakepoolSizeDbName = "epochstakepool.db"

utxoDbPath :: Options -> Maybe FilePath
utxoDbPath o = if optionsDisableUtxo o then Nothing; else Just (optionsDbPath o </> utxoDbName)

datumDbPath :: Options -> Maybe FilePath
datumDbPath o = if optionsDisableDatum o then Nothing; else Just (optionsDbPath o </> datumDbName)

scriptTxDbPath :: Options -> Maybe FilePath
scriptTxDbPath o = if optionsDisableScript o then Nothing; else Just (optionsDbPath o </> scriptTxDbName)

epochStakepoolSizeDbPath :: Options -> Maybe FilePath
epochStakepoolSizeDbPath o = if optionsDisableStakepoolSize o then Nothing else Just (optionsDbPath o </> epochStakepoolSizeDbName)
