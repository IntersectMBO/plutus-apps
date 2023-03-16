{-# LANGUAGE PolyKinds #-}

module Marconi.ChainIndex.CLI where

import Control.Applicative (optional, some)
import Data.ByteString.Char8 qualified as C8
import Data.Functor ((<&>))
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
import Marconi.ChainIndex.Types (TargetAddresses, addressDatumDbName, datumDbName, epochStateDbName, mintBurnDbName,
                                 scriptTxDbName, utxoDbName)

chainPointParser :: Opt.Parser C.ChainPoint
chainPointParser =
  pure C.ChainPointAtGenesis Opt.<|> (C.ChainPoint <$> slotNoParser <*> blockHeaderHashParser)
  where
    blockHeaderHashParser :: Opt.Parser (C.Hash C.BlockHeader)
    blockHeaderHashParser =
        Opt.option
            (Opt.maybeReader maybeParseHashBlockHeader Opt.<|> Opt.readerError "Malformed block header hash")
            (  Opt.long "block-header-hash"
            <> Opt.short 'b'
            <> Opt.metavar "BLOCK-HEADER-HASH"
            )
    slotNoParser :: Opt.Parser C.SlotNo
    slotNoParser =
        Opt.option
            (C.SlotNo <$> Opt.auto)
            (  Opt.long "slot-no"
            <> Opt.short 'n'
            <> Opt.metavar "SLOT-NO"
            )
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
    single :: Opt.Parser [C.Address C.ShelleyAddr]
    single = Opt.option (Opt.str <&> parseCardanoAddresses) desc

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
  { optionsSocketPath          :: !String,
    optionsNetworkId           :: !NetworkId,
    optionsChainPoint          :: !ChainPoint,
    optionsDbPath              :: !FilePath,    -- ^ SQLite database directory path
    optionsDisableUtxo         :: !Bool,
    optionsDisableAddressDatum :: !Bool,
    optionsDisableDatum        :: !Bool,
    optionsDisableScript       :: !Bool,
    optionsDisableEpochState   :: !Bool,
    optionsDisableMintBurn     :: !Bool,
    optionsTargetAddresses     :: !(Maybe TargetAddresses),
    optionsNodeConfigPath      :: !(Maybe FilePath)
  }
  deriving (Show)

parseOptions :: IO Options
parseOptions = getGitSha >>= Opt.execParser . programParser

programParser :: String -> Opt.ParserInfo Options
programParser gitSha = Opt.info
  (Opt.helper <*> commonVersionOption gitSha <*> optionsParser)
  (marconiDescr "marconi")

optionsParser :: Opt.Parser Options
optionsParser =
  Options
    <$> commonSocketPath
    <*> pNetworkId
    <*> chainPointParser
    <*> commonDbDir
    <*> Opt.switch (  Opt.long "disable-utxo"
                   <> Opt.help "disable utxo indexers."
                   )
    <*> Opt.switch (  Opt.long "disable-address-datum"
                   <> Opt.help "disable address->datum indexers."
                   )
    <*> Opt.switch (  Opt.long "disable-datum"
                   <> Opt.help "disable datum indexers."
                   )
    <*> Opt.switch (  Opt.long "disable-script-tx"
                   <> Opt.help "disable script-tx indexers."
                   )
    <*> Opt.switch (  Opt.long "disable-epoch-stakepool-size"
                   <> Opt.help "disable epoch stakepool size indexers."
                   )
    <*> Opt.switch (  Opt.long "disable-mintburn"
                   <> Opt.help "disable mint/burn indexers."
                   )
    <*> commonMaybeTargetAddress
    <*> (optional
            $ Opt.strOption
            $ Opt.long "node-config-path"
                <> Opt.help "Path to node configuration which you are connecting to.")

optAddressesParser
    :: Opt.Mod Opt.OptionFields [C.Address C.ShelleyAddr]
    -> Opt.Parser (Maybe TargetAddresses)
optAddressesParser =  optional . multiString

-- * Database paths

utxoDbPath :: Options -> Maybe FilePath
utxoDbPath o =
    if optionsDisableUtxo o
       then Nothing
       else Just (optionsDbPath o </> utxoDbName)

addressDatumDbPath :: Options -> Maybe FilePath
addressDatumDbPath o =
    if optionsDisableAddressDatum o
       then Nothing
       else Just (optionsDbPath o </> addressDatumDbName)

datumDbPath :: Options -> Maybe FilePath
datumDbPath o =
    if optionsDisableDatum o
       then Nothing
       else Just (optionsDbPath o </> datumDbName)

scriptTxDbPath :: Options -> Maybe FilePath
scriptTxDbPath o =
    if optionsDisableScript o
       then Nothing
       else Just (optionsDbPath o </> scriptTxDbName)

epochStateDbPath :: Options -> Maybe FilePath
epochStateDbPath o =
    if optionsDisableEpochState o
       then Nothing
       else Just (optionsDbPath o </> epochStateDbName)

mintBurnDbPath :: Options -> Maybe FilePath
mintBurnDbPath o =
    if optionsDisableMintBurn o
       then Nothing
       else Just (optionsDbPath o </> mintBurnDbName)

-- * Common CLI parsers for other derived programs.

commonSocketPath :: Opt.Parser String
commonSocketPath = Opt.strOption $ Opt.long "socket-path"
  <> Opt.short 's'
  <> Opt.help "Path to node socket."
  <> Opt.metavar "FILE-PATH"

commonDbDir :: Opt.Parser String
commonDbDir = Opt.strOption
   $ Opt.short 'd'
  <> Opt.long "db-dir"
  <> Opt.metavar "DIR"
  <> Opt.help "Directory path where all SQLite databases are located."

commonVersionOption :: String -> Opt.Parser (a -> a)
commonVersionOption sha = Opt.infoOption sha $ Opt.long "version" <> Opt.help "Show git SHA"

getGitSha :: IO String
getGitSha = fromMaybe "GIHUB_SHA environment variable not set!" <$> lookupEnv "GITHUB_SHA"

marconiDescr :: String -> Opt.InfoMod a
marconiDescr programName = Opt.fullDesc
  <> Opt.progDesc programName
  <> Opt.header
  (programName <> " - a lightweight customizable solution for indexing and querying the Cardano blockchain")

commonMaybePort :: Opt.Parser (Maybe Int)
commonMaybePort = Opt.optional $ Opt.option Opt.auto
   $ Opt.long "http-port"
  <> Opt.metavar "HTTP-PORT"
  <> Opt.help "JSON-RPC http port number, default is port 3000."

commonMaybeTargetAddress :: Opt.Parser (Maybe TargetAddresses)
commonMaybeTargetAddress = Opt.optional $ multiString
   $ Opt.long "addresses-to-index"
  <> Opt.short 'a'
  <> Opt.metavar "BECH32-ADDRESS"
  <> Opt.help
     "Bech32 Shelley addresses to index. \
     \ i.e \"--address-to-index address-1 --address-to-index address-2 ...\""
