{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Plutus.PAB.Run.CommandParser (parseOptions, AppOpts(..)) where

import Cardano.Api (ChainPoint (..), deserialiseFromRawBytesHex, proxyToAsType)
import Cardano.BM.Data.Severity (Severity (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.List (elemIndex)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative (CommandFields, Mod, Parser, ReadM, argument, auto, command, customExecParser, disambiguate,
                            eitherReader, flag, fullDesc, help, helper, idm, info, long, metavar, option, prefs,
                            progDesc, short, showHelpOnEmpty, showHelpOnError, str, subparser, value)
import Text.Read (readEither)
import Wallet.Types (ContractInstanceId (..))

import Plutus.ChainIndex.Compatibility (fromCardanoPoint)
import Plutus.ChainIndex.Types (Point (..))
import Plutus.PAB.App (StorageBackend (..))
import Plutus.PAB.Run.Command

data AppOpts = AppOpts { minLogLevel     :: Maybe Severity
                       , logConfigPath   :: Maybe FilePath
                       , configPath      :: Maybe FilePath
                       , passphrase      :: Maybe Text
                       , rollbackHistory :: Maybe Int
                       , resumeFrom      :: Point
                       , runEkgServer    :: Bool
                       , storageBackend  :: StorageBackend
                       , cmd             :: ConfigCommand
                       }

parseOptions :: IO AppOpts
parseOptions = customExecParser
            (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
            (info (helper <*> commandLineParser) idm)

logLevelFlag :: Parser (Maybe Severity)
logLevelFlag =
    flag
        Nothing
        (Just Debug)
        (short 'v' <> long "verbose" <> help "Enable debugging output.")

ekgFlag :: Parser Bool
ekgFlag =
    flag
        False
        True
        (short 'e' <> long "ekg" <> help "Enable the EKG server")

inMemoryFlag :: Parser StorageBackend
inMemoryFlag =
    flag
        BeamBackend
        InMemoryBackend
        (short 'm' <> long "memory" <> help "Use the memory-backed backend. If false, the beam backend is used.")

commandLineParser :: Parser AppOpts
commandLineParser =
        AppOpts <$> logLevelFlag
                <*> logConfigFileParser
                <*> configFileParser
                <*> passphraseParser
                <*> rollbackHistoryParser
                <*> chainPointParser
                <*> ekgFlag
                <*> inMemoryFlag
                <*> commandParser

configFileParser :: Parser (Maybe FilePath)
configFileParser =
    option
        (Just <$> str)
        (long "config" <>
         metavar "CONFIG_FILE" <>
         help "Config file location." <> value Nothing)

logConfigFileParser :: Parser (Maybe FilePath)
logConfigFileParser =
    option
        (Just <$> str)
        (long "log-config" <>
         metavar "LOG_CONFIG_FILE" <>
         help "Logging config file location." <> value Nothing)

chainPointReader :: ReadM Point
chainPointReader = eitherReader $
  \chainPoint -> do
    idx <- maybeToRight ("Failed to parse chain point specification. The format" <>
                         "should be HASH,SLOT") $
                        elemIndex ',' chainPoint
    let (hash, slot') = splitAt idx chainPoint
    slot <- readEither (drop 1 slot')
    hsh  <- mapLeft (const $ "Failed to parse hash " <> hash) $
        deserialiseFromRawBytesHex (proxyToAsType Proxy) (encodeUtf8 $ pack hash)
    pure $ fromCardanoPoint $ ChainPoint (SlotNo slot) hsh

chainPointParser :: Parser Point
chainPointParser =
    option chainPointReader
        (  metavar "HASH,SLOT"
        <> long "resume-from"
        <> help "Specify the hash and the slot where to start synchronisation"
        <> value PointAtGenesis )

passphraseParser :: Parser (Maybe Text)
passphraseParser =
    option
        (Just <$> str)
        (long "passphrase" <>
         metavar "WALLET_PASSPHRASE" <>
         help "Wallet passphrase." <> value Nothing)

{- Limit the number of blocks that we store for rollbacks. This options helps
   with the memory usage of the PAB and should be removed when we figure out
   an alternative to storing the UTXO in memory.
-}
rollbackHistoryParser :: Parser (Maybe Int)
rollbackHistoryParser =
    option
        (Just <$> auto)
        (long "rollback-history" <>
         metavar "ROLLBACK_HISTORY" <>
         help "How many blocks are remembered when rolling back" <>
         value Nothing)

commandParser :: Parser ConfigCommand
commandParser =
    subparser $
    mconcat
        [ migrationParser
        , allServersParser
        , clientServicesParser
        , mockWalletParser
        , pabWebserverParser
        , mockNodeParser
        , chainIndexParser
        , command
              "contracts"
              (info
                   (subparser
                        (mconcat
                             [ reportActiveContractsParser
                             , contractStateParser
                             , reportContractHistoryParser
                             , reportAvailableContractsParser
                             ]))
                   (fullDesc <> progDesc "Manage your smart contracts."))
        ]

migrationParser :: Mod CommandFields ConfigCommand
migrationParser =
    command "migrate" $
    flip info (fullDesc <> progDesc "Update the database with the latest schema.") $
        pure Migrate

mockNodeParser :: Mod CommandFields ConfigCommand
mockNodeParser =
    command "node-server" $
        info
            (pure StartNode)
            (fullDesc <> progDesc "Run a mock version of the Cardano node API server.")

mockWalletParser :: Mod CommandFields ConfigCommand
mockWalletParser =
    command "wallet-server" $
    info
        (pure MockWallet)
        (fullDesc <>
         progDesc "Run a mock version of the Cardano wallet API server.")

chainIndexParser :: Mod CommandFields ConfigCommand
chainIndexParser =
    command "chain-index" $
    info (pure ChainIndex) (fullDesc <> progDesc "Run the chain index.")

allServersParser :: Mod CommandFields ConfigCommand
allServersParser =
    command "all-servers" $
    flip info (fullDesc <> progDesc "Run all the mock servers needed.") $ do
        pure  (ForkCommands
                   [ StartNode
                   , MockWallet
                   , PABWebserver
                   , ChainIndex
                   ])

clientServicesParser :: Mod CommandFields ConfigCommand
clientServicesParser =
    command "client-services" $
    info
        (pure (ForkCommands
                    [ MockWallet
                    , PABWebserver
                    , ChainIndex
                    ]))
        (fullDesc <> progDesc "Run the client services (all services except the mock node).")

contractStateParser :: Mod CommandFields ConfigCommand
contractStateParser =
    command "state" $
    info
        (ContractState <$> contractIdParser)
        (fullDesc <> progDesc "Show the current state of a contract.")

contractIdParser :: Parser ContractInstanceId
contractIdParser = fmap ContractInstanceId $
    argument
        auto
        (help "ID of the contract. (See 'active-contracts' for a list.)")

reportAvailableContractsParser :: Mod CommandFields ConfigCommand
reportAvailableContractsParser =
    command "available" $
    info
        (pure ReportAvailableContracts)
        (fullDesc <> progDesc "Show all available contracts.")

reportActiveContractsParser :: Mod CommandFields ConfigCommand
reportActiveContractsParser =
    command "active" $
    info
        (pure ReportActiveContracts)
        (fullDesc <> progDesc "Show all active contracts.")

pabWebserverParser :: Mod CommandFields ConfigCommand
pabWebserverParser =
    command "webserver" $
    info
        (pure PABWebserver)
        (fullDesc <> progDesc "Start the PAB backend webserver.")

reportContractHistoryParser :: Mod CommandFields ConfigCommand
reportContractHistoryParser =
    command "history" $
    info
        (ReportContractHistory <$> contractIdParser)
        (fullDesc <> progDesc "Show the state history of a smart contract.")

