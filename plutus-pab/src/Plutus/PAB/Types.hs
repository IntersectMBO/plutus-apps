{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}

module Plutus.PAB.Types where

import Cardano.ChainIndex.Types qualified as ChainIndex
import Cardano.Node.Types (PABServerConfig)
import Cardano.Wallet.Types qualified as Wallet
import Control.Lens.TH (makePrisms)
import Control.Monad.Freer.Extras.Beam (BeamError)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default, def)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time.Units (Second)
import Data.UUID (UUID)
import Data.UUID.Extras qualified as UUID
import Database.SQLite.Simple qualified as Sqlite
import GHC.Generics (Generic)
import Ledger (Block, Blockchain, Tx, TxId, eitherTx, txId)
import Ledger.Index (UtxoIndex (UtxoIndex))
import Ledger.Index qualified as UtxoIndex
import Plutus.ChainIndex.Types (Point (..))
import Plutus.Contract.Types (ContractError)
import Plutus.PAB.Db.FS.Types (ContractStoreDir)
import Plutus.PAB.Db.Memory.Types (InMemInstances)
import Plutus.PAB.Instances ()
import Prettyprinter (Pretty, line, pretty, viaShow, (<+>))
import Servant.Client (BaseUrl (BaseUrl), ClientError, Scheme (Http))
import Wallet.API (WalletAPIError)
import Wallet.Emulator.Wallet (Wallet)
import Wallet.Types (ContractInstanceId (ContractInstanceId), NotificationError)

data PABError
    = FileNotFound FilePath
    | ContractNotFound FilePath
    | ContractInstanceNotFound ContractInstanceId
    | PABContractError ContractError
    | WalletClientError ClientError
    | NodeClientError ClientError
    | BeamEffectError BeamError
    | FSContractStoreError String
    | RandomTxClientError ClientError
    | ChainIndexError ClientError
    | WalletError WalletAPIError
    | ContractCommandError Int Text -- ?
    | InvalidUUIDError  Text
    | OtherError Text -- ?
    | EndpointCallError NotificationError
    | InstanceAlreadyStopped ContractInstanceId -- ^ Attempt to stop the instance failed because it was not running
    | WalletNotFound Wallet
    | MissingConfigFileOption
    | ContractStateNotFound ContractInstanceId
    | AesonDecodingError Text Text
    | MigrationNotDoneError Text
    | RemoteWalletWithMockNodeError
    | TxSenderNotAvailable
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty PABError where
    pretty = \case
        FileNotFound fp            -> "File not found:" <+> pretty fp
        ContractNotFound fp        -> "Contract not found:" <+> pretty fp
        ContractInstanceNotFound i -> "Contract instance not found:" <+> pretty i
        FSContractStoreError msg   -> "FS contract store error:" <+> pretty msg
        PABContractError e         -> "Contract error:" <+> pretty e
        WalletClientError e        -> "Wallet client error:" <+> viaShow e
        NodeClientError e          -> "Node client error:" <+> viaShow e
        BeamEffectError e          -> "Beam effect error:" <+> viaShow e
        RandomTxClientError e      -> "Random tx client error:" <+> viaShow e
        ChainIndexError e          -> "Chain index error:" <+> viaShow e
        WalletError e              -> "Wallet error:" <+> pretty e
        ContractCommandError i t   -> "Contract command error:" <+> pretty i <+> pretty t
        InvalidUUIDError t         -> "Invalid UUID:" <+> pretty t
        OtherError t               -> "Other error:" <+> pretty t
        EndpointCallError n        -> "Endpoint call failed:" <+> pretty n
        InstanceAlreadyStopped i   -> "Instance already stopped:" <+> pretty i
        WalletNotFound w           -> "Wallet not found:" <+> pretty w
        MissingConfigFileOption    -> "The --config option is required"
        ContractStateNotFound i    -> "State for contract instance not found:" <+> pretty i
        AesonDecodingError msg o   -> "Error while Aeson decoding: " <+> pretty msg <+> pretty o
        MigrationNotDoneError msg  -> pretty msg
                                   <> line
                                   <> "Did you forget to run the 'migrate' command ?"
                                   <+> "(ex. 'plutus-pab-migrate' or 'plutus-pab-examples --config <CONFIG_FILE> migrate')"
        RemoteWalletWithMockNodeError   -> "The remote wallet can't be used with the mock node."
        TxSenderNotAvailable         -> "Cannot send a transaction when connected to the real node."

data SqliteConfig = SqliteConfig
    { sqliteConfigFile     :: Text
    -- ^ The path to the sqlite database file. May be absolute or relative.
    , sqliteConfigPoolSize :: Int
    -- ^ Max number of concurrent sqlite database connections.
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ContractStoreConfig
  = UseInMemoryStore
  | UseFSStore FilePath
  | UseSqliteStore SqliteConfig
     deriving (Show, Eq, Generic)
     deriving anyclass (ToJSON, FromJSON)

type SqliteConnectionPool = Pool Sqlite.Connection

data ContractStoreBackend t
  = InMemoryContractStore (InMemInstances t)
  | SqliteContractStore SqliteConnectionPool
  | FSContractStore (ContractStoreDir t)

-- Default contract store config uses an in-memory sqlite database that is shared
-- between all threads in the process.
defaultContractStoreConfig :: ContractStoreConfig
defaultContractStoreConfig = UseInMemoryStore

instance Default ContractStoreConfig where
  def = defaultContractStoreConfig

data Config =
    Config
        { contractStoreConfig     :: ContractStoreConfig
        , walletServerConfig      :: Wallet.WalletConfig
        , nodeServerConfig        :: PABServerConfig
        , pabWebserverConfig      :: WebserverConfig
        , chainIndexConfig        :: ChainIndex.ChainIndexConfig
        , requestProcessingConfig :: RequestProcessingConfig
        , developmentOptions      :: DevelopmentOptions
        }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

defaultConfig :: Config
defaultConfig =
  Config
    { contractStoreConfig = def
    , walletServerConfig = def
    , nodeServerConfig = def
    , pabWebserverConfig = def
    , chainIndexConfig = def
    , requestProcessingConfig = def
    , developmentOptions = def
    }

instance Default Config where
  def = defaultConfig

newtype RequestProcessingConfig =
    RequestProcessingConfig
        { requestProcessingInterval :: Second -- ^ How many seconds to wait between calls to 'Plutus.PAB.Core.ContractInstance.processAllContractOutboxes'
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

defaultRequestProcessingConfig :: RequestProcessingConfig
defaultRequestProcessingConfig =
  RequestProcessingConfig
    { requestProcessingInterval = 1
    }

instance Default RequestProcessingConfig where
  def = defaultRequestProcessingConfig

data WebserverConfig =
    WebserverConfig
        { baseUrl              :: BaseUrl
        , staticDir            :: Maybe FilePath
        , permissiveCorsPolicy :: Bool -- ^ If true; use a very permissive CORS policy (any website can interact.)
        , endpointTimeout      :: Maybe Second
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Default config for debugging.
defaultWebServerConfig :: WebserverConfig
defaultWebServerConfig =
  WebserverConfig
    -- See Note [pab-ports] in test/full/Plutus/PAB/CliSpec.hs.
    { baseUrl              = BaseUrl Http "localhost" 9080 ""
    , staticDir            = Nothing
    , permissiveCorsPolicy = False
    , endpointTimeout      = Nothing
    }

instance Default WebserverConfig where
  def = defaultWebServerConfig

data DevelopmentOptions =
    DevelopmentOptions
        { pabRollbackHistory :: Maybe Int
        , pabResumeFrom      :: Point
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

defaultDevelopmentOptions :: DevelopmentOptions
defaultDevelopmentOptions =
    DevelopmentOptions
        { pabRollbackHistory = Nothing
        , pabResumeFrom      = PointAtGenesis
        }

instance Default DevelopmentOptions where
    def = defaultDevelopmentOptions

-- | The source of a PAB event, used for sharding of the event stream
data Source
    = PABEventSource
    | InstanceEventSource ContractInstanceId
    deriving (Show, Eq)

toUUID :: Source -> UUID
toUUID = \case
    InstanceEventSource (ContractInstanceId i) -> i
    PABEventSource                             -> UUID.sequenceIdToMockUUID 1

data ChainOverview =
    ChainOverview
        { chainOverviewBlockchain     :: Blockchain
        , chainOverviewUnspentTxsById :: Map TxId Tx
        , chainOverviewUtxoIndex      :: UtxoIndex
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

mkChainOverview :: Blockchain -> ChainOverview
mkChainOverview = foldl reducer emptyChainOverview
  where
    reducer :: ChainOverview -> Block -> ChainOverview
    reducer ChainOverview { chainOverviewBlockchain = oldBlockchain
                          , chainOverviewUnspentTxsById = oldTxById
                          , chainOverviewUtxoIndex = oldUtxoIndex
                          } txs =
        let unprunedTxById =
                foldl (\m -> eitherTx (const m) (\tx -> Map.insert (txId tx) tx m)) oldTxById txs
            newTxById = unprunedTxById -- TODO Prune spent keys.
            newUtxoIndex = UtxoIndex.insertBlock txs oldUtxoIndex
         in ChainOverview
                { chainOverviewBlockchain = txs : oldBlockchain
                , chainOverviewUnspentTxsById = newTxById
                , chainOverviewUtxoIndex = newUtxoIndex
                }
    emptyChainOverview =
        ChainOverview
            { chainOverviewBlockchain = []
            , chainOverviewUnspentTxsById = Map.empty
            , chainOverviewUtxoIndex = UtxoIndex Map.empty
            }

makePrisms ''PABError
