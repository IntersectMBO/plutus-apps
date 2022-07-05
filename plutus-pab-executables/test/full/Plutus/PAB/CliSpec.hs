{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Plutus.PAB.CliSpec
    ( tests
    ) where

import Cardano.BM.Configuration.Model qualified as CM
import Cardano.BM.Data.Severity (Severity)
import Cardano.BM.Data.Trace (Trace)
import Cardano.BM.Setup (setupTrace_)
import Cardano.ChainIndex.Types qualified as ChainIndex.Types
import Cardano.Node.Types (NodeMode (AlonzoNode, MockNode))
import Cardano.Node.Types qualified as Node.Types
import Cardano.Wallet.Mock.Client qualified as Wallet.Client
import Cardano.Wallet.Mock.Types (WalletInfo (WalletInfo))
import Cardano.Wallet.Types qualified as Wallet.Types
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.Availability (available, newToken, starting)
import Control.Lens (over)
import Control.Monad (forM_, void, when)
import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Coerce (coerce)
import Data.Default (def)
import Data.Either (isLeft)
import Data.List (delete)
import Data.OpenApi.Schema qualified as OpenApi
import Data.Text qualified as Text
import Data.Yaml (decodeFileThrow)
import Database.SQLite.Simple qualified as Sqlite
import GHC.Generics (Generic)
import Ledger.Ada (lovelaceValueOf)
import Network.HTTP.Client (ManagerSettings (managerResponseTimeout), defaultManagerSettings, newManager,
                            responseTimeoutNone)
import Plutus.ChainIndex.Types (Point (..))
import Plutus.Contracts.PingPong qualified as PingPong
import Plutus.Monitoring.Util (PrettyObject, convertLog)
import Plutus.PAB.App (StorageBackend (BeamSqliteBackend))
import Plutus.PAB.App qualified as App
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler, HasDefinitions, SomeBuiltin (SomeBuiltin))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Monitoring.Config (defaultConfig)
import Plutus.PAB.Monitoring.Monitoring qualified as LM
import Plutus.PAB.Monitoring.PABLogMsg (AppMsg)
import Plutus.PAB.Run (runWithOpts)
import Plutus.PAB.Run.Cli (ConfigCommandArgs, runConfigCommand)
import Plutus.PAB.Run.Command (ConfigCommand (ChainIndex, ForkCommands, Migrate), allServices)
import Plutus.PAB.Run.CommandParser (AppOpts (AppOpts, cmd, configPath, logConfigPath, minLogLevel, passphrase, resumeFrom, rollbackHistory, runEkgServer, storageBackend))
import Plutus.PAB.Run.PSGenerator (HasPSTypes (psTypes))
import Plutus.PAB.Types (Config (Config, chainIndexConfig, dbConfig, nodeServerConfig, pabWebserverConfig, walletServerConfig),
                         DbConfig (..))
import Plutus.PAB.Types qualified as PAB.Types
import Plutus.PAB.Webserver.API (API)
import Plutus.PAB.Webserver.Client (InstanceClient (callInstanceEndpoint),
                                    PabClient (PabClient, activateContract, instanceClient), pabClient)
import Plutus.PAB.Webserver.Types (ContractActivationArgs (ContractActivationArgs, caID, caWallet))
import Prettyprinter (Pretty (pretty), viaShow)
import Servant ((:<|>))
import Servant qualified
import Servant.Client (BaseUrl (BaseUrl), ClientEnv, Scheme (Http), client, mkClientEnv, runClientM)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Wallet.Emulator.Wallet (Wallet, knownWallet)
import Wallet.Types (ContractInstanceId)

tests :: TestTree
tests =
  testGroup "Plutus.PAB.Run.Cli"
    [ restoreContractStateTests
    ]

data TestingContracts = PingPong
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance HasDefinitions TestingContracts where
  getDefinitions = [ PingPong ]
  getSchema _    = Builtin.endpointsToSchemas @PingPong.PingPongSchema
  getContract _  = SomeBuiltin PingPong.simplePingPong

instance HasPSTypes TestingContracts where
  psTypes = undefined

instance Pretty TestingContracts where
  pretty = viaShow

-- | A testing config that mostly relies on the defaults, but overrides some
-- settings to make sure that all the ports are in a similar space, and we
-- have a good delay for endpoint availability.
defaultPabConfig :: Config
defaultPabConfig
  = def
      -- Note: We rely on a large timeout here to wait for endpoints to be
      -- available (i.e. transactions to be completed).
      -- TODO: Note: If it exceeds 900, Hydra assumes the CI is unresponsive
      -- (not unreasonably...)
      { pabWebserverConfig = def { PAB.Types.endpointTimeout = Just 60 }
      , nodeServerConfig = def { Node.Types.pscSocketPath = "/tmp/node-server.sock" }
      }

-- | Bump all the default ports, and any other needed things so that we
-- can run two PABs side-by-side.
bumpConfig
  :: Int       -- ^ Bump to add to the ports. Make sure there is no overlap!
  -> Text.Text -- ^ In-memory database name
  -> Config    -- ^ Config to bump.
  -> Config    -- ^ Bumped config!
bumpConfig x dbName conf@Config{ pabWebserverConfig   = p@PAB.Types.WebserverConfig{PAB.Types.baseUrl=p_u}
                               , walletServerConfig
                               , nodeServerConfig     = n@Node.Types.PABServerConfig{Node.Types.pscBaseUrl=n_u,Node.Types.pscSocketPath=soc}
                               , chainIndexConfig     = c@ChainIndex.Types.ChainIndexConfig{ChainIndex.Types.ciBaseUrl=c_u}
                               , dbConfig             = db@PAB.Types.DbConfig{PAB.Types.dbConfigFile=dbFile}
                               } = newConf
  where
    bump (BaseUrl scheme url port path) = BaseUrl scheme url (port + x) path
    newConf
      = conf { pabWebserverConfig   = p { PAB.Types.baseUrl          = bump p_u }
             , walletServerConfig   = over (Wallet.Types.walletSettingsL . Wallet.Types.baseUrlL) (coerce . bump . coerce) walletServerConfig
             , nodeServerConfig     = n { Node.Types.pscBaseUrl      = bump n_u, Node.Types.pscSocketPath = soc ++ "." ++ show x }
             , chainIndexConfig     = c { ChainIndex.Types.ciBaseUrl = coerce $ bump $ coerce c_u }
             , dbConfig             = db { PAB.Types.dbConfigFile    = "file::" <> dbName <> "?mode=memory&cache=shared" }
             }

startPab :: ConfigCommand -> Config -> IO ()
startPab services pabConfig = do
  let handler = Builtin.handleBuiltin @TestingContracts
      opts = AppOpts
              { minLogLevel = Nothing
              , logConfigPath = Nothing
              , configPath = Nothing
              , passphrase = Nothing
              , rollbackHistory = Nothing
              , resumeFrom = PointAtGenesis
              , runEkgServer = False
              , storageBackend = BeamSqliteBackend
              , cmd = services
              }

  let mc = Just pabConfig
  -- First, migrate.
  void . async $ runWithOpts handler mc (opts {cmd = Migrate})
  sleep 1

  -- Then, spin up the services.
  void . async $ runWithOpts handler mc opts
  sleep 5


-- | Make a config from the 'bumped' one that re-uses some of the services
-- from the primary one (the ones we're not starting).
secondaryConfig :: Config -> Config -> Config
secondaryConfig primary other =
  other { chainIndexConfig = chainIndexConfig primary
        }

startPrimaryPab :: Config -> IO ()
startPrimaryPab = startPab allServices

startSecondaryPab :: Config -> IO ()
startSecondaryPab = startPab $ ForkCommands (delete ChainIndex services)
  where
    ForkCommands services = allServices

sleep :: Int -> IO ()
sleep n = threadDelay $ n * 1_000_000

getClientEnv :: Config -> IO ClientEnv
getClientEnv pabConfig = do
  manager <- newManager $ defaultManagerSettings { managerResponseTimeout = responseTimeoutNone }

  let newApiUrl = PAB.Types.baseUrl (pabWebserverConfig pabConfig)

  pure $ mkClientEnv manager newApiUrl

startPingPongContract :: Config -> IO ContractInstanceId
startPingPongContract pabConfig = do
  apiClientEnv <- getClientEnv pabConfig

  let ca = ContractActivationArgs
                { caID     = PingPong
                , caWallet = Just (knownWallet 1)
                }

  let PabClient{activateContract} = pabClient @TestingContracts @Integer

  eci <- runClientM (activateContract ca) apiClientEnv

  case eci of
    Left e   -> error $ "Error starting contract: " <> show e
    Right ci -> pure ci

-- | Tag whether or not we expect the calls to succeed.
data EndpointCall = Succeed String
                  | Fail String
ep :: EndpointCall -> String
ep (Succeed s) = s
ep (Fail s)    = s

runPabInstanceEndpoints :: Config -> ContractInstanceId -> [EndpointCall] -> IO ()
runPabInstanceEndpoints pabConfig instanceId endpoints = do
  apiClientEnv <- getClientEnv pabConfig

  let PabClient{activateContract, instanceClient} = pabClient @TestingContracts @Integer
      callEndpoint = callInstanceEndpoint . instanceClient $ instanceId

  forM_ endpoints $ \e -> do
    x <- runClientM (callEndpoint (ep e) (toJSON ())) apiClientEnv
    case e of
      Succeed _ -> do
          assertEqual "Got the wrong thing back from the API" (Right ()) x
      Fail _ -> do
          assertBool "Endpoint call succeeded (it should've failed.)" (isLeft x)

{- Note [pab-ports]

The tests below run several PABs simultaneously. As a result, we need to make
sure that the port allocations don't overlap. The function 'bumpConfig' bumps
the ports (and the socket path) so that this overlap doesn't occur. It does
mean that you need to be a bit mindful of the magic numbers that are used in
the 'restoreContractStateTests' below, and ensure that there won't ever be an
overlap with the numbers that are used if they are all running at the same
time.

-}

restoreContractStateTests :: TestTree
restoreContractStateTests =
  let dbPath = Text.unpack . dbConfigFile . dbConfig in
  testGroup "restoreContractState scenarios"
    [ testCase "Can init,pong,ping in one PAB instance" $ do
        -- This isn't testing anything related to restoring state; but simply
        -- provides evidence that if the subsequent tests _fail_, then that is
        -- an genuine error.
        let pabConfig = defaultPabConfig

        -- We use 'withConnection' here and in the tests below
        -- to keep the in-memory sqlite db, otherwise the pool
        -- closes the connection and the db gets destroyed
        Sqlite.withConnection (dbPath pabConfig) $ \_ -> do
          startPrimaryPab pabConfig
          ci <- startPingPongContract pabConfig

          runPabInstanceEndpoints pabConfig ci (map Succeed ["initialise", "pong", "ping"])

    , testCase "PingPong contract state is maintained across PAB instances" $ do
        -- We'll check the following: Init, Pong, <STOP>, <RESTART>, Ping works.
        let pabConfig = bumpConfig 50 "db1" defaultPabConfig
        Sqlite.withConnection (dbPath pabConfig) $ \_ -> do
          startPrimaryPab pabConfig
          ci <- startPingPongContract pabConfig

          -- Run init, pong on one pab
          runPabInstanceEndpoints pabConfig ci (map Succeed ["initialise", "pong"])

          -- Then, check 'ping' works on a different PAB instance (that will
          -- have restored from the same DB.)
          let newConfig = bumpConfig 10 "db1" pabConfig
          startSecondaryPab (secondaryConfig pabConfig newConfig)

          runPabInstanceEndpoints newConfig ci [Succeed "ping"]

    , testCase "PingPong contract state is NOT maintained across PAB instances with different dbs" $ do
        -- Note: We bump the ports by 100 here because the two calls above.
        -- This should mean that no matter the order of these tests, there
        -- will be no clashes.
        let pabConfig = bumpConfig 100 "db2" defaultPabConfig
        Sqlite.withConnection (dbPath pabConfig) $ \_ -> do
          startPrimaryPab pabConfig
          ci <- startPingPongContract pabConfig

          -- Run init, pong on one pab
          runPabInstanceEndpoints pabConfig ci (map Succeed ["initialise", "pong"])

          -- This time, "ping" should fail because we're using a different
          -- in-memory db.
          let newConfig = bumpConfig 10 "db3" pabConfig
          startSecondaryPab (secondaryConfig pabConfig newConfig)

          runPabInstanceEndpoints newConfig ci [Fail "ping"]
    ]
