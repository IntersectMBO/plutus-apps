{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
-- | Start a local cluster of cardano nodes and PAB(s)
module Plutus.PAB.LocalCluster.Run where

import Cardano.Api qualified as CAPI
import Cardano.Api.NetworkId.Extra (NetworkIdWrapper (NetworkIdWrapper))
import Cardano.BM.Backend.EKGView qualified as EKG
import Cardano.BM.Data.Severity (Severity (Notice))
import Cardano.BM.Data.Tracer (HasPrivacyAnnotation, HasSeverityAnnotation)
import Cardano.BM.Plugin (loadPlugin)
import Cardano.BM.Tracing (HasSeverityAnnotation (getSeverityAnnotation), Severity (Debug, Info))
import Cardano.CLI (LogOutput (LogToFile, LogToStdStreams), Port, ekgEnabled, getEKGURL, getPrometheusURL,
                    withLoggingNamed)
import Cardano.ChainIndex.Types qualified as PAB.CI
import Cardano.Launcher.Node (nodeSocketFile)
import Cardano.Mnemonic (SomeMnemonic (SomeMnemonic))
import Cardano.Node.Types (NodeMode (AlonzoNode),
                           PABServerConfig (pscKeptBlocks, pscNetworkId, pscNodeMode, pscSlotConfig, pscSocketPath))
import Cardano.Startup (installSignalHandlers, setDefaultFilePermissions, withUtf8Encoding)
import Cardano.Wallet.Api.Client qualified as WalletClient
import Cardano.Wallet.Api.Server (Listen (ListenOnPort))
import Cardano.Wallet.Api.Types (ApiMnemonicT (ApiMnemonicT), ApiT (ApiT), ApiWallet (ApiWallet),
                                 EncodeAddress (encodeAddress), WalletOrAccountPostData (WalletOrAccountPostData),
                                 postData)
import Cardano.Wallet.Api.Types qualified as Wallet.Types
import Cardano.Wallet.Logging (stdoutTextTracer, trMessageText)
import Cardano.Wallet.Primitive.AddressDerivation (NetworkDiscriminant (Mainnet), Passphrase (Passphrase))
import Cardano.Wallet.Primitive.SyncProgress (SyncTolerance (SyncTolerance))
import Cardano.Wallet.Primitive.Types (GenesisParameters (GenesisParameters),
                                       NetworkParameters (NetworkParameters, slottingParameters),
                                       SlotLength (SlotLength),
                                       SlottingParameters (SlottingParameters, getSecurityParameter),
                                       StartTime (StartTime), WalletName (WalletName))
import Cardano.Wallet.Primitive.Types.Coin (Coin (Coin))
import Cardano.Wallet.Shelley (SomeNetworkDiscriminant (SomeNetworkDiscriminant), serveWallet, setupTracers,
                               tracerSeverities)
import Cardano.Wallet.Shelley.BlockchainSource (BlockchainSource (NodeSource))
import Cardano.Wallet.Shelley.Launch (withSystemTempDir)
import Cardano.Wallet.Shelley.Launch.Cluster (ClusterLog, Credential (KeyCredential), RunningNode (RunningNode),
                                              localClusterConfigFromEnv, moveInstantaneousRewardsTo, oneMillionAda,
                                              sendFaucetAssetsTo, sendFaucetFundsTo, testMinSeverityFromEnv,
                                              tokenMetadataServerFromEnv, walletMinSeverityFromEnv, withCluster)
import Cardano.Wallet.Types (WalletUrl (WalletUrl))
import Cardano.Wallet.Types qualified as Wallet.Config
import Control.Arrow (first)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Lens (contramap, set, (&), (.~), (^.))
import Control.Monad (void, when)
import Control.Tracer (traceWith)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import Data.OpenApi.Schema qualified as OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Quantity (Quantity (getQuantity))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Class (ToText (toText))
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Ledger.TimeSlot (SlotConfig (SlotConfig))
import Ledger.TimeSlot qualified as TimeSlot
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Plutus.ChainIndex.App qualified as ChainIndex
import Plutus.ChainIndex.Config qualified as CI
import Plutus.ChainIndex.Logging qualified as ChainIndex.Logging
import Plutus.ChainIndex.Types (Point (..))
import Plutus.PAB.App (StorageBackend (BeamSqliteBackend))
import Plutus.PAB.Effects.Contract.Builtin (BuiltinHandler, HasDefinitions)
import Plutus.PAB.Run qualified as PAB.Run
import Plutus.PAB.Run.Command (ConfigCommand (Migrate, PABWebserver))
import Plutus.PAB.Run.CommandParser (AppOpts (AppOpts, cmd, configPath, logConfigPath, minLogLevel, resumeFrom, rollbackHistory, runEkgServer, storageBackend))
import Plutus.PAB.Run.CommandParser qualified as PAB.Command
import Plutus.PAB.Types (Config (chainIndexConfig, dbConfig, nodeServerConfig, walletServerConfig),
                         DbConfig (dbConfigFile))
import Plutus.PAB.Types qualified as PAB.Config
import Prettyprinter (Pretty)
import Servant qualified
import Servant.Client (BaseUrl (BaseUrl, baseUrlHost, baseUrlPath, baseUrlPort, baseUrlScheme), Scheme (Http),
                       mkClientEnv, runClientM)
import System.Directory (createDirectory)
import System.FilePath ((</>))
import Test.Integration.Faucet (genRewardAccounts, maryIntegrationTestAssets, mirMnemonics, shelleyIntegrationTestFunds)
import Test.Integration.Faucet qualified as Faucet
import Test.Integration.Framework.DSL (fixturePassphrase)

data LogOutputs =
    LogOutputs
        { loCluster :: [LogOutput]
        , loWallet  :: [LogOutput]
        }

-- Do all the program setup required for running the local cluster, create a
-- temporary directory, log output configurations, and pass these to the given
-- main action.
withLocalClusterSetup
    :: (FilePath -> LogOutputs -> IO a)
    -> IO a
withLocalClusterSetup action = do
    putStrLn "Starting PAB local cluster. Please make sure the SHELLEY_TEST_DATA environment variable is set to 'plutus-pab/local-cluster/cluster-data/cardano-node-shelley' in the plutus-apps repository."

    -- Handle SIGTERM properly
    installSignalHandlers (putStrLn "Terminated")

    -- Ensure key files have correct permissions for cardano-cli
    setDefaultFilePermissions

    -- Set UTF-8, regardless of user locale
    withUtf8Encoding $
        -- This temporary directory will contain logs, and all other data
        -- produced by the local test cluster.
        withSystemTempDir stdoutTextTracer "test-cluster" $ \dir -> do
            let logOutputs name minSev =
                    [ LogToFile (dir </> name) (min minSev Info)
                    , LogToStdStreams minSev ]

            lops <-
                LogOutputs
                    <$> (logOutputs "cluster.log" <$> testMinSeverityFromEnv)
                    <*> (logOutputs "wallet.log" <$> walletMinSeverityFromEnv)

            action dir lops

runWith :: forall a.
    ( Show a
    , Ord a
    , FromJSON a
    , ToJSON a
    , Pretty a
    , Servant.MimeUnrender Servant.JSON a
    , HasDefinitions a
    , OpenApi.ToSchema a
    )
    => BuiltinHandler a
    -> IO ()
runWith userContractHandler = withLocalClusterSetup $ \dir lo@LogOutputs{loCluster} ->
    withLoggingNamed "cluster" loCluster $ \(_, (_, trCluster)) -> do
        let tr' = contramap MsgCluster $ trMessageText trCluster
        clusterCfg <- localClusterConfigFromEnv
        withCluster tr' dir clusterCfg
            (setupFaucet dir (trMessageText trCluster))
            (whenReady dir (trMessageText trCluster) lo)
  where
    setupFaucet dir trCluster (RunningNode socketPath _ _) = do
        traceWith trCluster MsgSettingUpFaucet
        let trCluster' = contramap MsgCluster trCluster
        let encodeAddresses = map (first (T.unpack . encodeAddress @'Mainnet))
        let accts = KeyCredential <$> concatMap genRewardAccounts mirMnemonics
        let rewards = (, Coin $ fromIntegral oneMillionAda) <$> accts

        sendFaucetFundsTo trCluster' socketPath dir $
            encodeAddresses shelleyIntegrationTestFunds
        sendFaucetAssetsTo trCluster' socketPath dir 20 $ encodeAddresses $
            maryIntegrationTestAssets (Coin 1_000_000_000)
        moveInstantaneousRewardsTo trCluster' socketPath dir rewards

    whenReady dir trCluster LogOutputs{loWallet} rn@(RunningNode socketPath block0 (gp, vData)) = do
        withLoggingNamed "cardano-wallet" loWallet $ \(sb, (cfg, tr)) -> do
            let walletHost = "127.0.0.1"
                walletPort = 46493

            setupPABServices userContractHandler walletHost walletPort dir rn

            ekgEnabled >>= flip when (EKG.plugin cfg tr sb >>= loadPlugin sb)

            let tracers = setupTracers (tracerSeverities (Just Debug)) tr
            let db = dir </> "wallets"
            createDirectory db
            tokenMetadataServer <- tokenMetadataServerFromEnv

            prometheusUrl <- maybe "none"
                    (\(h, p) -> T.pack h <> ":" <> toText @(Port "Prometheus") p)
                <$> getPrometheusURL
            ekgUrl <- maybe "none"
                    (\(h, p) -> T.pack h <> ":" <> toText @(Port "EKG") p)
                <$> getEKGURL

            void $ serveWallet
                (NodeSource socketPath vData)
                gp
                (SomeNetworkDiscriminant $ Proxy @'Mainnet)
                tracers
                (SyncTolerance 10)
                (Just db)
                Nothing
                (fromString walletHost)
                (ListenOnPort walletPort)
                Nothing
                Nothing
                tokenMetadataServer
                block0
                (\u -> traceWith trCluster $ MsgBaseUrl (T.pack . show $ u)
                    ekgUrl prometheusUrl)

newtype ChainIndexPort = ChainIndexPort Int

setupPABServices
    :: forall a.
    ( Show a
    , Ord a
    , FromJSON a
    , ToJSON a
    , Pretty a
    , Servant.MimeUnrender Servant.JSON a
    , HasDefinitions a
    , OpenApi.ToSchema a
    )
    => BuiltinHandler a -> String -> Int -> FilePath -> RunningNode -> IO ()
setupPABServices userContractHandler walletHost walletPort dir rn = void $ async $ do -- TODO: better types for arguments
    walletUrl <- restoreWallets walletHost walletPort
    chainIndexPort <- launchChainIndex dir rn
    launchPAB userContractHandler fixturePassphrase dir walletUrl rn chainIndexPort

{-| Launch the chain index in a separate thread.
-}
launchChainIndex :: FilePath -> RunningNode -> IO ChainIndexPort
launchChainIndex dir (RunningNode socketPath _block0 (_gp, _vData)) = do
    config <- ChainIndex.Logging.defaultConfig
    let dbPath = dir </> "chain-index.db"
        chainIndexConfig = CI.defaultConfig
                    & CI.socketPath .~ nodeSocketFile socketPath
                    & CI.dbPath .~ dbPath
                    & CI.networkId .~ CAPI.Mainnet
    void . async $ void $ ChainIndex.runMain config chainIndexConfig
    return $ ChainIndexPort $ chainIndexConfig ^. CI.port

{-| Launch the PAB in a separate thread.
-}
launchPAB
    :: forall a.
    ( Show a
    , Ord a
    , FromJSON a
    , ToJSON a
    , Pretty a
    , Servant.MimeUnrender Servant.JSON a
    , HasDefinitions a
    , OpenApi.ToSchema a
    )
    => BuiltinHandler a
    -> Text -- ^ Passphrase
    -> FilePath -- ^ Temp directory
    -> BaseUrl -- ^ wallet url
    -> RunningNode -- ^ Socket path
    -> ChainIndexPort -- ^ Port of the chain index
    -> IO ()
launchPAB userContractHandler
    passPhrase
    dir
    walletUrl
    (RunningNode socketPath _block0 (networkParameters, _))
    (ChainIndexPort chainIndexPort) = do

    let opts = AppOpts{minLogLevel = Nothing, logConfigPath = Nothing, configPath = Nothing, rollbackHistory = Nothing, resumeFrom = PointAtGenesis, runEkgServer = False, storageBackend = BeamSqliteBackend, cmd = PABWebserver, PAB.Command.passphrase = Just passPhrase}
        networkID = NetworkIdWrapper CAPI.Mainnet
        -- TODO: Remove when PAB queries local node for slot config
        slotConfig = slotConfigOfNetworkParameters networkParameters
        -- TODO: Remove when PAB queries local node for security param
        securityParam = fromIntegral
                      $ getQuantity
                      $ getSecurityParameter
                      $ slottingParameters networkParameters
        config =
            PAB.Config.defaultConfig
                { nodeServerConfig = def
                    { pscSocketPath = nodeSocketFile socketPath
                    , pscNodeMode = AlonzoNode
                    , pscNetworkId = networkID
                    , pscSlotConfig = slotConfig
                    , pscKeptBlocks = securityParam
                    }
                , dbConfig = def{dbConfigFile = T.pack (dir </> "plutus-pab.db")}
                , chainIndexConfig = def{PAB.CI.ciBaseUrl = PAB.CI.ChainIndexUrl $ BaseUrl Http "localhost" chainIndexPort ""}
                , walletServerConfig = set (Wallet.Config.walletSettingsL . Wallet.Config.baseUrlL) (WalletUrl walletUrl) def
                }
    PAB.Run.runWithOpts userContractHandler (Just config) opts { cmd = Migrate }
    PAB.Run.runWithOpts userContractHandler (Just config) opts { cmd = PABWebserver }

slotConfigOfNetworkParameters :: NetworkParameters -> SlotConfig
slotConfigOfNetworkParameters
    (NetworkParameters
        (GenesisParameters _ (StartTime startUtcTime))
        (SlottingParameters (SlotLength nominalDiffTime) _ _ _) _) =
    SlotConfig (floor $ 1000 * nominalDiffTimeToSeconds nominalDiffTime) (TimeSlot.utcTimeToPOSIXTime startUtcTime)

{-| Set up wallets
-}
restoreWallets :: String -> Int -> IO BaseUrl
restoreWallets walletHost walletPort = do
    sleep 15
    manager <- newManager defaultManagerSettings
    let baseUrl = BaseUrl{baseUrlScheme=Http,baseUrlHost=walletHost,baseUrlPort=walletPort,baseUrlPath=""}
        clientEnv = mkClientEnv manager baseUrl
        mnemonic :: ApiMnemonicT '[15, 18, 21, 24] = ApiMnemonicT $ SomeMnemonic $ head Faucet.seqMnemonics
        wpData    = Wallet.Types.WalletPostData
                        Nothing
                        mnemonic
                        Nothing
                        (ApiT $ WalletName "plutus-wallet")
                        (ApiT $ Passphrase $ fromString $ T.unpack fixturePassphrase)
        walletAcc = WalletOrAccountPostData{postData=Left wpData}
    result <- flip runClientM clientEnv $ WalletClient.postWallet WalletClient.walletClient walletAcc
    case result of
        Left err -> do
            putStrLn "restoreWallet failed"
            putStrLn $ "Error: " <> show err
            putStrLn "restoreWallet: trying again in 30s"
            sleep 15
            restoreWallets walletHost walletPort
        Right (ApiWallet (ApiT i) _ _ _ _ _ _ _ _) -> do
            putStrLn $ "Restored wallet: " <> show i
            putStrLn $ "Passphrase: " <> T.unpack fixturePassphrase
            return baseUrl

sleep :: Int -> IO ()
sleep n = threadDelay $ n * 1_000_000


-- Logging

data TestsLog
    = MsgBaseUrl Text Text Text -- wallet url, ekg url, prometheus url
    | MsgSettingUpFaucet
    | MsgCluster ClusterLog
    deriving (Show)

instance ToText TestsLog where
    toText = \case
        MsgBaseUrl walletUrl ekgUrl prometheusUrl -> mconcat
            [ "Wallet url: " , walletUrl
            , ", EKG url: " , ekgUrl
            , ", Prometheus url:", prometheusUrl
            ]
        MsgSettingUpFaucet -> "Setting up faucet..."
        MsgCluster msg -> toText msg

instance HasPrivacyAnnotation TestsLog
instance HasSeverityAnnotation TestsLog where
    getSeverityAnnotation = \case
        MsgSettingUpFaucet -> Notice
        MsgBaseUrl {}      -> Notice
        MsgCluster msg     -> getSeverityAnnotation msg

