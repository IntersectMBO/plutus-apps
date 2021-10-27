
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
-- | Start a local cluster of cardano nodes and PAB(s)
module Main where

import qualified Cardano.Api                                as CAPI
import           Cardano.Api.NetworkId.Extra                (NetworkIdWrapper (..))
import qualified Cardano.BM.Backend.EKGView                 as EKG
import           Cardano.BM.Data.Severity                   (Severity (..))
import           Cardano.BM.Data.Tracer                     (HasPrivacyAnnotation (..), HasSeverityAnnotation (..))
import           Cardano.BM.Plugin                          (loadPlugin)
import           Cardano.BM.Setup                           (setupTrace_)
import           Cardano.BM.Trace                           (Trace)
import           Cardano.CLI                                (LogOutput (..), Port, ekgEnabled, getEKGURL,
                                                             getPrometheusURL, withLoggingNamed)
import qualified Cardano.ChainIndex.Types                   as PAB.CI
import           Cardano.Launcher.Node                      (nodeSocketFile)
import           Cardano.Mnemonic                           (SomeMnemonic (..))
import           Cardano.Node.Types                         (MockServerConfig (..), NodeMode (AlonzoNode))
import           Cardano.Startup                            (installSignalHandlers, setDefaultFilePermissions,
                                                             withUtf8Encoding)
import qualified Cardano.Wallet.Api.Client                  as WalletClient
import           Cardano.Wallet.Api.Server                  (Listen (..))
import           Cardano.Wallet.Api.Types                   (ApiMnemonicT (..), ApiT (..), ApiWallet (..),
                                                             EncodeAddress (..), WalletOrAccountPostData (..))
import qualified Cardano.Wallet.Api.Types                   as Wallet.Types
import           Cardano.Wallet.Logging                     (stdoutTextTracer, trMessageText)
import           Cardano.Wallet.Mock.Types                  (WalletUrl (..))
import qualified Cardano.Wallet.Mock.Types                  as Wallet.Config
import           Cardano.Wallet.Primitive.AddressDerivation (NetworkDiscriminant (..), Passphrase (..))
import           Cardano.Wallet.Primitive.SyncProgress      (SyncTolerance (..))
import           Cardano.Wallet.Primitive.Types             (WalletName (..))
import           Cardano.Wallet.Primitive.Types.Coin        (Coin (..))
import           Cardano.Wallet.Shelley                     (SomeNetworkDiscriminant (..), serveWallet, setupTracers,
                                                             tracerSeverities)
import           Cardano.Wallet.Shelley.Launch              (withSystemTempDir)
import           Cardano.Wallet.Shelley.Launch.Cluster      (ClusterLog (..), Credential (..), RunningNode (..),
                                                             localClusterConfigFromEnv, moveInstantaneousRewardsTo,
                                                             oneMillionAda, sendFaucetAssetsTo, sendFaucetFundsTo,
                                                             testMinSeverityFromEnv, tokenMetadataServerFromEnv,
                                                             walletMinSeverityFromEnv, withCluster)
import           ContractExample                            (ExampleContracts)
import           Control.Arrow                              (first)
import           Control.Concurrent                         (threadDelay)
import           Control.Concurrent.Async                   (async)
import           Control.Lens
import           Control.Monad                              (void, when)
import           Control.Tracer                             (traceWith)
import           Data.Default                               (Default (def))
import           Data.Proxy                                 (Proxy (..))
import           Data.String                                (IsString (..))
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           Data.Text.Class                            (ToText (..))
import           Network.HTTP.Client                        (defaultManagerSettings, newManager)
import qualified Plutus.ChainIndex.App                      as ChainIndex
import           Plutus.ChainIndex.ChainIndexLog            (ChainIndexLog)
import qualified Plutus.ChainIndex.Config                   as CI
import qualified Plutus.ChainIndex.Logging                  as ChainIndex.Logging
import           Plutus.PAB.App                             (StorageBackend (..))
import           Plutus.PAB.Effects.Contract.Builtin        (handleBuiltin)
import qualified Plutus.PAB.Run                             as PAB.Run
import           Plutus.PAB.Run.Command                     (ConfigCommand (..))
import           Plutus.PAB.Run.CommandParser               (AppOpts (..))
import qualified Plutus.PAB.Run.CommandParser               as PAB.Command
import           Plutus.PAB.Types                           (Config (..), DbConfig (..))
import qualified Plutus.PAB.Types                           as PAB.Config
import           Servant.Client                             (BaseUrl (..), Scheme (..), mkClientEnv, runClientM)
import           System.Directory                           (createDirectory)
import           System.FilePath                            ((</>))
import           Test.Integration.Faucet                    (genRewardAccounts, maryIntegrationTestAssets, mirMnemonics,
                                                             shelleyIntegrationTestFunds)
import qualified Test.Integration.Faucet                    as Faucet
import           Test.Integration.Framework.DSL             (fixturePassphrase)

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

main :: IO ()
main = withLocalClusterSetup $ \dir lo@LogOutputs{loCluster} ->
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

            setupPABServices walletHost walletPort dir rn

            ekgEnabled >>= flip when (EKG.plugin cfg tr sb >>= loadPlugin sb)

            let tracers = setupTracers (tracerSeverities (Just Debug)) tr
            let db = dir </> "wallets"
            createDirectory db
            tokenMetadataServer <- tokenMetadataServerFromEnv

            prometheusUrl <- (maybe "none"
                    (\(h, p) -> T.pack h <> ":" <> toText @(Port "Prometheus") p)
                )
                <$> getPrometheusURL
            ekgUrl <- (maybe "none"
                    (\(h, p) -> T.pack h <> ":" <> toText @(Port "EKG") p)
                )
                <$> getEKGURL

            void $ serveWallet
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
                socketPath
                block0
                (gp, vData)
                (\u -> traceWith trCluster $ MsgBaseUrl (T.pack . show $ u)
                    ekgUrl prometheusUrl)

newtype ChainIndexPort = ChainIndexPort Int

setupPABServices :: String -> Int -> FilePath -> RunningNode -> IO ()
setupPABServices walletHost walletPort dir rn = void $ async $ do -- TODO: better types for arguments
    walletUrl <- restoreWallets walletHost walletPort
    chainIndexPort <- launchChainIndex dir rn
    launchPAB fixturePassphrase dir walletUrl rn chainIndexPort

{-| Launch the chain index in a separate thread.
-}
launchChainIndex :: FilePath -> RunningNode -> IO ChainIndexPort
launchChainIndex dir (RunningNode socketPath _block0 (_gp, _vData)) = do
    config <- ChainIndex.Logging.defaultConfig
    (trace :: Trace IO ChainIndexLog, _) <- setupTrace_ config "chain-index"
    let dbPath = dir </> "chain-index.db"
        chainIndexConfig = CI.defaultConfig
                    & CI.socketPath .~ nodeSocketFile socketPath
                    & CI.dbPath .~ dbPath
                    & CI.networkId .~ CAPI.Mainnet
    void . async $ void $ ChainIndex.runMain trace chainIndexConfig
    return $ ChainIndexPort $ chainIndexConfig ^. CI.port

{-| Launch the PAB in a separate thread.
-}
launchPAB ::
    Text -> -- ^ Passphrase
    FilePath -> -- ^ Temp directory
    BaseUrl -> -- ^ wallet url
    RunningNode -> -- ^ Socket path
    ChainIndexPort -> -- ^ Port of the chain index
    IO ()
launchPAB passPhrase dir walletUrl (RunningNode socketPath _block0 (_gp, _vData)) (ChainIndexPort chainIndexPort) = do
    let opts = AppOpts{minLogLevel = Nothing, logConfigPath = Nothing, configPath = Nothing, runEkgServer = False, storageBackend = BeamSqliteBackend, cmd = PABWebserver, PAB.Command.passphrase = Just passPhrase}
        networkID = NetworkIdWrapper CAPI.Mainnet
        config =
            PAB.Config.defaultConfig
                { nodeServerConfig = def{mscSocketPath=nodeSocketFile socketPath,mscNodeMode=AlonzoNode,mscNetworkId=networkID}
                , dbConfig = def{dbConfigFile = T.pack (dir </> "plutus-pab.db")}
                , chainIndexConfig = def{PAB.CI.ciBaseUrl = PAB.CI.ChainIndexUrl $ BaseUrl Http "localhost" chainIndexPort ""}
                , walletServerConfig = def{Wallet.Config.baseUrl=WalletUrl walletUrl{baseUrlPath="/v2"}}
                }
    -- TODO: For some reason this has to be async - program terminates if it's done synchronously???
    void . async $ PAB.Run.runWithOpts @ExampleContracts handleBuiltin (Just config) opts{cmd=Migrate}
    sleep 2
    void . async $ PAB.Run.runWithOpts @ExampleContracts handleBuiltin (Just config) opts{cmd=PABWebserver}

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
    result <- flip runClientM clientEnv $ (WalletClient.postWallet WalletClient.walletClient) walletAcc
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
