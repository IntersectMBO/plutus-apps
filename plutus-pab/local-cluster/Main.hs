{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}
-- | Start a local cluster of cardano nodes and PAB(s)
module Main where

import           Cardano.BM.Data.Severity                   (Severity (..))
import           Cardano.BM.Data.Tracer                     (HasPrivacyAnnotation (..), HasSeverityAnnotation (..))
import           Cardano.BM.Plugin                          (loadPlugin)
import           Cardano.CLI                                (LogOutput (..), Port, ekgEnabled, getEKGURL,
                                                             getPrometheusURL, withLoggingNamed)
import           Cardano.Startup                            (installSignalHandlers, setDefaultFilePermissions,
                                                             withUtf8Encoding)
import           Cardano.Wallet.Api.Types                   (EncodeAddress (..))
import           Cardano.Wallet.Logging                     (stdoutTextTracer, trMessageText)
import           Cardano.Wallet.Primitive.AddressDerivation (NetworkDiscriminant (..))
import           Cardano.Wallet.Primitive.SyncProgress      (SyncTolerance (..))
import           Cardano.Wallet.Primitive.Types.Coin        (Coin (..))
import           Cardano.Wallet.Shelley                     (SomeNetworkDiscriminant (..), serveWallet, setupTracers,
                                                             tracerSeverities)
import           Cardano.Wallet.Shelley.Launch              (withSystemTempDir)
import           Cardano.Wallet.Shelley.Launch.Cluster      (ClusterLog (..), Credential (..), RunningNode (..),
                                                             localClusterConfigFromEnv, moveInstantaneousRewardsTo,
                                                             oneMillionAda, sendFaucetAssetsTo, sendFaucetFundsTo,
                                                             testMinSeverityFromEnv, tokenMetadataServerFromEnv,
                                                             walletListenFromEnv, walletMinSeverityFromEnv, withCluster)
import           Control.Arrow                              (first)
import           Control.Monad                              (void, when)
import           Control.Tracer                             (contramap, traceWith)
import           Data.Proxy                                 (Proxy (..))
import           Data.Text                                  (Text)
import           Data.Text.Class                            (ToText (..))
import           System.Directory                           (createDirectory)
import           System.FilePath                            ((</>))
import           Test.Integration.Faucet                    (genRewardAccounts, maryIntegrationTestAssets, mirMnemonics,
                                                             shelleyIntegrationTestFunds)

import qualified Cardano.BM.Backend.EKGView                 as EKG
import qualified Data.Text                                  as T

-- Do all the program setup required for running the local cluster, create a
-- temporary directory, log output configurations, and pass these to the given
-- main action.
withLocalClusterSetup
    :: (FilePath -> [LogOutput] -> [LogOutput] -> IO a)
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

            clusterLogs <- logOutputs "cluster.log" <$> testMinSeverityFromEnv
            walletLogs <- logOutputs "wallet.log" <$> walletMinSeverityFromEnv

            action dir clusterLogs walletLogs
main :: IO ()
main = withLocalClusterSetup $ \dir clusterLogs walletLogs ->
    withLoggingNamed "cluster" clusterLogs $ \(_, (_, trCluster)) -> do
        let tr' = contramap MsgCluster $ trMessageText trCluster
        clusterCfg <- localClusterConfigFromEnv
        withCluster tr' dir clusterCfg
            (setupFaucet dir (trMessageText trCluster))
            (whenReady dir (trMessageText trCluster) walletLogs)
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

    whenReady dir trCluster logs (RunningNode socketPath block0 (gp, vData)) =
        withLoggingNamed "cardano-wallet" logs $ \(sb, (cfg, tr)) -> do

            ekgEnabled >>= flip when (EKG.plugin cfg tr sb >>= loadPlugin sb)

            let tracers = setupTracers (tracerSeverities (Just Debug)) tr
            let db = dir </> "wallets"
            createDirectory db
            listen <- walletListenFromEnv
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
                "127.0.0.1"
                listen
                Nothing
                Nothing
                tokenMetadataServer
                socketPath
                block0
                (gp, vData)
                (\u -> traceWith trCluster $ MsgBaseUrl (T.pack . show $ u)
                    ekgUrl prometheusUrl)

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
