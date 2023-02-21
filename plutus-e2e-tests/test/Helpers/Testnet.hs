{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Helpers.Testnet where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import CardanoTestnet qualified as TN
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromJust)
import Hedgehog (MonadTest)
import Hedgehog.Extras.Stock.IO.Network.Sprocket qualified as IO
import Hedgehog.Extras.Test qualified as HE
import Hedgehog.Extras.Test.Base qualified as H
import Helpers.Common (cardanoEraToShelleyBasedEra, makeAddress, toEraInCardanoMode)
import Helpers.Utils (maybeReadAs)
import System.Directory qualified as IO
import System.Environment qualified as IO
import System.FilePath ((</>))
import Test.Runtime qualified as TN
import Testnet.Conf qualified as TC (Conf (..), ProjectBase (ProjectBase), YamlFilePath (YamlFilePath), mkConf)

data LocalNodeOptions = LocalNodeOptions
  { era             :: C.AnyCardanoEra
  , protocolVersion :: Int
  , localEnvDir     :: FilePath -- path to directory containing 'utxo-keys' and 'ipc' directories
  , testnetMagic    :: Int
  }

localNodeOptionsPreview :: Either LocalNodeOptions TN.TestnetOptions
localNodeOptionsPreview = Left $ LocalNodeOptions
  { era = C.AnyCardanoEra C.BabbageEra
  , protocolVersion = 8
  , localEnvDir = "/tmp/preview"
  , testnetMagic = 2
  }

testnetOptionsAlonzo6, testnetOptionsBabbage7, testnetOptionsBabbage8 :: Either LocalNodeOptions TN.TestnetOptions
testnetOptionsAlonzo6 = Right $ TN.defaultTestnetOptions {TN.era = C.AnyCardanoEra C.AlonzoEra, TN.protocolVersion = 6}
testnetOptionsBabbage7 = Right $ TN.defaultTestnetOptions {TN.era = C.AnyCardanoEra C.BabbageEra, TN.protocolVersion = 7}
testnetOptionsBabbage8 = Right $ TN.defaultTestnetOptions {TN.era = C.AnyCardanoEra C.BabbageEra, TN.protocolVersion = 8}

eraFromOptions :: (MonadTest m) => Either LocalNodeOptions TN.TestnetOptions -> m C.AnyCardanoEra
eraFromOptions = return . either era TN.era

pvFromOptions :: (MonadTest m) => Either LocalNodeOptions TN.TestnetOptions -> m Int
pvFromOptions = return . either protocolVersion TN.protocolVersion

-- | Get path to where cardano-testnet files are
getProjectBase :: (MonadIO m, MonadTest m) => m String
getProjectBase = liftIO . IO.canonicalizePath =<< HE.getProjectBase

-- | Start a testnet with provided testnet options (including era and protocol version)
startTestnet ::
  C.CardanoEra era ->
  TN.TestnetOptions ->
  FilePath ->
  FilePath ->
  H.Integration (C.LocalNodeConnectInfo C.CardanoMode, C.ProtocolParameters, C.NetworkId)
startTestnet era testnetOptions base tempAbsBasePath' = do
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf :: TC.Conf <- HE.noteShowM $ TC.mkConf (TC.ProjectBase base) (TC.YamlFilePath configurationTemplate) (tempAbsBasePath' <> "/") Nothing
  tn <- TN.testnet testnetOptions conf

  -- Boilerplate codecs used for protocol serialisation. The number of epochSlots is specific
  -- to each blockchain instance. This value is used by cardano mainnet/testnet and only applies
  -- to the Byron era.
  socketPathAbs <- getPoolSocketPathAbs conf tn
  let epochSlots = C.EpochSlots 21600
      localNodeConnectInfo =
        C.LocalNodeConnectInfo
          { C.localConsensusModeParams = C.CardanoModeParams epochSlots
          , C.localNodeNetworkId = getNetworkId tn
          , C.localNodeSocketPath = socketPathAbs
          }
      networkId = getNetworkId tn
  pparams <- getProtocolParams era localNodeConnectInfo
  liftIO $ IO.setEnv "CARDANO_NODE_SOCKET_PATH" socketPathAbs -- set node socket environment for Cardano.Api.Convenience.Query
  pure (localNodeConnectInfo, pparams, networkId)

connectToLocalNode ::
  C.CardanoEra era ->
  LocalNodeOptions ->
  FilePath ->
  H.Integration (C.LocalNodeConnectInfo C.CardanoMode, C.ProtocolParameters, C.NetworkId)
connectToLocalNode era localNodeOptions tempAbsPath = do
  let localEnvDir' = localEnvDir localNodeOptions

  HE.createDirectoryIfMissing (tempAbsPath </> "utxo-keys")
  HE.createDirectoryIfMissing (tempAbsPath </> "sockets")

  HE.createFileLink (localEnvDir' </> "test.skey") (tempAbsPath </> "utxo-keys/utxo1.skey")
  HE.createFileLink (localEnvDir' </> "test.vkey") (tempAbsPath </> "utxo-keys/utxo1.vkey")
  HE.createFileLink (localEnvDir' </> "ipc/node.socket") (tempAbsPath </> "sockets/node.socket")

  let socketPathAbs = tempAbsPath </> "sockets/node.socket"
      networkId = C.Testnet $ C.NetworkMagic $ fromIntegral (testnetMagic localNodeOptions)

  -- Boilerplate codecs used for protocol serialisation. The number of epochSlots is specific
  -- to each blockchain instance. This value is used by cardano mainnet/testnet and only applies
  -- to the Byron era.
  let epochSlots = C.EpochSlots 21600
      localNodeConnectInfo =
        C.LocalNodeConnectInfo
          { C.localConsensusModeParams = C.CardanoModeParams epochSlots,
            C.localNodeNetworkId = networkId,
            C.localNodeSocketPath = socketPathAbs
          }
  pparams <- getProtocolParams era localNodeConnectInfo
  liftIO $ IO.setEnv "CARDANO_NODE_SOCKET_PATH" socketPathAbs -- set node socket environment for Cardano.Api.Convenience.Query
  pure (localNodeConnectInfo, pparams, networkId)

-- | Start testnet with cardano-testnet or use local node that's already
--   connected to a public testnet
setupTestEnvironment ::
  Either LocalNodeOptions TN.TestnetOptions ->
  FilePath ->
  H.Integration (C.LocalNodeConnectInfo C.CardanoMode, C.ProtocolParameters, C.NetworkId)
setupTestEnvironment options tempAbsPath = do
  case options of
    Left localNodeOptions -> do
      C.AnyCardanoEra era <- return $ era localNodeOptions
      connectToLocalNode era localNodeOptions tempAbsPath
    Right testnetOptions -> do
      C.AnyCardanoEra era <- return $ TN.era testnetOptions
      base <- getProjectBase
      startTestnet era testnetOptions base tempAbsPath

-- | Network ID of the testnet
getNetworkId :: TN.TestnetRuntime -> C.NetworkId
getNetworkId tn = C.Testnet $ C.NetworkMagic $ fromIntegral (TN.testnetMagic tn)

-- | Path to a pool node's unix socket
getPoolSocketPathAbs :: (MonadTest m, MonadIO m) => TC.Conf -> TN.TestnetRuntime -> m FilePath
getPoolSocketPathAbs conf tn = do
  let tempAbsPath = TC.tempAbsPath conf
  socketPath <- IO.sprocketArgumentName <$> H.headM (TN.poolNodeSprocket <$> TN.poolNodes tn)
  H.note =<< (liftIO $ IO.canonicalizePath $ tempAbsPath </> socketPath)

-- | Query network's protocol parameters
getProtocolParams :: (MonadIO m, MonadTest m) => C.CardanoEra era -> C.LocalNodeConnectInfo C.CardanoMode -> m C.ProtocolParameters
getProtocolParams era localNodeConnectInfo =
  H.leftFailM . H.leftFailM . liftIO $
    C.queryNodeLocalState localNodeConnectInfo Nothing $
      C.QueryInEra (toEraInCardanoMode era) $ C.QueryInShelleyBasedEra (cardanoEraToShelleyBasedEra era) C.QueryProtocolParameters

-- | Signing key and address for wallet 1
--   Handles two key types: GenesisUTxOKey and PaymentKey
w1 ::
  (MonadIO m, MonadTest m) =>
  FilePath ->
  C.NetworkId ->
  m (C.SigningKey C.PaymentKey, C.VerificationKey C.PaymentKey, C.Address C.ShelleyAddr)
w1 tempAbsPath' networkId = do
  -- GenesisUTxOKey comes from cardano-testnet
  mGenesisVKey :: Maybe (C.VerificationKey C.GenesisUTxOKey) <-
    maybeReadAs (C.AsVerificationKey C.AsGenesisUTxOKey) $ tempAbsPath' </> "utxo-keys/utxo1.vkey"
  mGenesisSKey :: Maybe (C.SigningKey C.GenesisUTxOKey) <-
    maybeReadAs (C.AsSigningKey C.AsGenesisUTxOKey) $ tempAbsPath' </> "utxo-keys/utxo1.skey"
  -- PaymentKey comes from cardano-cli (the likely type for a locally created wallet)
  mPaymentVKey :: Maybe (C.VerificationKey C.PaymentKey) <-
    maybeReadAs (C.AsVerificationKey C.AsPaymentKey) $ tempAbsPath' </> "utxo-keys/utxo1.vkey"
  mPaymentSKey :: Maybe (C.SigningKey C.PaymentKey) <-
    maybeReadAs (C.AsSigningKey C.AsPaymentKey) $ tempAbsPath' </> "utxo-keys/utxo1.skey"

  let vKey :: C.VerificationKey C.PaymentKey = maybe (fromJust mPaymentVKey) C.castVerificationKey mGenesisVKey
      sKey :: C.SigningKey C.PaymentKey = maybe (fromJust mPaymentSKey) C.castSigningKey mGenesisSKey
      address = makeAddress (Left vKey) networkId

  return (sKey, vKey, address)
