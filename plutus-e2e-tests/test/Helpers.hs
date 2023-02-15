{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Helpers where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List as List
import Data.Map qualified as Map
import Data.Maybe as M
import Data.Set qualified as Set
import GHC.Stack qualified as GHC
import System.Directory qualified as IO
import System.Environment qualified as IO
import System.FilePath ((</>))
import System.IO qualified as IO
import System.IO.Temp qualified as IO
import System.Info qualified as IO

import Hedgehog (MonadTest)
import Hedgehog qualified as H
import Hedgehog.Extras.Stock.CallStack qualified as H
import Hedgehog.Extras.Stock.IO.Network.Sprocket qualified as IO
import Hedgehog.Extras.Test qualified as HE
import Hedgehog.Extras.Test.Base qualified as H

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (SubmitFail, SubmitSuccess))
import Test.Runtime qualified as TN
import Testnet.Conf qualified as TC (Conf (..), ProjectBase (ProjectBase), YamlFilePath (YamlFilePath), mkConf)
import Testnet.Plutus qualified as TN

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
testnetOptionsAlonzo6  = Right $ TN.defaultTestnetOptions {TN.era = C.AnyCardanoEra C.AlonzoEra,  TN.protocolVersion = 6}
testnetOptionsBabbage7 = Right $ TN.defaultTestnetOptions {TN.era = C.AnyCardanoEra C.BabbageEra, TN.protocolVersion = 7}
testnetOptionsBabbage8 = Right $ TN.defaultTestnetOptions {TN.era = C.AnyCardanoEra C.BabbageEra, TN.protocolVersion = 8}

eraFromOptions :: (MonadTest m) => Either LocalNodeOptions TN.TestnetOptions -> m C.AnyCardanoEra
eraFromOptions = return . either era TN.era

pvFromOptions :: (MonadTest m) => Either LocalNodeOptions TN.TestnetOptions -> m Int
pvFromOptions = return . either protocolVersion TN.protocolVersion

-- | Right from Either or throw Left error
unsafeFromRight :: Show l => Either l r -> r
unsafeFromRight (Left err)    = error (show err)
unsafeFromRight (Right value) = value

-- | Maybe throw error
unsafeFromMaybe :: Maybe a -> a
unsafeFromMaybe Nothing  = error "not just, nothing."
unsafeFromMaybe (Just a) = a

-- | Check whether the auto-balancing txbody build (constructBalancedTx) resulted in an error
isTxBodyScriptExecutionError :: String -> Either C.TxBodyErrorAutoBalance r -> Bool
isTxBodyScriptExecutionError
  expectedError (Left (C.TxBodyScriptExecutionError m)) = expectedError `isInfixOf` (show m)
isTxBodyScriptExecutionError _ _                        = False

-- | Get path to where cardano-testnet files are
getProjectBase :: (MonadIO m, MonadTest m) => m String
getProjectBase = liftIO . IO.canonicalizePath =<< HE.getProjectBase

-- | Any CardanoEra with CardanoMode
toEraInCardanoMode :: C.CardanoEra era -> (C.EraInMode era C.CardanoMode)
toEraInCardanoMode era = fromMaybe $ C.toEraInMode era C.CardanoMode
  where
    fromMaybe Nothing    = error $ "No mode for this era " ++ show era ++ " in CardanoMode"
    fromMaybe (Just eim) = eim

-- | Any CardanoEra to one that supports tokens. Used for building TxOut.
multiAssetSupportedInEra :: C.CardanoEra era -> C.MultiAssetSupportedInEra era
multiAssetSupportedInEra era = fromEither $ C.multiAssetSupportedInEra era
  where
    fromEither (Left _)  = error $ "Era must support MA"
    fromEither (Right m) = m

-- | Treat CardanoEra as ShelleyBased to satisfy constraint on constructBalancedTx.
withIsShelleyBasedEra :: C.CardanoEra era -> (C.IsShelleyBasedEra era => r) -> r
withIsShelleyBasedEra era r =
    case era of
        C.AlonzoEra  -> r
        C.BabbageEra -> r
        _            -> error "Must use Alonzo or Babbage era"

-- | Converts a C.CardanoEra to a C.ShelleyBasedEra. Used for querying in Shelley-based era.
cardanoEraToShelleyBasedEra :: C.CardanoEra era -> C.ShelleyBasedEra era
cardanoEraToShelleyBasedEra cEra = case cEra of
    C.AlonzoEra  -> C.ShelleyBasedEraAlonzo
    C.BabbageEra -> C.ShelleyBasedEraBabbage
    _            -> error "Must use Alonzo or Babbage era"

-- | This is a copy of the workspace from
-- hedgehog-extras:Hedgehog.Extras.Test.Base, which for darwin sets
-- the systemTemp folder to /tmp.
--
-- It creates a temporary folder with @prefixPath@, which is removed
-- after the supplied function @f@ returns.
workspace :: (MonadTest m, MonadIO m, GHC.HasCallStack) => FilePath -> (FilePath -> m ()) -> m ()
workspace prefixPath f = GHC.withFrozenCallStack $ do
  systemTemp <- case IO.os of
    "darwin" -> pure "/tmp"
    _        -> H.evalIO IO.getCanonicalTemporaryDirectory
  maybeKeepWorkspace <- H.evalIO $ IO.lookupEnv "KEEP_WORKSPACE"
  let systemPrefixPath = systemTemp <> "/" <> prefixPath
  H.evalIO $ IO.createDirectoryIfMissing True systemPrefixPath
  ws <- H.evalIO $ IO.createTempDirectory systemPrefixPath "test"
  H.annotate $ "Workspace: " <> ws
  liftIO $ IO.writeFile (ws <> "/module") H.callerModuleName
  f ws
  when (IO.os /= "mingw32" && maybeKeepWorkspace /= Just "1") $ do
    H.evalIO $ IO.removeDirectoryRecursive ws

-- | Start a testnet with provided testnet options (including era and protocol version)
startTestnet :: C.CardanoEra era
  -> TN.TestnetOptions
  -> FilePath
  -> FilePath
  -> H.Integration (C.LocalNodeConnectInfo C.CardanoMode, C.ProtocolParameters, C.NetworkId)
startTestnet era testnetOptions base tempAbsBasePath' = do
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf :: TC.Conf <- HE.noteShowM $
    TC.mkConf (TC.ProjectBase base) (TC.YamlFilePath configurationTemplate)
      (tempAbsBasePath' <> "/")
      Nothing
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

connectToLocalNode :: C.CardanoEra era
  -> LocalNodeOptions
  -> FilePath
  -> H.Integration (C.LocalNodeConnectInfo C.CardanoMode, C.ProtocolParameters, C.NetworkId)
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
          { C.localConsensusModeParams = C.CardanoModeParams epochSlots
          , C.localNodeNetworkId = networkId
          , C.localNodeSocketPath = socketPathAbs
          }
  pparams <- getProtocolParams era localNodeConnectInfo
  liftIO $ IO.setEnv "CARDANO_NODE_SOCKET_PATH" socketPathAbs -- set node socket environment for Cardano.Api.Convenience.Query
  pure (localNodeConnectInfo, pparams, networkId)

-- | Start testnet with cardano-testnet or use local node that's already
--   connected to a public testnet
setupTestEnvironment :: Either LocalNodeOptions TN.TestnetOptions
  -> FilePath
  -> H.Integration (C.LocalNodeConnectInfo C.CardanoMode, C.ProtocolParameters, C.NetworkId)
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
getProtocolParams era localNodeConnectInfo = H.leftFailM . H.leftFailM . liftIO
  $ C.queryNodeLocalState localNodeConnectInfo Nothing
  $ C.QueryInEra (toEraInCardanoMode era) $ C.QueryInShelleyBasedEra (cardanoEraToShelleyBasedEra era) C.QueryProtocolParameters

-- | Read file text envelope as a specific type (e.g. C.VerificationKey C.GenesisUTxOKey)
--   and throw error on failure
readAs :: (C.HasTextEnvelope a, MonadIO m, MonadTest m) => C.AsType a -> FilePath -> m a
readAs as path = do
  path' <- H.note path
  H.leftFailM . liftIO $ C.readFileTextEnvelope as path'

-- | Same as readAs but return Nothing on error
maybeReadAs :: (C.HasTextEnvelope a, MonadIO m, MonadTest m) => C.AsType a -> FilePath -> m (Maybe a)
maybeReadAs as path = do
  path' <- H.note path
  maybeEither . liftIO $ C.readFileTextEnvelope as path'
  where
    maybeEither m = m >>= return . either (const Nothing) Just

-- | Signing key and address for wallet 1
--   Handles two key types: GenesisUTxOKey and PaymentKey
w1 :: (MonadIO m, MonadTest m)
  => FilePath
  -> C.NetworkId
  -> m (C.SigningKey C.PaymentKey, C.Address C.ShelleyAddr)
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

  let
    vKey :: C.VerificationKey C.PaymentKey = maybe (fromJust mPaymentVKey) (C.castVerificationKey) mGenesisVKey
    sKey :: C.SigningKey C.PaymentKey = maybe (fromJust mPaymentSKey) (C.castSigningKey) mGenesisSKey
    address = makeAddress (Left vKey) networkId

  return (sKey, address)

-- | Make a payment or script address
makeAddress :: (Either (C.VerificationKey C.PaymentKey) C.ScriptHash)
  -> C.NetworkId
  -> C.Address C.ShelleyAddr
makeAddress (Left paymentKey)  nId =
    C.makeShelleyAddress nId (C.PaymentCredentialByKey $ C.verificationKeyHash paymentKey) C.NoStakeAddress
makeAddress (Right scriptHash) nId =
    C.makeShelleyAddress nId (C.PaymentCredentialByScript scriptHash) C.NoStakeAddress

-- | Build TxOut for spending or minting with no datum or reference script present
txOut :: C.CardanoEra era
  -> C.Value
  -> C.Address C.ShelleyAddr
  -> C.TxOut C.CtxTx era
txOut era value address = C.TxOut
    (maybeAnyAddressInEra $ C.anyAddressInEra era $ C.toAddressAny address)
    (C.TxOutValue (multiAssetSupportedInEra era) value)
    C.TxOutDatumNone
    C.ReferenceScriptNone
  where
    maybeAnyAddressInEra Nothing    = error $ "Era must be ShelleyBased"
    maybeAnyAddressInEra (Just aie) = aie

-- | Build TxOut with a reference script
txOutWithRefScript :: C.CardanoEra era
  -> C.Value
  -> C.Address C.ShelleyAddr
  -> C.Script lang
  -> C.TxOut C.CtxTx era
txOutWithRefScript era value address script = withRefScript era script $ txOut era value address

txOutWithInlineDatum, txOutWithDatumHash, txOutWithDatumInTx :: C.CardanoEra era
  -> C.Value
  -> C.Address C.ShelleyAddr
  -> C.ScriptData
  -> C.TxOut C.CtxTx era
-- | Build TxOut with inline datum
txOutWithInlineDatum era value address datum = withInlineDatum era datum $ txOut era value address
-- | Build TxOut with datum hash
txOutWithDatumHash era value address datum = withDatumHash era datum $ txOut era value address
-- | Build TxOut with datum hash whilst including datum value in txbody
txOutWithDatumInTx era value address datum = withDatumInTx era datum $ txOut era value address

-- | Add reference script to TxOut
withRefScript :: C.CardanoEra era
  -> C.Script lang
  -> C.TxOut C.CtxTx era
  -> C.TxOut C.CtxTx era
withRefScript era script (C.TxOut e v d _) =
    C.TxOut e v d (C.ReferenceScript (refInsScriptsAndInlineDatsSupportedInEra era) (C.toScriptInAnyLang script))

withInlineDatum, withDatumHash, withDatumInTx :: C.CardanoEra era
  -> C.ScriptData
  -> C.TxOut C.CtxTx era
  -> C.TxOut C.CtxTx era
-- | Add inline datum to TxOut
withInlineDatum era datum (C.TxOut e v _ rs) =
    C.TxOut e v (C.TxOutDatumInline (refInsScriptsAndInlineDatsSupportedInEra era) datum) rs
-- | Add datum hash to TxOut
withDatumHash era datum (C.TxOut e v _ rs) =
    C.TxOut e v (C.TxOutDatumHash (scriptDataSupportedInEra era) (C.hashScriptData datum)) rs
-- | Add datum hash to TxOut whilst including datum value in txbody
withDatumInTx era datum (C.TxOut e v _ rs) =
    C.TxOut e v (C.TxOutDatumInTx (scriptDataSupportedInEra era) datum)  rs

refInsScriptsAndInlineDatsSupportedInEra :: C.CardanoEra era -> C.ReferenceTxInsScriptsInlineDatumsSupportedInEra era
refInsScriptsAndInlineDatsSupportedInEra = fromMaybe . C.refInsScriptsAndInlineDatsSupportedInEra
  where
    fromMaybe Nothing  = error "Era must support reference inputs"
    fromMaybe (Just e) = e

scriptDataSupportedInEra :: C.CardanoEra era -> C.ScriptDataSupportedInEra era
scriptDataSupportedInEra = fromMaybe . C.scriptDataSupportedInEra
  where
    fromMaybe Nothing  = error "Era must support script data"
    fromMaybe (Just e) = e

-- | Find the first UTxO at address and return as TxIn. Used for txbody's txIns.
firstTxIn :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo C.CardanoMode
  -> C.Address C.ShelleyAddr
  -> m C.TxIn
firstTxIn era = txInAtAddressByIndex era 0

-- | Find UTxO at address by index and return as TxIn. Used for txbody's txIns.
txInAtAddressByIndex :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> Int
  -> C.LocalNodeConnectInfo C.CardanoMode
  -> C.Address C.ShelleyAddr
  -> m C.TxIn
txInAtAddressByIndex era idx localNodeConnectInfo address = do
  atM idx =<< txInsFromUtxo =<< findUTxOByAddress era localNodeConnectInfo address
  where
    atM :: (MonadTest m) => Int -> [a] -> m a
    atM i' l = return $ l !! i'

-- | Find the TxIn at address which is ada-only and has the most ada
adaOnlyTxInAtAddress :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo C.CardanoMode
  -> C.Address C.ShelleyAddr
  -> m C.TxIn
adaOnlyTxInAtAddress era localNodeConnectInfo address = do
  utxo <- findUTxOByAddress era localNodeConnectInfo address
  return $ fst $ head $ sortByMostAda $ adaOnly $ Map.toList $ C.unUTxO utxo
  where
    adaOnly = filter
      (\(_, C.TxOut _ (C.TxOutValue _ v) _ _) ->
        ((length $ C.valueToList v) == 1) &&
        ((fst $ head $ C.valueToList v) == C.AdaAssetId))
    sortByMostAda = sortBy
      (\(_, C.TxOut _ (C.TxOutValue _ v1) _ _)
        (_, C.TxOut _ (C.TxOutValue _ v2) _ _) ->
            compare (snd $ head $ C.valueToList v2) (snd $ head $ C.valueToList v1))

-- | Get TxIns from all UTxOs
txInsFromUtxo :: (MonadIO m) => C.UTxO era -> m [C.TxIn]
txInsFromUtxo utxos = do
  let (txIns, _) = unzip $ Map.toList $ C.unUTxO utxos
  return txIns

-- | Query ledger for UTxOs at address
findUTxOByAddress
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era -> C.LocalNodeConnectInfo C.CardanoMode -> C.Address a -> m (C.UTxO era)
findUTxOByAddress era localNodeConnectInfo address = let
  query = C.QueryInShelleyBasedEra (cardanoEraToShelleyBasedEra era) $ C.QueryUTxO $
    C.QueryUTxOByAddress $ Set.singleton (C.toAddressAny address)
  in
  H.leftFailM . H.leftFailM . liftIO $ C.queryNodeLocalState localNodeConnectInfo Nothing $
    C.QueryInEra (toEraInCardanoMode era) query

-- | Get [TxIn] and total lovelace value for an address.
getAddressTxInsLovelaceValue
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era -> C.LocalNodeConnectInfo C.CardanoMode -> C.Address a -> m ([C.TxIn], C.Lovelace)
getAddressTxInsLovelaceValue era con address = do
  utxo <- findUTxOByAddress era con address
  let
    (txIns, txOuts) = unzip $ Map.toList $ C.unUTxO utxo
    values = map (\case C.TxOut _ v _ _ -> C.txOutValueToLovelace v) txOuts
  pure (txIns, sum values)

-- | Get [TxIn] and value for an address (including assets).
getAddressTxInsValue
  :: (MonadIO m, MonadTest m)
  => C.CardanoEra era -> C.LocalNodeConnectInfo C.CardanoMode -> C.Address a -> m ([C.TxIn], C.Value)
getAddressTxInsValue era con address = do
  utxo <- findUTxOByAddress era con address
  let
    (txIns, txOuts) = unzip $ Map.toList $ C.unUTxO utxo
    values = map (\case C.TxOut _ v _ _ -> C.txOutValueToValue v) txOuts
  pure (txIns, (mconcat values))

-- | Empty transaction body to begin building from.
emptyTxBodyContent :: C.CardanoEra era -> C.ProtocolParameters -> C.TxBodyContent C.BuildTx era
emptyTxBodyContent era pparams = C.TxBodyContent
  { C.txIns              = []
  , C.txInsCollateral    = C.TxInsCollateralNone
  , C.txInsReference     = C.TxInsReferenceNone
  , C.txOuts             = []
  , C.txTotalCollateral  = C.TxTotalCollateralNone
  , C.txReturnCollateral = C.TxReturnCollateralNone
  , C.txFee              = C.TxFeeExplicit (fromTxFeesExplicit $ C.txFeesExplicitInEra era) 0
  , C.txValidityRange    = (C.TxValidityNoLowerBound,(C.TxValidityNoUpperBound $
                             fromNoUpperBoundMaybe $ C.validityNoUpperBoundSupportedInEra era))
  , C.txMetadata         = C.TxMetadataNone
  , C.txAuxScripts       = C.TxAuxScriptsNone
  , C.txExtraKeyWits     = C.TxExtraKeyWitnessesNone
  , C.txProtocolParams   = C.BuildTxWith $ Just pparams
  , C.txWithdrawals      = C.TxWithdrawalsNone
  , C.txCertificates     = C.TxCertificatesNone
  , C.txUpdateProposal   = C.TxUpdateProposalNone
  , C.txMintValue        = C.TxMintNone
  , C.txScriptValidity   = C.TxScriptValidityNone
  }
  where
    fromNoUpperBoundMaybe Nothing    = error "Era must support no upper bound"
    fromNoUpperBoundMaybe (Just nub) = nub

    fromTxFeesExplicit (Left _)    = error "Era must support explicit fees"
    fromTxFeesExplicit (Right tfe) = tfe

-- | Produce collateral inputs if era supports it. Used for building txbody.
txInsCollateral :: C.CardanoEra era -> [C.TxIn] -> C.TxInsCollateral era
txInsCollateral era txIns = case C.collateralSupportedInEra era of
    Nothing        -> error "era supporting collateral only"
    Just supported -> C.TxInsCollateral supported txIns

-- | Get TxId from a signed transaction.
--  Useful for producing TxIn for building subsequant transaction.
txId :: C.Tx era -> C.TxId
txId = C.getTxId . C.getTxBody

-- | Build TxIn from TxId and index. Useful for waiting for or asserting expected TxOut is
--   onchain after submitting transaction.
txIn :: C.TxId -> Int -> C.TxIn
txIn txId txIx = C.TxIn txId (C.TxIx $ fromIntegral txIx)

pubkeyTxIns  :: [C.TxIn] -> [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era))]
pubkeyTxIns = map (\txIn -> txInWitness txIn $ C.KeyWitness C.KeyWitnessForSpending)

txInWitness :: C.TxIn -> (C.Witness C.WitCtxTxIn era) -> (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era))
txInWitness txIn wit = (txIn, C.BuildTxWith wit)

txInsReference :: C.CardanoEra era
  -> [C.TxIn]
  -> C.TxInsReference build era
txInsReference era txIns = case era of
    C.BabbageEra -> C.TxInsReference C.ReferenceTxInsScriptsInlineDatumsInBabbageEra txIns

txMintValue :: C.CardanoEra era
  -> C.Value
  -> Map.Map C.PolicyId (C.ScriptWitness C.WitCtxMint era)
  -> C.TxMintValue C.BuildTx era
txMintValue era tv m = C.TxMintValue (multiAssetSupportedInEra era) tv (C.BuildTxWith m)

buildTx :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.TxBodyContent C.BuildTx era
  -> C.Address C.ShelleyAddr
  -> C.SigningKey C.PaymentKey
  -> C.NetworkId
  -> m (C.Tx era)
buildTx era txBody changeAddress sKey networkId = do
    eitherTx <- buildTx' era txBody changeAddress sKey networkId
    return $ fromEither eitherTx
    where
      fromEither (Left e)   = error $ show e
      fromEither (Right tx) = tx

-- | Maybe build signed transaction using convenience functions for calculating fees and exunits.
--   Useful for asserting for error.
buildTx' :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.TxBodyContent C.BuildTx era
  -> C.Address C.ShelleyAddr
  -> C.SigningKey C.PaymentKey
  -> C.NetworkId
  -> m (Either C.TxBodyErrorAutoBalance (C.Tx era))
buildTx' era txBody changeAddress sKey networkId = do
  (nodeEraUtxo, pparams, eraHistory, systemStart, stakePools) <- H.leftFailM . liftIO $
    C.queryStateForBalancedTx era networkId allInputs

  return $ withIsShelleyBasedEra era $ C.constructBalancedTx
    (toEraInCardanoMode era)
    txBody
    (C.shelleyAddressInEra changeAddress)
    Nothing -- Override key witnesses
    nodeEraUtxo -- tx inputs
    pparams
    eraHistory
    systemStart
    stakePools
    [C.WitnessPaymentKey sKey]
  where
    allInputs :: [C.TxIn]
    allInputs = do
      let txIns = (fst <$> C.txIns txBody)
      case C.txInsReference txBody of
        C.TxInsReferenceNone        -> txIns
        C.TxInsReference _ refTxIns -> txIns ++ refTxIns

submitTx :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo C.CardanoMode
  -> C.Tx era
  -> m ()
submitTx era localNodeConnectInfo tx = do
  submitResult :: SubmitResult (C.TxValidationErrorInMode C.CardanoMode) <-
    liftIO $ C.submitTxToNodeLocal localNodeConnectInfo $ C.TxInMode tx (toEraInCardanoMode era)
  failOnTxSubmitFail submitResult
  where
    failOnTxSubmitFail :: (Show a, MonadTest m) => SubmitResult a -> m ()
    failOnTxSubmitFail = \case
      SubmitFail reason -> H.failMessage GHC.callStack $ "Transaction failed: " <> show reason
      SubmitSuccess     -> pure ()

--TODO: loop timeout
waitForTxIdAtAddress :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo C.CardanoMode
  -> C.Address C.ShelleyAddr
  -> C.TxId
  -> m ()
waitForTxIdAtAddress era localNodeConnectInfo address txId = do
  let loop = do
        txIns <- txInsFromUtxo =<< findUTxOByAddress era localNodeConnectInfo address
        let txIds = map (\(C.TxIn txId _) -> txId) txIns
        when (not $ txId `elem` txIds) loop
  loop

waitForTxInAtAddress :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo C.CardanoMode
  -> C.Address C.ShelleyAddr
  -> C.TxIn
  -> String -- temp debug text for intermittent timeout failure
  -> m ()
waitForTxInAtAddress era localNodeConnectInfo address txIn debugStr = do
  let timeoutSeconds = 90 :: Int
      loop i prevUtxo = do
        if i == 0
          then error ("waitForTxInAtAddress timeout. \n-- Debug --\nTest function: " ++ debugStr
                    ++ "\nAddress: " ++ show address ++ "\nTxIn: " ++ show txIn ++ "\nPrev UTxO: " ++ show prevUtxo)
          else HE.threadDelay 1000000
        utxos <- findUTxOByAddress era localNodeConnectInfo address
        when (Map.notMember txIn $ C.unUTxO utxos) (loop (pred i) (show utxos))
  loop timeoutSeconds " "

-- | Get tx out at address is for general use when txo is expected
getTxOutAtAddress :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo C.CardanoMode
  -> C.Address C.ShelleyAddr
  -> C.TxIn
  -> String -- temp debug text for intermittent timeout failure (waitForTxInAtAddress)
  -> m (C.TxOut C.CtxUTxO era)
getTxOutAtAddress era localNodeConnectInfo address txIn debugStr = do
    maybeTxOut <- getTxOutAtAddress' era localNodeConnectInfo address txIn debugStr
    return $ fromMaybe maybeTxOut
    where
      fromMaybe Nothing    = error $ "txIn " ++ show txIn ++ " is not at address " ++ show address
      fromMaybe (Just txo) = txo

-- | Maybe get tx out at address for asserting when it is not expected to be present
getTxOutAtAddress' :: (MonadIO m, MonadTest m)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo C.CardanoMode
  -> C.Address C.ShelleyAddr
  -> C.TxIn
  -> String -- temp debug text for intermittent timeout failure (waitForTxInAtAddress)
  -> m (Maybe (C.TxOut C.CtxUTxO era))
getTxOutAtAddress' era localNodeConnectInfo address txIn debugStr = do
  waitForTxInAtAddress era localNodeConnectInfo address txIn debugStr
  utxos <- findUTxOByAddress era localNodeConnectInfo address
  return $ Map.lookup txIn $ C.unUTxO utxos

txOutHasValue :: (MonadIO m)
  => C.TxOut C.CtxUTxO era
  -> C.Value
  -> m Bool
txOutHasValue (C.TxOut _ txOutValue _ _) tokenValue = do
  let value = C.txOutValueToValue txOutValue
  return $ List.isInfixOf (C.valueToList tokenValue) (C.valueToList value)
