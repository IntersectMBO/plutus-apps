{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TupleSections       #-}

module Helpers where

import Control.Concurrent qualified as IO
import Control.Concurrent.Async qualified as IO
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Stack qualified as GHC
import Streaming.Prelude qualified as S
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
import Cardano.Streaming qualified as CS
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (SubmitFail, SubmitSuccess))
import Test.Runtime qualified as TN
import Testnet.Cardano qualified as TN
import Testnet.Conf qualified as TC (Conf (..), ProjectBase (ProjectBase), YamlFilePath (YamlFilePath), mkConf)

-- | Start a testnet.
startTestnet
  :: TN.TestnetOptions
  -> FilePath
  -> FilePath
  -> H.Integration (C.LocalNodeConnectInfo C.CardanoMode, TC.Conf, TN.TestnetRuntime)
startTestnet testnetOptions base tempAbsBasePath' = do
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf :: TC.Conf <- HE.noteShowM $
    TC.mkConf (TC.ProjectBase base) (TC.YamlFilePath configurationTemplate)
      (tempAbsBasePath' <> "/")
      Nothing
  tn <- TN.testnet testnetOptions conf

  -- Boilerplate codecs used for protocol serialisation.  The number
  -- of epochSlots is specific to each blockchain instance. This value
  -- what the cardano main and testnet uses. Only applies to the Byron
  -- era.
  socketPathAbs <- getSocketPathAbs conf tn
  let epochSlots = C.EpochSlots 21600
      localNodeConnectInfo =
        C.LocalNodeConnectInfo
          { C.localConsensusModeParams = C.CardanoModeParams epochSlots
          , C.localNodeNetworkId = getNetworkId tn
          , C.localNodeSocketPath = socketPathAbs
          }

  pure (localNodeConnectInfo, conf, tn)

getNetworkId :: TN.TestnetRuntime -> C.NetworkId
getNetworkId tn = C.Testnet $ C.NetworkMagic $ fromIntegral (TN.testnetMagic tn)

getSocketPathAbs :: (MonadTest m, MonadIO m) => TC.Conf -> TN.TestnetRuntime -> m FilePath
getSocketPathAbs conf tn = do
  let tempAbsPath = TC.tempAbsPath conf
  socketPath <- IO.sprocketArgumentName <$> H.headM (TN.nodeSprocket <$> TN.bftNodes tn)
  H.note =<< (liftIO $ IO.canonicalizePath $ tempAbsPath </> socketPath)

getPoolSocketPathAbs :: (MonadTest m, MonadIO m) => TC.Conf -> TN.TestnetRuntime -> m FilePath
getPoolSocketPathAbs conf tn = do
  let tempAbsPath = TC.tempAbsPath conf
  socketPath <- IO.sprocketArgumentName <$> H.headM (TN.poolNodeSprocket <$> TN.poolNodes tn)
  H.note =<< (liftIO $ IO.canonicalizePath $ tempAbsPath </> socketPath)

readAs :: (C.HasTextEnvelope a, MonadIO m, MonadTest m) => C.AsType a -> FilePath -> m a
readAs as path = do
  path' <- H.note path
  H.leftFailM . liftIO $ C.readFileTextEnvelope as path'

-- | An empty transaction
emptyTxBodyContent
  :: C.IsShelleyBasedEra era
  => (C.TxValidityLowerBound era, C.TxValidityUpperBound era)
  -> C.ProtocolParameters
  -> C.TxBodyContent C.BuildTx era
emptyTxBodyContent validityRange pparams = C.TxBodyContent
  { C.txIns              = []
  , C.txInsCollateral    = C.TxInsCollateralNone
  , C.txInsReference     = C.TxInsReferenceNone
  , C.txOuts             = []
  , C.txTotalCollateral  = C.TxTotalCollateralNone
  , C.txReturnCollateral = C.TxReturnCollateralNone
  , C.txFee              = C.TxFeeExplicit (txFeesExplicitInShelleyBasedEra C.shelleyBasedEra) 0
  , C.txValidityRange    = validityRange
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

getProtocolParams :: forall era m
    . (C.IsShelleyBasedEra era, MonadIO m, MonadTest m)
    => C.LocalNodeConnectInfo C.CardanoMode
    -> m C.ProtocolParameters
getProtocolParams localNodeConnectInfo = do
  eraInMode <- H.nothingFail
             $ C.toEraInMode (C.shelleyBasedToCardanoEra (C.shelleyBasedEra @era)) C.CardanoMode
  H.leftFailM . H.leftFailM . liftIO
      $ C.queryNodeLocalState localNodeConnectInfo Nothing
      $ C.QueryInEra eraInMode
      $ C.QueryInShelleyBasedEra C.shelleyBasedEra C.QueryProtocolParameters

findUTxOByAddress
  :: (C.IsShelleyBasedEra era, MonadIO m, MonadTest m)
  => C.LocalNodeConnectInfo C.CardanoMode
  -> C.Address a
  -> m (C.UTxO era)
findUTxOByAddress localNodeConnectInfo address = do
  let query = C.QueryInShelleyBasedEra C.shelleyBasedEra
            $ C.QueryUTxO
            $ C.QueryUTxOByAddress
            $ Set.singleton (C.toAddressAny address)
  eraInMode <- H.nothingFail
             $ C.toEraInMode (C.shelleyBasedToCardanoEra C.shelleyBasedEra) C.CardanoMode
  H.leftFailM . H.leftFailM . liftIO $ C.queryNodeLocalState localNodeConnectInfo Nothing $
    C.QueryInEra eraInMode query

-- | Get [TxIn] and total value for an address.
getAddressTxInsValue
  :: forall era m a. (C.IsShelleyBasedEra era, MonadIO m, MonadTest m)
  => C.LocalNodeConnectInfo C.CardanoMode
  -> C.Address a
  -> m ([C.TxIn], C.Lovelace)
getAddressTxInsValue con address = do
  utxo <- findUTxOByAddress @era con address
  let
    (txIns, txOuts) = unzip $ Map.toList $ C.unUTxO utxo
    values = map (\case C.TxOut _ v _ _ -> C.txOutValueToLovelace v) txOuts
  pure (txIns, sum values)

submitTx
    :: (C.IsCardanoEra era, MonadIO m, MonadTest m)
    => C.LocalNodeConnectInfo C.CardanoMode
    -> C.Tx era
    -> m ()
submitTx localNodeConnectInfo tx = do
  eraInMode <- H.nothingFail
             $ C.toEraInMode C.cardanoEra C.CardanoMode
  submitResult :: SubmitResult (C.TxValidationErrorInMode C.CardanoMode) <-
    liftIO $ C.submitTxToNodeLocal localNodeConnectInfo $ C.TxInMode tx eraInMode
  failOnTxSubmitFail submitResult
  where
    failOnTxSubmitFail :: (Show a, MonadTest m) => SubmitResult a -> m ()
    failOnTxSubmitFail = \case
      SubmitFail reason -> H.failMessage GHC.callStack $ "Transaction failed: " <> show reason
      SubmitSuccess     -> pure ()

-- | Block until a transaction with @txId@ is sent over the local chainsync protocol.
awaitTxId :: C.LocalNodeConnectInfo C.CardanoMode -> C.TxId -> IO ()
awaitTxId con txId = do
  chan :: IO.Chan [C.TxId] <- IO.newChan
  let indexer = CS.blocks con C.ChainPointAtGenesis
        & CS.ignoreRollbacks
        & S.map bimTxIds
        & S.chain (IO.writeChan chan)
  void $ (IO.link =<<) $ IO.async $ void $ S.effects indexer
  let loop = do
        txIds <- IO.readChan chan
        when (txId `notElem` txIds) loop
  loop

-- | Submit the argument transaction and await for it to be accepted into the blockhain.
submitAwaitTx
  :: (C.IsCardanoEra era, MonadIO m, MonadTest m)
  => C.LocalNodeConnectInfo C.CardanoMode
  -> (C.Tx era, C.TxBody era)
  -> m ()
submitAwaitTx con (tx, txBody) = do
  submitTx con tx
  liftIO $ awaitTxId con $ C.getTxId txBody

mkTransferTx
  :: forall era m. (C.IsShelleyBasedEra era, MonadIO m, MonadTest m, MonadFail m)
  => C.NetworkId
  -> C.LocalNodeConnectInfo C.CardanoMode
  -> (C.TxValidityLowerBound era, C.TxValidityUpperBound era)
  -> C.Address C.ShelleyAddr
  -> C.Address C.ShelleyAddr
  -> [C.ShelleyWitnessSigningKey]
  -> C.Lovelace
  -> m (C.Tx era, C.TxBody era)
mkTransferTx networkId con validityRange from to keyWitnesses howMuch = do
  pparams <- getProtocolParams @era con
  (txIns, totalLovelace) <- getAddressTxInsValue @era con from
  let
    tx0 = (emptyTxBodyContent validityRange pparams)
      { C.txIns = map (, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) txIns
      , C.txOuts = [mkAddressAdaTxOut to totalLovelace]
      }
  txBody0 :: C.TxBody era <- HE.leftFail $ C.makeTransactionBody tx0
  let fee = calculateFee
                pparams
                (length $ C.txIns tx0)
                (length $ C.txOuts tx0)
                0
                (length keyWitnesses)
                networkId
                txBody0 :: C.Lovelace

  when (howMuch + fee >= totalLovelace) $ fail "Not enough funds"
  let
    tx = tx0 { C.txFee = C.TxFeeExplicit (txFeesExplicitInShelleyBasedEra C.shelleyBasedEra) fee
             , C.txOuts = [ mkAddressAdaTxOut to howMuch
                          , mkAddressAdaTxOut from $ totalLovelace - howMuch - fee
                          ]}
  txBody :: C.TxBody era <- HE.leftFail $ C.makeTransactionBody tx
  return (C.signShelleyTransaction txBody keyWitnesses, txBody)

mkAddressValueTxOut
  :: (C.IsShelleyBasedEra era)
  => C.Address C.ShelleyAddr -> C.TxOutValue era -> C.TxOut ctx era
mkAddressValueTxOut address value = C.TxOut
  (C.AddressInEra (C.ShelleyAddressInEra C.shelleyBasedEra) address)
  value
  C.TxOutDatumNone
  C.ReferenceScriptNone

mkAddressAdaTxOut
    :: (C.IsShelleyBasedEra era)
    => C.Address C.ShelleyAddr -> C.Lovelace -> C.TxOut ctx era
mkAddressAdaTxOut address lovelace =
    let txOutValue =
            case C.multiAssetSupportedInEra $ C.shelleyBasedToCardanoEra C.shelleyBasedEra of
              Left adaOnlyInEra     -> C.TxOutAdaOnly adaOnlyInEra lovelace
              Right multiAssetInEra -> C.TxOutValue multiAssetInEra $ C.lovelaceToValue lovelace
     in mkAddressValueTxOut address txOutValue

-- | Adapted from:
-- https://github.com/input-output-hk/cardano-node/blob/d15ff2b736452857612dd533c1ddeea2405a2630/cardano-cli/src/Cardano/CLI/Shelley/Run/Transaction.hs#L1105-L1112
-- https://github.com/input-output-hk/cardano-node/blob/d15ff2b736452857612dd533c1ddeea2405a2630/cardano-cli/src/Cardano/CLI/Shelley/Run/Transaction.hs#L1121-L1128
calculateFee :: C.IsShelleyBasedEra era => C.ProtocolParameters -> Int -> Int -> Int -> Int -> C.NetworkId -> C.TxBody era -> C.Lovelace
calculateFee pparams nInputs nOutputs nByronKeyWitnesses nShelleyKeyWitnesses networkId txBody = C.estimateTransactionFee
  networkId
  (C.protocolParamTxFeeFixed pparams)
  (C.protocolParamTxFeePerByte pparams)
  (C.makeSignedTransaction [] txBody)
  nInputs nOutputs
  nByronKeyWitnesses nShelleyKeyWitnesses

-- | Add fee to transaction body, return transaction with the fee
-- applied, and also the fee in lovelace.
calculateAndUpdateTxFee
  :: H.MonadTest m
  => C.ProtocolParameters -> C.NetworkId -> Int -> Int
  -> C.TxBodyContent C.BuildTx C.AlonzoEra -> m (C.Lovelace, C.TxBodyContent C.BuildTx C.AlonzoEra)
calculateAndUpdateTxFee pparams networkId lengthTxIns lengthKeyWitnesses txbc = do
  txb <- HE.leftFail $ C.makeTransactionBody txbc
  let
    feeLovelace = calculateFee pparams lengthTxIns (length $ C.txOuts txbc) 0 lengthKeyWitnesses networkId txb :: C.Lovelace
    fee = C.TxFeeExplicit C.TxFeesExplicitInAlonzoEra feeLovelace
    txbc' = txbc { C.txFee = fee }
  return (feeLovelace, txbc')

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

setDarwinTmpdir :: IO ()
setDarwinTmpdir = when (IO.os == "darwin") $ IO.setEnv "TMPDIR" "/tmp"

-- * Accessors

bimTxIds :: C.BlockInMode mode -> [C.TxId]
bimTxIds (C.BlockInMode block _) = blockTxIds block

blockTxIds :: C.Block era -> [C.TxId]
blockTxIds (C.Block (C.BlockHeader _slotNo _ _blockNo) txs) = map (C.getTxId . C.getTxBody) txs

bimSlotNo :: C.BlockInMode mode -> C.SlotNo
bimSlotNo (C.BlockInMode (C.Block (C.BlockHeader slotNo _ _blockNo) _txs) _era) = slotNo

bimBlockNo :: C.BlockInMode mode -> C.BlockNo
bimBlockNo (C.BlockInMode (C.Block (C.BlockHeader _slotNo _ blockNo) _txs) _era) = blockNo

-- | Should probably move in `cardano-api`.
txFeesExplicitInShelleyBasedEra
    :: C.ShelleyBasedEra era
    -> C.TxFeesExplicitInEra era
txFeesExplicitInShelleyBasedEra shelleyBased =
    case shelleyBased of
      C.ShelleyBasedEraShelley -> C.TxFeesExplicitInShelleyEra
      C.ShelleyBasedEraAllegra -> C.TxFeesExplicitInAllegraEra
      C.ShelleyBasedEraMary    -> C.TxFeesExplicitInMaryEra
      C.ShelleyBasedEraAlonzo  -> C.TxFeesExplicitInAlonzoEra
      C.ShelleyBasedEraBabbage -> C.TxFeesExplicitInBabbageEra

addressAnyToShelley
  :: C.AddressAny
  -> Maybe (C.Address C.ShelleyAddr)
addressAnyToShelley  (C.AddressShelley a) = Just a
addressAnyToShelley  _                    = Nothing
