{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Helpers where

import Control.Concurrent qualified as IO
import Control.Concurrent.Async qualified as IO
import Control.Monad (join, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Data.List as List
import Data.Map qualified as Map
import Data.Map.Lazy (isSubmapOf)
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
--import Cardano.Streaming qualified as CS
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (SubmitFail, SubmitSuccess))
import Test.Runtime qualified as TN
import Testnet.Babbage qualified as TN
import Testnet.Conf qualified as TC (Conf (..), ProjectBase (ProjectBase), YamlFilePath (YamlFilePath), mkConf)

getProjectBase :: (MonadIO m, MonadTest m) => m String
getProjectBase = liftIO . IO.canonicalizePath =<< HE.getProjectBase

unsafeFromEither :: Show l => Either l r -> r
unsafeFromEither (Left err)    = error (show err)
unsafeFromEither (Right value) = value

unsafeFromMaybe :: Maybe a -> a
unsafeFromMaybe Nothing  = error "not maybe, nothing."
unsafeFromMaybe (Just a) = a

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

-- | Start a testnet.
startTestnet :: TN.TestnetOptions
  -> FilePath
  -> FilePath
  -> H.Integration (C.LocalNodeConnectInfo C.CardanoMode, C.ProtocolParameters, C.NetworkId)
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
  socketPathAbs <- getPoolSocketPathAbs conf tn
  let epochSlots = C.EpochSlots 21600
      localNodeConnectInfo =
        C.LocalNodeConnectInfo
          { C.localConsensusModeParams = C.CardanoModeParams epochSlots
          , C.localNodeNetworkId = getNetworkId tn
          , C.localNodeSocketPath = socketPathAbs
          }
      networkId = getNetworkId tn
  pparams <- getBabbageProtocolParams localNodeConnectInfo
  liftIO $ IO.setEnv "CARDANO_NODE_SOCKET_PATH" socketPathAbs -- set node socket environment for Cardano.Api.Convenience.Query
  pure (localNodeConnectInfo, pparams, networkId)

getNetworkId :: TN.TestnetRuntime -> C.NetworkId
getNetworkId tn = C.Testnet $ C.NetworkMagic $ fromIntegral (TN.testnetMagic tn)

getPoolSocketPathAbs :: (MonadTest m, MonadIO m) => TC.Conf -> TN.TestnetRuntime -> m FilePath
getPoolSocketPathAbs conf tn = do
  let tempAbsPath = TC.tempAbsPath conf
  socketPath <- IO.sprocketArgumentName <$> H.headM (TN.poolNodeSprocket <$> TN.poolNodes tn)
  socketPathAbs <- H.note =<< (liftIO $ IO.canonicalizePath $ tempAbsPath </> socketPath)
  pure socketPathAbs

getBabbageProtocolParams :: (MonadIO m, MonadTest m) => C.LocalNodeConnectInfo C.CardanoMode -> m C.ProtocolParameters
getBabbageProtocolParams localNodeConnectInfo = H.leftFailM . H.leftFailM . liftIO
  $ C.queryNodeLocalState localNodeConnectInfo Nothing
  $ C.QueryInEra C.BabbageEraInCardanoMode
  $ C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage C.QueryProtocolParameters

readAs :: (C.HasTextEnvelope a, MonadIO m, MonadTest m) => C.AsType a -> FilePath -> m a
readAs as path = do
  path' <- H.note path
  H.leftFailM . liftIO $ C.readFileTextEnvelope as path'

w1 :: (MonadIO m, MonadTest m)
  => FilePath
  -> C.NetworkId
  -> m (C.SigningKey C.GenesisUTxOKey, C.Address C.ShelleyAddr)
w1 tempAbsPath' networkId = do
  genesisVKey :: C.VerificationKey C.GenesisUTxOKey <-
    readAs (C.AsVerificationKey C.AsGenesisUTxOKey) $ tempAbsPath' </> "utxo-keys/utxo1.vkey"
  genesisSKey :: C.SigningKey C.GenesisUTxOKey <-
    readAs (C.AsSigningKey C.AsGenesisUTxOKey) $ tempAbsPath' </> "utxo-keys/utxo1.skey"

  let
    paymentKey = C.castVerificationKey genesisVKey :: C.VerificationKey C.PaymentKey
    address :: C.Address C.ShelleyAddr
    address = C.makeShelleyAddress
      networkId
      (C.PaymentCredentialByKey (C.verificationKeyHash paymentKey :: C.Hash C.PaymentKey))
      C.NoStakeAddress :: C.Address C.ShelleyAddr

  return (genesisSKey, address)

txOutNoDatumOrRefScript :: C.Value -> C.Address C.ShelleyAddr -> C.TxOut ctx C.BabbageEra
txOutNoDatumOrRefScript value address = C.TxOut
    (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) address)
    (C.TxOutValue C.MultiAssetInBabbageEra value)
    C.TxOutDatumNone
    C.ReferenceScriptNone

firstTxIn :: (MonadIO m, MonadTest m) => C.LocalNodeConnectInfo C.CardanoMode -> C.Address C.ShelleyAddr -> m C.TxIn
firstTxIn = txInFromUtxo 0

txInFromUtxo :: (MonadIO m, MonadTest m) => Int -> C.LocalNodeConnectInfo C.CardanoMode -> C.Address C.ShelleyAddr -> m C.TxIn
txInFromUtxo i localNodeConnectInfo address = do
  atM i =<< txInsFromUtxo =<< findUTxOByAddress localNodeConnectInfo address
  where
    atM :: (MonadTest m) => Int -> [a] -> m a
    atM i' l = return $ l !! i'

txInsFromUtxo :: (MonadIO m) => C.UTxO C.BabbageEra -> m [C.TxIn]
txInsFromUtxo utxos = do
  let (txIns, _) = unzip $ Map.toList $ C.unUTxO utxos
  return txIns

findUTxOByAddress
  :: (MonadIO m, MonadTest m)
  => C.LocalNodeConnectInfo C.CardanoMode -> C.Address a -> m (C.UTxO C.BabbageEra)
findUTxOByAddress localNodeConnectInfo address = let
  query = C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage $ C.QueryUTxO $
    C.QueryUTxOByAddress $ Set.singleton (C.toAddressAny address)
  in
  H.leftFailM . H.leftFailM . liftIO $ C.queryNodeLocalState localNodeConnectInfo Nothing $
    C.QueryInEra C.BabbageEraInCardanoMode query

-- | Get [TxIn] and total lovelace value for an address.
getAddressTxInsLovelaceValue
  :: (MonadIO m, MonadTest m)
  => C.LocalNodeConnectInfo C.CardanoMode -> C.Address a -> m ([C.TxIn], C.Lovelace)
getAddressTxInsLovelaceValue con address = do
  utxo <- findUTxOByAddress con address
  let
    (txIns, txOuts) = unzip $ Map.toList $ C.unUTxO utxo
    values = map (\case C.TxOut _ v _ _ -> C.txOutValueToLovelace v) txOuts
  pure (txIns, sum values)

-- | Get [TxIn] and value for an address (including assets).
getAddressTxInsValue
  :: (MonadIO m, MonadTest m)
  => C.LocalNodeConnectInfo C.CardanoMode -> C.Address a -> m ([C.TxIn], C.Value)
getAddressTxInsValue con address = do
  utxo <- findUTxOByAddress con address
  let
    (txIns, txOuts) = unzip $ Map.toList $ C.unUTxO utxo
    values = map (\case C.TxOut _ v _ _ -> C.txOutValueToValue v) txOuts
  pure (txIns, (mconcat values))

-- valueAtAddressTxIn:: (MonadIO m, MonadTest m) -- at single txo
--   => C.LocalNodeConnectInfo C.CardanoMode
--   -> C.Address C.ShelleyAddr
--   -> C.Value
-- valueAtAddressUtxo localNodeConnectInfo address expValue = do
--     (_, value) <- getAddressTxInsValue localNodeConnectInfo address

-- | An empty transaction
emptyTxBodyContent :: C.ProtocolParameters -> C.TxBodyContent C.BuildTx C.BabbageEra
emptyTxBodyContent pparams = C.TxBodyContent
  { C.txIns              = []
  , C.txInsCollateral    = C.TxInsCollateralNone
  , C.txInsReference     = C.TxInsReferenceNone
  , C.txOuts             = []
  , C.txTotalCollateral  = C.TxTotalCollateralNone
  , C.txReturnCollateral = C.TxReturnCollateralNone
  , C.txFee              = C.TxFeeExplicit C.TxFeesExplicitInBabbageEra 0
  , C.txValidityRange    = (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra)
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

txInFromSignedTx :: C.Tx era -> Int -> C.TxIn
txInFromSignedTx signedTx txIx = C.TxIn (C.getTxId $ C.getTxBody signedTx) (C.TxIx $ fromIntegral txIx)

pubkeyTxIns  :: [C.TxIn] -> [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra))]
pubkeyTxIns txIns = map (\txIn -> (txIn, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)) txIns

txMintValue :: C.Value-> Map.Map C.PolicyId (C.ScriptWitness C.WitCtxMint C.BabbageEra)
  -> C.TxMintValue C.BuildTx C.BabbageEra
txMintValue tv m = C.TxMintValue C.MultiAssetInBabbageEra tv (C.BuildTxWith m)

buildTx :: (MonadIO m, MonadTest m)
  => C.TxBodyContent C.BuildTx C.BabbageEra
  -> C.Address C.ShelleyAddr
  -> C.SigningKey C.GenesisUTxOKey
  -> C.NetworkId
  -> m (C.Tx C.BabbageEra)
buildTx txBody changeAddress sKey networkId = do
  (nodeEraUtxo, pparams, eraHistory, systemStart, stakePools) <- H.leftFailM . liftIO $
    C.queryStateForBalancedTx C.BabbageEra networkId (fst <$> C.txIns txBody)

  return $ unsafeFromEither $ C.constructBalancedTx
    C.BabbageEraInCardanoMode
    txBody
    (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) changeAddress)
    Nothing -- Override key witnesses
    nodeEraUtxo -- tx inputs
    pparams
    eraHistory
    systemStart
    stakePools
    [C.WitnessPaymentKey $ C.castSigningKey sKey]

submitTx :: (MonadIO m, MonadTest m)
  => C.LocalNodeConnectInfo C.CardanoMode
  -> C.Tx C.BabbageEra
  -> m ()
submitTx localNodeConnectInfo tx = do
  submitResult :: SubmitResult (C.TxValidationErrorInMode C.CardanoMode) <-
    liftIO $ C.submitTxToNodeLocal localNodeConnectInfo $ C.TxInMode tx C.BabbageEraInCardanoMode
  failOnTxSubmitFail submitResult
  where
    failOnTxSubmitFail :: (Show a, MonadTest m) => SubmitResult a -> m ()
    failOnTxSubmitFail = \case
      SubmitFail reason -> H.failMessage GHC.callStack $ "Transaction failed: " <> show reason
      SubmitSuccess     -> pure ()

--TODO: loop timeout
waitForTxIdAtAddress :: (MonadIO m, MonadTest m)
  => C.LocalNodeConnectInfo C.CardanoMode
  -> C.Address C.ShelleyAddr
  -> C.TxId
  -> m ()
waitForTxIdAtAddress localNodeConnectInfo address txId = do
  let loop = do
        txIns <- txInsFromUtxo =<< findUTxOByAddress localNodeConnectInfo address
        let txIds = map (\(C.TxIn txId _) -> txId) txIns
        when (not $ txId `elem` txIds) loop
  loop

--TODO: loop timeout
waitForTxInAtAddress :: (MonadIO m, MonadTest m)
  => C.LocalNodeConnectInfo C.CardanoMode
  -> C.Address C.ShelleyAddr
  -> C.TxIn
  -> m ()
waitForTxInAtAddress localNodeConnectInfo address txIn = do
  let loop = do
        utxos <- findUTxOByAddress localNodeConnectInfo address
        when (Map.notMember txIn $ C.unUTxO utxos) loop
  loop

--   utxos <- findUTxOByAddress localNodeConnectInfo address
--   return $ unsafeFromMaybe $ Map.lookup txIn $ C.unUTxO utxos -- $ findUTxOByAddress localNodeConnectInfo address

getTxOutAtAddress :: (MonadIO m, MonadTest m)
  => C.LocalNodeConnectInfo C.CardanoMode
  -> C.Address C.ShelleyAddr
  -> C.TxIn
  -> m (C.TxOut C.CtxUTxO C.BabbageEra)
getTxOutAtAddress localNodeConnectInfo address txIn = do
  waitForTxInAtAddress localNodeConnectInfo address txIn
  utxos <- findUTxOByAddress localNodeConnectInfo address
  return $ unsafeFromMaybe $ Map.lookup txIn $ C.unUTxO utxos

txOutHasValue :: (MonadIO m, MonadTest m)
  => C.TxOut C.CtxUTxO C.BabbageEra
  -> C.Value
  -> m Bool
txOutHasValue (C.TxOut _ txOutValue _ _) tokenValue = do
  let value = C.txOutValueToValue txOutValue
  return $ List.isInfixOf (C.valueToList tokenValue) (C.valueToList value)

{-
| Block until a transaction with @txId@ is sent over the local chainsync protocol.
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
        when (not $ txId `elem` txIds) loop
  loop

| Submit the argument transaction and await for it to be accepted into the blockhain.
submitAwaitTx
  :: (MonadIO m, MonadTest m)
  => C.LocalNodeConnectInfo C.CardanoMode -> (C.Tx C.AlonzoEra, C.TxBody C.AlonzoEra) -> m ()
submitAwaitTx con (tx, txBody) = do
  submitTx con tx
  liftIO $ awaitTxId con $ C.getTxId txBody


mkTransferTx
  :: (MonadIO m, MonadTest m, MonadFail m)
  => C.NetworkId -> C.LocalNodeConnectInfo C.CardanoMode -> C.Address C.ShelleyAddr -> C.Address C.ShelleyAddr -> [C.ShelleyWitnessSigningKey] -> C.Lovelace
  -> m (C.Tx C.BabbageEra, C.TxBody C.BabbageEra)
mkTransferTx networkId con from to keyWitnesses howMuch = do
  pparams <- getBabbageProtocolParams con
  (txIns, totalLovelace) <- getAddressTxInsLovelaceValue con from
  let
    fee0 = 0
    tx0 = (emptyTxBodyContent fee0 pparams)
      { C.txIns = map (, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) txIns
      , C.txOuts = [mkAddressAdaTxOut to $ totalLovelace - fee0]
      }
  txBody0 :: C.TxBody C.BabbageEra <- HE.leftFail $ C.makeTransactionBody tx0
  let fee = calculateFee pparams (length $ C.txIns tx0) (length $ C.txOuts tx0) 0 (length keyWitnesses) networkId txBody0 :: C.Lovelace

  when (howMuch + fee >= totalLovelace) $ fail "Not enough funds"
  let
    tx = tx0 { C.txFee = C.TxFeeExplicit C.TxFeesExplicitInBabbageEra fee
             , C.txOuts = [ mkAddressAdaTxOut to howMuch
                          , mkAddressAdaTxOut from $ totalLovelace - howMuch - fee
                          ]}
  txBody :: C.TxBody C.BabbageEra <- HE.leftFail $ C.makeTransactionBody tx
  return (C.signShelleyTransaction txBody keyWitnesses, txBody)

mkAddressAdaTxOut :: C.Address C.ShelleyAddr -> C.Lovelace -> C.TxOut ctx C.BabbageEra
mkAddressAdaTxOut address lovelace =
  C.TxOut
    (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) address)
    (C.TxOutValue C.MultiAssetInBabbageEra $ C.lovelaceToValue lovelace)
    C.TxOutDatumNone
    C.ReferenceScriptNone

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

-- * Accessors

bimTxIds :: C.BlockInMode mode -> [C.TxId]
bimTxIds (C.BlockInMode block _) = blockTxIds block

blockTxIds :: C.Block era -> [C.TxId]
blockTxIds (C.Block (C.BlockHeader _slotNo _ _blockNo) txs) = map (C.getTxId . C.getTxBody) txs

bimSlotNo :: C.BlockInMode mode -> C.SlotNo
bimSlotNo (C.BlockInMode (C.Block (C.BlockHeader slotNo _ _blockNo) _txs) _era) = slotNo

bimBlockNo :: C.BlockInMode mode -> C.BlockNo
bimBlockNo (C.BlockInMode (C.Block (C.BlockHeader _slotNo _ blockNo) _txs) _era) = blockNo
-}
