{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -Wno-orphans    #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Integration (tests) where

import Codec.Serialise (serialise)
import Control.Concurrent qualified as IO
import Control.Concurrent.STM qualified as IO
import Control.Exception (catch)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Stack qualified as GHC
import Streaming.Prelude qualified as S
import System.Directory qualified as IO
import System.Environment qualified as IO
import System.FilePath ((</>))
import System.IO.Temp qualified as IO
import System.Info qualified as IO

import Hedgehog (MonadTest, Property, assert, (===))
import Hedgehog qualified as H
import Hedgehog.Extras.Stock.IO.Network.Sprocket qualified as IO
import Hedgehog.Extras.Test qualified as HE
import Hedgehog.Extras.Test.Base qualified as H
import Test.Tasty (TestTree, testGroup)
-- import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logError)
import Cardano.BM.Tracing (defaultConfigStdout)
import Cardano.Streaming (ChainSyncEventException (NoIntersectionFound), withChainSyncEventStream)
import Gen.Cardano.Api.Typed qualified as CGen
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (SubmitFail, SubmitSuccess))
import Plutus.V1.Ledger.Scripts qualified as Plutus
import PlutusTx qualified
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)
import Test.Base qualified as H
import Testnet.Cardano qualified as TN
import Testnet.Conf qualified as TC (Conf (..), ProjectBase (ProjectBase), YamlFilePath (YamlFilePath), mkConf)

import Hedgehog.Extras qualified as H
import Marconi.Index.ScriptTx qualified as ScriptTx
import Marconi.Indexers qualified as M
import Marconi.Logging ()

tests :: TestTree
tests = testGroup "Integration"
  [ -- Uncomment when bug #775 on github is fixed (PLT-1068)
    -- testPropertyNamed "prop_script_hash_in_local_testnet_tx_match" "testIndex" testIndex
  ]

{- | We test the script transaction indexer by setting up a testnet,
   adding a script to it and then spending it, and then see if the
   script shows up on the indexer.

   Specifically, we:

    - spin up a testnet and the script transaction indexer

    - create a plutus script, then submit a transaction which contains
      an UTxO where this script is the validator

    - submit a second transaction that spends this UTxO, then query
      the indexer to see if it was indexed properly
-}
testIndex :: Property
testIndex = H.integration . HE.runFinallies . workspace "chairman" $ \tempAbsBasePath' -> do

  base <- HE.noteM $ liftIO . IO.canonicalizePath =<< HE.getProjectBase
  (socketPathAbs, networkId, tempAbsPath) <- startTestnet base tempAbsBasePath'

  -- Create a channel that is passed into the indexer, such that it
  -- can write index updates to it and we can await for them (also
  -- making us not need threadDelay)
  indexedTxs <- liftIO IO.newChan
  let writeScriptUpdate (ScriptTx.ScriptTxUpdate txScripts _slotNo) = case txScripts of
        (x : xs) -> IO.writeChan indexedTxs $ x :| xs
        _        -> pure ()

  -- Start indexer
  let sqliteDb = tempAbsPath </> "script-tx.db"
  indexer <- liftIO $ do

    coordinator <- M.initialCoordinator 1
    ch <- IO.atomically . IO.dupTChan $ M._channel coordinator
    (loop, indexer) <- M.scriptTxWorker_ (\_ update -> writeScriptUpdate update $> []) (ScriptTx.Depth 0) coordinator ch sqliteDb

    -- Receive ChainSyncEvents and pass them on to indexer's channel
    void $ IO.forkIO $ do
      let chainPoint = C.ChainPointAtGenesis :: C.ChainPoint
      c <- defaultConfigStdout
      withTrace c "marconi" $ \trace -> let
        indexerWorker = withChainSyncEventStream socketPathAbs networkId chainPoint $ S.mapM_ $
          \chainSyncEvent -> IO.atomically $ IO.writeTChan ch chainSyncEvent
        handleException NoIntersectionFound = logError trace $ renderStrict $ layoutPretty defaultLayoutOptions $
          "No intersection found for chain point" <+> pretty chainPoint <> "."
        in indexerWorker `catch` handleException :: IO ()

    -- Start indexer worker loop
    void $ IO.forkIO loop

    return indexer

  utxoVKeyFile <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo1.vkey"
  utxoSKeyFile <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo1.skey"

  let
    -- Create an always succeeding validator script
    plutusScript :: C.PlutusScript C.PlutusScriptV1
    plutusScript = C.PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Plutus.unValidatorScript validator
      where
        validator :: Plutus.Validator
        validator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| \_ _ _ -> () ||])

    plutusScriptHash = C.hashScript $ C.PlutusScript C.PlutusScriptV1 plutusScript :: C.ScriptHash
    plutusScriptAddr :: C.Address C.ShelleyAddr
    plutusScriptAddr = C.makeShelleyAddress networkId (C.PaymentCredentialByScript plutusScriptHash) C.NoStakeAddress

  -- Step 1: Create a tx ouput with a datum hash at the script address. In order for a tx ouput to be locked
  -- by a plutus script, it must have a datahash. We also need collateral tx inputs so we split the utxo
  -- in order to accomodate this.

  genesisVKey :: C.VerificationKey C.GenesisUTxOKey <-
    readAs (C.AsVerificationKey C.AsGenesisUTxOKey) utxoVKeyFile
  genesisSKey :: C.SigningKey C.GenesisUTxOKey <-
    readAs (C.AsSigningKey C.AsGenesisUTxOKey) utxoSKeyFile

  let
    paymentKey = C.castVerificationKey genesisVKey :: C.VerificationKey C.PaymentKey
    address :: C.Address C.ShelleyAddr
    address = C.makeShelleyAddress
      networkId
      (C.PaymentCredentialByKey (C.verificationKeyHash paymentKey :: C.Hash C.PaymentKey))
      C.NoStakeAddress :: C.Address C.ShelleyAddr

  -- Boilerplate codecs used for protocol serialisation.  The number
  -- of epochSlots is specific to each blockchain instance. This value
  -- what the cardano main and testnet uses. Only applies to the Byron
  -- era.
  let epochSlots = C.EpochSlots 21600
      localNodeConnectInfo =
        C.LocalNodeConnectInfo
          { C.localConsensusModeParams = C.CardanoModeParams epochSlots
          , C.localNodeNetworkId = networkId
          , C.localNodeSocketPath = socketPathAbs
          }

  (tx1in, C.TxOut _ v _ _) <- do
    utxo <- findUTxOByAddress localNodeConnectInfo (C.toAddressAny address)
    headM $ Map.toList $ C.unUTxO utxo
  let totalLovelace = C.txOutValueToLovelace v

  pparams <- H.leftFailM . H.leftFailM . liftIO
    $ C.queryNodeLocalState localNodeConnectInfo Nothing
    $ C.QueryInEra C.AlonzoEraInCardanoMode
    $ C.QueryInShelleyBasedEra C.ShelleyBasedEraAlonzo C.QueryProtocolParameters

  let scriptDatum = C.ScriptDataNumber 42 :: C.ScriptData
      scriptDatumHash = C.hashScriptData scriptDatum

      tx1fee = 271 :: C.Lovelace
      amountPaid = 10_000_000 :: C.Lovelace -- 10 ADA
      -- Must return everything that was not paid to script and that didn't went to fees:
      amountReturned = totalLovelace - amountPaid - tx1fee :: C.Lovelace

      txOut1 :: C.TxOut ctx C.AlonzoEra
      txOut1 =
        C.TxOut
          (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraAlonzo) plutusScriptAddr)
          (C.TxOutValue C.MultiAssetInAlonzoEra $ C.lovelaceToValue amountPaid)
          (C.TxOutDatumHash C.ScriptDataInAlonzoEra scriptDatumHash)
          C.ReferenceScriptNone
      txOut2 :: C.TxOut ctx C.AlonzoEra
      txOut2 =
        C.TxOut
          (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraAlonzo) address)
          (C.TxOutValue C.MultiAssetInAlonzoEra $ C.lovelaceToValue amountReturned)
          C.TxOutDatumNone
          C.ReferenceScriptNone
      txBodyContent :: C.TxBodyContent C.BuildTx C.AlonzoEra
      txBodyContent =
        C.TxBodyContent {
          C.txIns              = [(tx1in, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)],
          C.txInsCollateral    = C.TxInsCollateralNone,
          C.txInsReference     = C.TxInsReferenceNone,
          C.txOuts             = [txOut1, txOut2],
          C.txTotalCollateral  = C.TxTotalCollateralNone,
          C.txReturnCollateral = C.TxReturnCollateralNone,
          C.txFee              = C.TxFeeExplicit C.TxFeesExplicitInAlonzoEra tx1fee,
          C.txValidityRange    = (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInAlonzoEra),
          C.txMetadata         = C.TxMetadataNone,
          C.txAuxScripts       = C.TxAuxScriptsNone,
          C.txExtraKeyWits     = C.TxExtraKeyWitnessesNone,
          C.txProtocolParams   = C.BuildTxWith $ Just pparams,
          C.txWithdrawals      = C.TxWithdrawalsNone,
          C.txCertificates     = C.TxCertificatesNone,
          C.txUpdateProposal   = C.TxUpdateProposalNone,
          C.txMintValue        = C.TxMintNone,
          C.txScriptValidity   = C.TxScriptValidityNone
        }
  tx1body :: C.TxBody C.AlonzoEra <- H.leftFail $ C.makeTransactionBody txBodyContent
  let
    kw :: C.KeyWitness C.AlonzoEra
    kw = C.makeShelleyKeyWitness tx1body (C.WitnessPaymentKey $ C.castSigningKey genesisSKey)
    tx1 = C.makeSignedTransaction [kw] tx1body

  submitTx localNodeConnectInfo tx1

  -- Second transaction: spend the UTXO specified in the first transaction

  _ <- liftIO $ IO.readChan indexedTxs -- wait for the first transaction to be accepted

  tx2collateralTxIn <- headM . Map.keys . C.unUTxO =<< findUTxOByAddress localNodeConnectInfo (C.toAddressAny address)

  (scriptTxIn, C.TxOut _ valueAtScript _ _) <- do
    scriptUtxo <- findUTxOByAddress localNodeConnectInfo $ C.toAddressAny plutusScriptAddr
    headM $ Map.toList $ C.unUTxO scriptUtxo

  let lovelaceAtScript = C.txOutValueToLovelace valueAtScript
  assert $ lovelaceAtScript == 10_000_000 -- script has the 10 ADA we put there in tx1

  redeemer <- H.forAll CGen.genScriptData -- The script always returns true so any redeemer will do
  let
      -- The following execution unit and fee values were found by
      -- trial and error. When the transaction which we are in the
      -- process of creating, fails, then it will report the values it
      -- wants. And although they change again after you correct them,
      -- then the procedure converges quickly.
      executionUnits = C.ExecutionUnits {C.executionSteps = 500_000, C.executionMemory = 10_000 }
      tx2fee = 1000303 :: C.Lovelace

      scriptWitness :: C.Witness C.WitCtxTxIn C.AlonzoEra
      scriptWitness = C.ScriptWitness C.ScriptWitnessForSpending $
        C.PlutusScriptWitness C.PlutusScriptV1InAlonzo C.PlutusScriptV1 (C.PScript plutusScript)
        (C.ScriptDatumForTxIn scriptDatum) redeemer executionUnits

      collateral = C.TxInsCollateral C.CollateralInAlonzoEra [tx2collateralTxIn]

      tx2out :: C.TxOut ctx C.AlonzoEra
      tx2out =
          C.TxOut
            (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraAlonzo) address)
             -- send ADA back to the original genesis address               ^
            (C.TxOutValue C.MultiAssetInAlonzoEra $ C.lovelaceToValue $ lovelaceAtScript - tx2fee)
            C.TxOutDatumNone
            C.ReferenceScriptNone

      tx2bodyContent :: C.TxBodyContent C.BuildTx C.AlonzoEra
      tx2bodyContent =
        C.TxBodyContent {
          C.txIns              = [(scriptTxIn, C.BuildTxWith scriptWitness)],
          C.txInsCollateral    = collateral,
          C.txInsReference     = C.TxInsReferenceNone,
          C.txOuts             = [tx2out],
          C.txTotalCollateral  = C.TxTotalCollateralNone,
          C.txReturnCollateral = C.TxReturnCollateralNone,
          C.txFee              = C.TxFeeExplicit C.TxFeesExplicitInAlonzoEra tx2fee,
          C.txValidityRange    = (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInAlonzoEra),
          C.txMetadata         = C.TxMetadataNone,
          C.txAuxScripts       = C.TxAuxScriptsNone,
          C.txExtraKeyWits     = C.TxExtraKeyWitnessesNone,
          C.txProtocolParams   = C.BuildTxWith $ Just pparams,
          C.txWithdrawals      = C.TxWithdrawalsNone,
          C.txCertificates     = C.TxCertificatesNone,
          C.txUpdateProposal   = C.TxUpdateProposalNone,
          C.txMintValue        = C.TxMintNone,
          C.txScriptValidity   = C.TxScriptValidityNone
        }

  tx2body :: C.TxBody C.AlonzoEra <- H.leftFail $ C.makeTransactionBody tx2bodyContent
  let tx2 = C.signShelleyTransaction tx2body [C.WitnessGenesisUTxOKey genesisSKey]

  submitTx localNodeConnectInfo tx2


  {- Test if what the indexer got is what we sent.

  We test both of (1) what we get from `onInsert` callback and (2)
  with `query` (what ends up in the sqlite database). For the `query`
  we currently need to use threadDelay and poll to query the database
  because the indexer runs in a separate thread and there is no way
  of awaiting the data to be flushed into the database. -}

  (ScriptTx.TxCbor tx, indexedScriptHashes) :| _ <- liftIO $ IO.readChan indexedTxs

  ScriptTx.ScriptAddress indexedScriptHash <- headM indexedScriptHashes

  indexedTx2 :: C.Tx C.AlonzoEra <- H.leftFail $ C.deserialiseFromCBOR (C.AsTx C.AsAlonzoEra) tx

  plutusScriptHash === indexedScriptHash
  tx2 === indexedTx2

  -- The query poll
  queriedTx2 :: C.Tx C.AlonzoEra <- do
    let
      queryLoop n = do
        H.threadDelay 250_000 -- wait 250ms before querying
        txCbors <- liftIO $ ScriptTx.query indexer (ScriptTx.ScriptAddress plutusScriptHash) []
        case txCbors of
          result : _ -> pure result
          _          -> queryLoop (n + 1)
    ScriptTx.TxCbor txCbor <- queryLoop (0 :: Integer)
    H.leftFail $ C.deserialiseFromCBOR (C.AsTx C.AsAlonzoEra) txCbor

  tx2 === queriedTx2

-- * Helpers

startTestnet :: FilePath -> FilePath -> H.Integration (String, C.NetworkId, FilePath)
startTestnet base tempAbsBasePath' = do
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf@TC.Conf { TC.tempBaseAbsPath, TC.tempAbsPath } <- HE.noteShowM $
    TC.mkConf (TC.ProjectBase base) (TC.YamlFilePath configurationTemplate)
      (tempAbsBasePath' <> "/")
      Nothing
  assert $ tempAbsPath == (tempAbsBasePath' <> "/")
        && tempAbsPath == (tempBaseAbsPath <> "/")
  tn <- TN.testnet TN.defaultTestnetOptions conf
  let networkId = C.Testnet $ C.NetworkMagic $ fromIntegral (TN.testnetMagic tn)
  socketPath <- IO.sprocketArgumentName <$> headM (TN.nodeSprocket <$> TN.bftNodes tn)
  socketPathAbs <- H.note =<< (liftIO $ IO.canonicalizePath $ tempAbsPath </> socketPath)
  pure (socketPathAbs, networkId, tempAbsPath)

readAs :: (C.HasTextEnvelope a, MonadIO m, MonadTest m) => C.AsType a -> FilePath -> m a
readAs as path = H.leftFailM . liftIO $ C.readFileTextEnvelope as path

findUTxOByAddress
  :: (MonadIO m, MonadTest m)
  => C.LocalNodeConnectInfo C.CardanoMode -> C.AddressAny -> m (C.UTxO C.AlonzoEra)
findUTxOByAddress localNodeConnectInfo address = let
  query = C.QueryInShelleyBasedEra C.ShelleyBasedEraAlonzo $ C.QueryUTxO $
    C.QueryUTxOByAddress $ Set.singleton address
  in
  H.leftFailM . H.leftFailM . liftIO $ C.queryNodeLocalState localNodeConnectInfo Nothing $
    C.QueryInEra C.AlonzoEraInCardanoMode query

submitTx :: (MonadIO m, MonadTest m) => C.LocalNodeConnectInfo C.CardanoMode -> C.Tx C.AlonzoEra -> m ()
submitTx localNodeConnectInfo tx = do
  submitResult :: SubmitResult (C.TxValidationErrorInMode C.CardanoMode) <-
    liftIO $ C.submitTxToNodeLocal localNodeConnectInfo $ C.TxInMode tx C.AlonzoEraInCardanoMode
  failOnTxSubmitFail submitResult
  where
    failOnTxSubmitFail :: (Show a, MonadTest m) => SubmitResult a -> m ()
    failOnTxSubmitFail = \case
      SubmitFail reason -> H.failMessage GHC.callStack $ "Transaction failed: " <> show reason
      SubmitSuccess     -> pure ()

-- TODO: remove when this is exported from hedgehog-extras/src/Hedgehog/Extras/Test/Base.hs
headM :: (MonadTest m, GHC.HasCallStack) => [a] -> m a
headM (a:_) = return a
headM []    = GHC.withFrozenCallStack $ H.failMessage GHC.callStack "Cannot take head of empty list"

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
  -- liftIO $ IO.writeFile (ws <> "/module") callerModuleName
  f ws
  when (IO.os /= "mingw32" && maybeKeepWorkspace /= Just "1") $ do
    H.evalIO $ IO.removeDirectoryRecursive ws
