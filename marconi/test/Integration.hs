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
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Streaming.Prelude qualified as S
import System.Directory qualified as IO
import System.FilePath ((</>))

import Hedgehog (Property, assert, (===))
import Hedgehog qualified as H
import Hedgehog.Extras.Test qualified as HE
import Hedgehog.Extras.Test.Base qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logError)
import Cardano.BM.Tracing (defaultConfigStdout)
import Cardano.Streaming (ChainSyncEventException (NoIntersectionFound), withChainSyncEventStream)
import Gen.Cardano.Api.Typed qualified as CGen
import Plutus.V1.Ledger.Scripts qualified as Plutus
import PlutusTx qualified
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)
import Test.Base qualified as H

import Helpers qualified as TN
import Testnet.Cardano qualified as TN
-- ^ Although these are defined in this cabal component, they are
-- helpers for interacting with the testnet, thus TN

import Hedgehog.Extras qualified as H
import Marconi.Index.ScriptTx qualified as ScriptTx
import Marconi.Indexers qualified as M
import Marconi.Logging ()

tests :: TestTree
tests = testGroup "Integration"
  [ testPropertyNamed "prop_script_hash_in_local_testnet_tx_match" "testIndex" testIndex
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
testIndex = H.integration . HE.runFinallies . TN.workspace "chairman" $ \tempAbsPath -> do

  base <- HE.noteM $ liftIO . IO.canonicalizePath =<< HE.getProjectBase

  (localNodeConnectInfo, conf, runtime) <- TN.startTestnet TN.defaultTestnetOptions base tempAbsPath
  let networkId = TN.getNetworkId runtime
  socketPathAbs <- TN.getSocketPathAbs conf runtime

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
    TN.readAs (C.AsVerificationKey C.AsGenesisUTxOKey) $ tempAbsPath </> "shelley/utxo-keys/utxo1.vkey"
  genesisSKey :: C.SigningKey C.GenesisUTxOKey <-
    TN.readAs (C.AsSigningKey C.AsGenesisUTxOKey) $ tempAbsPath </> "shelley/utxo-keys/utxo1.skey"

  let
    paymentKey = C.castVerificationKey genesisVKey :: C.VerificationKey C.PaymentKey
    address :: C.Address C.ShelleyAddr
    address = C.makeShelleyAddress
      networkId
      (C.PaymentCredentialByKey (C.verificationKeyHash paymentKey :: C.Hash C.PaymentKey))
      C.NoStakeAddress :: C.Address C.ShelleyAddr

  (tx1in, C.TxOut _ v _ _) <- do
    utxo <- TN.findUTxOByAddress localNodeConnectInfo address
    H.headM $ Map.toList $ C.unUTxO utxo
  let totalLovelace = C.txOutValueToLovelace v

  pparams <- TN.getAlonzoProtocolParams localNodeConnectInfo

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
      txBodyContent = (TN.emptyTxBodyContent tx1fee pparams)
        { C.txIns = [(tx1in, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)]
        , C.txOuts = [txOut1, txOut2]
        , C.txProtocolParams   = C.BuildTxWith $ Just pparams
        }
  tx1body :: C.TxBody C.AlonzoEra <- H.leftFail $ C.makeTransactionBody txBodyContent
  let
    kw :: C.KeyWitness C.AlonzoEra
    kw = C.makeShelleyKeyWitness tx1body (C.WitnessPaymentKey $ C.castSigningKey genesisSKey)
    tx1 = C.makeSignedTransaction [kw] tx1body

  TN.submitTx localNodeConnectInfo tx1

  -- Second transaction: spend the UTXO specified in the first transaction

  _ <- liftIO $ IO.readChan indexedTxs -- wait for the first transaction to be accepted

  tx2collateralTxIn <- H.headM . Map.keys . C.unUTxO =<< TN.findUTxOByAddress localNodeConnectInfo address

  (scriptTxIn, C.TxOut _ valueAtScript _ _) <- do
    scriptUtxo <- TN.findUTxOByAddress localNodeConnectInfo plutusScriptAddr
    H.headM $ Map.toList $ C.unUTxO scriptUtxo

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

  TN.submitTx localNodeConnectInfo tx2


  {- Test if what the indexer got is what we sent.

  We test both of (1) what we get from `onInsert` callback and (2)
  with `query` (what ends up in the sqlite database). For the `query`
  we currently need to use threadDelay and poll to query the database
  because the indexer runs in a separate thread and there is no way
  of awaiting the data to be flushed into the database. -}

  indexedWithScriptHashes <- liftIO $ IO.readChan indexedTxs

  -- We have to filter out the txs the empty scripts hashes because
  -- sometimes the RollForward event contains a block with the first transaction 'tx1'
  -- which has no scripts. The test fails because of that in 'headM indexedScriptHashes'.
  -- For more details see https://github.com/input-output-hk/plutus-apps/issues/775
  let (ScriptTx.TxCbor tx, indexedScriptHashes) = head $ (NE.filter (\(_, hashes) -> hashes /= [])) indexedWithScriptHashes

  ScriptTx.ScriptAddress indexedScriptHash <- H.headM indexedScriptHashes

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
