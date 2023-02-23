{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}

module Spec.Marconi.ChainIndex.Indexers.MintBurn where

import Codec.Serialise (serialise)
import Control.Concurrent qualified as IO
import Control.Concurrent.Async qualified as IO
import Control.Concurrent.STM qualified as IO
import Control.Exception (catch)
import Control.Lens qualified as Lens
import Control.Monad (foldM, forM, replicateM, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Word (Word64)
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)
import Streaming.Prelude qualified as S
import System.Directory qualified as IO
import System.FilePath ((</>))

import Hedgehog (Gen, Property, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Extras.Test qualified as HE
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logError)
import Cardano.BM.Tracing (defaultConfigStdout)
import Cardano.Streaming (ChainSyncEventException (NoIntersectionFound), withChainSyncEventStream)
import Gen.Cardano.Api.Typed qualified as CGen
import Plutus.V1.Ledger.Api qualified as PlutusV1
import PlutusTx qualified
import Test.Base qualified as H
import Testnet.Cardano qualified as TN

import Helpers qualified as TN
import Marconi.ChainIndex.Indexers qualified as M
import Marconi.ChainIndex.Indexers.MintBurn qualified as MintBurn
import Marconi.ChainIndex.Logging ()
import Marconi.Core.Storable qualified as RI

-- | Each test case is described beside every top level property
-- declaration.
tests :: TestTree
tests = testGroup "MintBurn"
  [ testPropertyNamed
      "Mints in `TxBodyContent` survive `makeTransactionBody` and end up in expected place in `TxBody`"
      "mintsPreserved" mintsPreserved
  , testPropertyNamed
      "Mint events created and indexed are returned when querying the indexer"
      "queryMintedValues" queryMintedValues
  , testPropertyNamed
      "Querying a resumed indexer returns only the persisted events"
      "resume" resume
  , testPropertyNamed
      "Rewinding to any slot forgets any newer events than that slot"
      "rewind" rewind
  , testPropertyNamed
      "Intervals work as expected"
      "intervals" intervals
  , testPropertyNamed
      "Indexing a testnet and then submitting a transaction with a mint event to it has the indexer receive that mint event"
      "endToEnd" endToEnd
  ]

-- | This is a sanity-check test that turns a TxBodyContent with mint
-- events into a TxBody through `makeTransactionBody` and checks if
-- the mint events are found in the result. It doesn't test an
-- indexer.
mintsPreserved :: Property
mintsPreserved = H.property $ do
  mintValue <- forAll genTxMintValue
  C.Tx txb _ :: C.Tx C.AlonzoEra <- forAll (genTxWithMint mintValue) >>= \case
    Left err  -> fail $ "TxBodyError: " <> show err
    Right tx' -> return tx'
  -- Index the transaction:
  let mints = MintBurn.txbMints txb
      gottenPolicyAssets = map (\mint -> (MintBurn.mintAssetPolicyId mint, MintBurn.mintAssetAssetName mint, MintBurn.mintAssetQuantity mint)) mints
  -- Print footnote should the test fail:
  let generatedPolicyAssets = getPolicyAssets mintValue
  H.footnote $ "Assets to be created: " <> show generatedPolicyAssets <> "\n"
            <> "Assets gotten: " <> show gottenPolicyAssets
  -- The assets that were used to construct the transaction were found
  -- in the generate transaction:
  equalSet generatedPolicyAssets gottenPolicyAssets

-- | Create transactions, index them, query indexer and find mint events.
queryMintedValues :: Property
queryMintedValues = H.property $ do
  (indexer, insertedEvents, _) <- generateAndIndexEvents ":memory:"
  -- Query results:
  MintBurn.MintBurnResult queryResult <- liftIO $ RI.query RI.QEverything indexer MintBurn.Everything
  -- Compare the sets of events inserted to the indexer and the set
  -- gotten out of the indexer:
  equalSet (MintBurn.groupBySlotAndHash insertedEvents) (MintBurn.fromRows queryResult)

-- | Insert some events to an indexer, then recreate it from what is
-- on disk (the in-memory part is lost), then query it and find all
-- persisted events and none of the in-memory events.
resume :: Property
resume = H.property $ do
  -- Index events that overflow:
  (indexer, events, (bufferSize, _nTx)) <- generateAndIndexEvents ":memory:"
  -- Open a new indexer based off of the old indexers sql connection:
  indexer' <- liftIO $ mkNewIndexerBasedOnOldDb indexer
  MintBurn.MintBurnResult queryResult <- liftIO $ RI.query RI.QEverything indexer' MintBurn.Everything
  let expected = MintBurn.groupBySlotAndHash $ take (eventsPersisted bufferSize (length events)) events
  -- The test: events that were persisted are exactly those we get from the query.
  equalSet expected (MintBurn.fromRows queryResult)

-- | Test that rewind (rollback for on-disk events) behaves as
-- expected: insert events such that buffer overflows, rollback so far
-- back that some events were already persisted, find no newer events
-- than rollback point in query.
rewind :: Property
rewind = H.property $ do
  (indexer, events, (_bufferSize, nTx)) <- generateAndIndexEvents ":memory:"
  -- Rollback slot is from 0 to number of slots (slot numbers are from 0 to nTx - 1)
  rollbackSlotNo <- fmap coerce $ forAll $ Gen.integral $ Range.constant 0 ((let w64 = fromIntegral nTx in if w64 == 0 then 0 else w64 - 1) :: Word64)
  let cp = C.ChainPoint rollbackSlotNo dummyBlockHeaderHash
  rewoundIndexer <- let errMsg = "Failed to rewind! This shouldn't happen and the test should be fixed"
    in maybe (fail errMsg) pure =<< liftIO (RI.rewind cp indexer)
  MintBurn.MintBurnResult queryResult <- liftIO $ RI.query RI.QEverything rewoundIndexer MintBurn.Everything
  -- Expect only older than rollback events.
  let expected = filter (\e -> MintBurn.txMintEventSlotNo e <= rollbackSlotNo) events
  equalSet expected (MintBurn.fromRows queryResult)

-- | Test that interval query works.
intervals :: Property
intervals = H.property $ do
  (indexer, events, (_bufferSize, _nTx)) <- generateAndIndexEvents ":memory:"

  let
    cpFromSlot slotNo = C.ChainPoint slotNo dummyBlockHeaderHash
    queryInterval from to = do
      MintBurn.MintBurnResult queryResult <- liftIO $ RI.query (RI.QInterval from to) indexer MintBurn.Everything
      pure $ MintBurn.fromRows queryResult

  -- Genesis to genesis returns nothing
  H.assert . null =<< queryInterval C.ChainPointAtGenesis C.ChainPointAtGenesis
  -- When there were at least one event created:
  when (not $ null events) $ do
    let eventCp e = cpFromSlot $ MintBurn.txMintEventSlotNo e
    -- From genesis to "latest slot + 1" returns everything:
    equalSet events =<< (queryInterval C.ChainPointAtGenesis $ cpFromSlot $ (MintBurn.txMintEventSlotNo (last events)) + 1)
    -- From first event's slot to last event's slot returns everything:
    equalSet events =<< queryInterval (eventCp $ head events) (eventCp $ last events)
    -- Form any slot to genesis returns nothing
    ix <- forAll $ Gen.integral $ Range.constant 0 (length events - 1)
    H.assert . null =<< queryInterval (cpFromSlot $ MintBurn.txMintEventSlotNo $ events !! ix) C.ChainPointAtGenesis

    -- TODO: Enable the following test when there is an API for
    -- getting an actual interval, and/or add another test for when
    -- the meaning of queryInterval is clear.
    --
    -- Form any existing earlier slot to any existing same-or-later slot:
    -- (from, to) <- do
    --   a' <- forAll $ Gen.integral $ Range.constant 0 (length events - 1)
    --   b' <- forAll $ Gen.integral $ Range.constant 0 (length events - 1)
    --   let a = MintBurn.txMintEventSlotNo $ events !! a'
    --       b = MintBurn.txMintEventSlotNo $ events !! b'
    --   return $ if a <= b then (a, b) else (b, a)
    -- let expected = filter (\e -> let slotNo = MintBurn.txMintEventSlotNo e in from <= slotNo && slotNo <= to) events
    -- equalSet expected =<< queryInterval (cpFromSlot from) (cpFromSlot to)

-- | Start testnet, start mint/burn indexer on it, create a single
-- mint event, put it in a transaction and submit it, find the
-- generated event passed back through the indexer.
endToEnd :: Property
endToEnd = H.withShrinks 0 $ H.integration $ (liftIO TN.setDarwinTmpdir >>) $ HE.runFinallies $ H.workspace "." $ \tempPath -> do
  base <- HE.noteM $ liftIO . IO.canonicalizePath =<< HE.getProjectBase
  (localNodeConnectInfo, conf, runtime) <- TN.startTestnet TN.defaultTestnetOptions base tempPath
  let networkId = TN.getNetworkId runtime
  socketPath <- TN.getSocketPathAbs conf runtime

  -- This is the channel we wait on to know if the event has been indexed
  indexedTxs <- liftIO IO.newChan
  -- Start indexer
  liftIO $ do
    coordinator <- M.initialCoordinator 1
    ch <- IO.atomically . IO.dupTChan $ M._channel coordinator
    (loop, _indexerMVar) <- M.mintBurnWorker_ 123 (IO.writeChan indexedTxs) coordinator ch (tempPath </> "db.db")
    void $ IO.async loop
    -- Receive ChainSyncEvents and pass them on to indexer's channel
    void $ IO.async $ do
      let chainPoint = C.ChainPointAtGenesis :: C.ChainPoint
      c <- defaultConfigStdout
      withTrace c "marconi" $ \trace -> let
        indexerWorker = withChainSyncEventStream socketPath networkId [chainPoint] $ S.mapM_ $
          \chainSyncEvent -> IO.atomically $ IO.writeTChan ch chainSyncEvent
        handleException NoIntersectionFound = logError trace $ renderStrict $ layoutPretty defaultLayoutOptions $
          "No intersection found for chain point" <+> pretty chainPoint <> "."
        in indexerWorker `catch` handleException :: IO ()

  -- Create & submit transaction
  pparams <- TN.getProtocolParams @C.AlonzoEra localNodeConnectInfo
  txMintValue <- forAll genTxMintValue

  genesisVKey :: C.VerificationKey C.GenesisUTxOKey <- TN.readAs (C.AsVerificationKey C.AsGenesisUTxOKey) $ tempPath </> "shelley/utxo-keys/utxo1.vkey"
  genesisSKey :: C.SigningKey C.GenesisUTxOKey <- TN.readAs (C.AsSigningKey C.AsGenesisUTxOKey) $ tempPath </> "shelley/utxo-keys/utxo1.skey"
  let paymentKey = C.castVerificationKey genesisVKey :: C.VerificationKey C.PaymentKey
      address :: C.Address C.ShelleyAddr
      address = C.makeShelleyAddress
        networkId
        (C.PaymentCredentialByKey (C.verificationKeyHash paymentKey :: C.Hash C.PaymentKey))
        C.NoStakeAddress :: C.Address C.ShelleyAddr

  value <- H.fromJustM $ getValue txMintValue
  (txIns, lovelace) <- TN.getAddressTxInsValue @C.AlonzoEra localNodeConnectInfo address
  let fee = 500 :: C.Lovelace
      amountReturned = lovelace - fee :: C.Lovelace
      txOut :: C.TxOut ctx C.AlonzoEra
      txOut =
        C.TxOut
          (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraAlonzo) address)
          (C.TxOutValue C.MultiAssetInAlonzoEra $ C.lovelaceToValue amountReturned <> value)
          C.TxOutDatumNone
          C.ReferenceScriptNone
      txBodyContent :: C.TxBodyContent C.BuildTx C.AlonzoEra
      txBodyContent =
          (TN.emptyTxBodyContent
              (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInAlonzoEra)
              (C.TxFeeExplicit C.TxFeesExplicitInAlonzoEra fee)
              pparams
          ) { C.txIns = map (, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) txIns
            , C.txOuts = [txOut]
            , C.txProtocolParams = C.BuildTxWith $ Just pparams
            , C.txMintValue = txMintValue
            , C.txInsCollateral = C.TxInsCollateral C.CollateralInAlonzoEra txIns
            }
  txBody :: C.TxBody C.AlonzoEra <- H.leftFail $ C.makeTransactionBody txBodyContent
  let
    kw :: C.KeyWitness C.AlonzoEra
    kw = C.makeShelleyKeyWitness txBody (C.WitnessPaymentKey $ C.castSigningKey genesisSKey)
    tx = C.makeSignedTransaction [kw] txBody
  TN.submitTx localNodeConnectInfo tx

  -- Receive event from the indexer, compare the mint that we
  -- submitted above with the one we got from the indexer.
  event <- liftIO $ IO.readChan indexedTxs
  case MintBurn.txMintEventTxAssets event of
     (_txId, gottenMintEvents :: NonEmpty MintBurn.MintAsset) :| [] -> let
       in equalSet (mintsToPolicyAssets $ NonEmpty.toList gottenMintEvents) (getPolicyAssets txMintValue)
     _ -> fail "More than one mint/burn event, but we created only one!"

-- * Generators

-- | The workhorse of the test: generate an indexer, then generate
-- transactions to index, then index them.
generateAndIndexEvents :: FilePath -> H.PropertyT IO (MintBurn.MintBurnIndexer, [MintBurn.TxMintEvent], (Int, Int))
generateAndIndexEvents dbPath = do
  (events, (bufferSize, nTx)) <- forAll generateTxsWithMints
  -- Report buffer overflow:
  let overflow = bufferSize < length events
  H.classify "No events created at all" $ null events
  H.classify "Buffer doesn't overflow" $ not (null events) && not overflow
  H.classify "Buffer overflows" $ not (null events) && overflow
  indexer <- liftIO $ do
    indexer <- MintBurn.open dbPath bufferSize
    foldM (\indexer' event -> RI.insert (MintBurn.MintBurnEvent event) indexer') indexer events
  pure (indexer, events, (bufferSize, nTx))

-- | Generate transactions which have mints inside, then extract
-- TxMintEvent's from these, then return them with buffer size and
-- number of transactions.
generateTxsWithMints :: Gen ([MintBurn.TxMintEvent], (Int, Int))
generateTxsWithMints = do
  bufferSize <- Gen.integral (Range.constant 1 10)
  nTx <- Gen.choice                                                   -- Number of events:
    [ Gen.constant 0                                                  --  1. no events generated
    , Gen.integral $ Range.constant 0 bufferSize                      --  2. buffer not filled
    , Gen.integral $ Range.constant (bufferSize + 1) (bufferSize * 2) --  3. guaranteed buffer overflow
    ]
  -- Generate transactions
  txAll' <- forM [0 .. (nTx - 1)] $ \slotNoInt -> do
    tx <- genTxWithMint =<< genTxMintValue
    pure (tx, fromIntegral slotNoInt :: C.SlotNo)
  -- Filter out Left C.TxBodyError
  txAll <- forM txAll' $ \case
    (Right tx, slotNo) -> pure (tx, slotNo)
    (Left txBodyError, _) -> fail $ "Failed to create a transaction! This shouldn't happen, the generator should be fixed. TxBodyError: " <> show txBodyError
  let events = mapMaybe (\(tx, slotNo) -> MintBurn.TxMintEvent slotNo dummyBlockHeaderHash . pure <$> MintBurn.txMints tx) txAll
  pure (events, (bufferSize, nTx))

genTxWithMint :: C.TxMintValue C.BuildTx C.AlonzoEra -> Gen (Either C.TxBodyError (C.Tx C.AlonzoEra))
genTxWithMint txMintValue = do
  txbc <- CGen.genTxBodyContent C.AlonzoEra
  txIn <- CGen.genTxIn
  pparams' :: C.ProtocolParameters <- CGen.genProtocolParameters
  let
    pparams = C.BuildTxWith $ Just pparams'
      { C.protocolParamUTxOCostPerWord = Just 1
      , C.protocolParamPrices = Just $ C.ExecutionUnitPrices 1 1
      , C.protocolParamMaxTxExUnits = Just $ C.ExecutionUnits 1 1
      , C.protocolParamMaxBlockExUnits = Just $ C.ExecutionUnits 1 1
      , C.protocolParamMaxValueSize = Just 1
      , C.protocolParamCollateralPercent = Just 1
      , C.protocolParamMaxCollateralInputs = Just 1
      }
    txbc' = txbc
      { C.txMintValue = txMintValue
      , C.txInsCollateral = C.TxInsCollateral C.CollateralInAlonzoEra [txIn]
      , C.txProtocolParams = pparams
      }
  pure $ do
    txb <- C.makeTransactionBody txbc'
    pure $ C.signShelleyTransaction txb []

-- | Helper to create tx with @commonMintingPolicy@, @assetName@ and @quantity@
genTxWithAsset :: C.AssetName -> C.Quantity -> Gen (Either C.TxBodyError (C.Tx C.AlonzoEra))
genTxWithAsset assetName quantity = genTxWithMint $ C.TxMintValue C.MultiAssetInAlonzoEra mintedValues (C.BuildTxWith $ Map.singleton policyId policyWitness)
  where (policyId, policyWitness, mintedValues) = mkMintValue commonMintingPolicy [(assetName, quantity)]

genTxMintValue :: Gen (C.TxMintValue C.BuildTx C.AlonzoEra)
genTxMintValue = do
  n :: Int <- Gen.integral (Range.constant 1 5)
  -- n :: Int <- Gen.integral (Range.constant 0 5)
  -- TODO: fix bug RewindableIndex.Storable.rewind and change range to start from 0.
  policyAssets <- replicateM n genAsset
  let (policyId, policyWitness, mintedValues) = mkMintValue commonMintingPolicy policyAssets
  pure $ C.TxMintValue C.MultiAssetInAlonzoEra mintedValues (C.BuildTxWith $ Map.singleton policyId policyWitness)
  where
    genAsset :: Gen (C.AssetName, C.Quantity)
    genAsset = (,) <$> genAssetName <*> genQuantity
      where
        genAssetName = coerce @_ @C.AssetName <$> Gen.bytes (Range.constant 1 5)
        genQuantity = coerce @Integer @C.Quantity <$> Gen.integral (Range.constant 1 100)

-- * Helpers

-- | Remove events that remained in buffer.
onlyPersisted :: Int -> [a] -> [a]
onlyPersisted bufferSize events = take (eventsPersisted bufferSize $ length events) $ events

eventsPersisted :: Int -> Int -> Int
eventsPersisted bufferSize nEvents = let
  -- Number of buffer flushes
  bufferFlushesN = let
    (n, m) = nEvents `divMod` bufferSize
    in if m == 0 then n - 1 else n
  -- Number of events persisted
  numberOfEventsPersisted = bufferFlushesN * bufferSize
  in numberOfEventsPersisted

mkMintValue
  :: PlutusV1.MintingPolicy -> [(C.AssetName, C.Quantity)]
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint C.AlonzoEra, C.Value)
mkMintValue policy policyAssets = (policyId, policyWitness, mintedValues)
  where
    serialisedPolicyScript :: C.PlutusScript C.PlutusScriptV1
    serialisedPolicyScript = C.PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ PlutusV1.unMintingPolicyScript policy

    policyId :: C.PolicyId
    policyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV1 serialisedPolicyScript :: C.PolicyId

    executionUnits :: C.ExecutionUnits
    executionUnits = C.ExecutionUnits {C.executionSteps = 300000, C.executionMemory = 1000 }
    redeemer :: C.ScriptData
    redeemer = C.fromPlutusData $ PlutusV1.toData ()
    policyWitness :: C.ScriptWitness C.WitCtxMint C.AlonzoEra
    policyWitness = C.PlutusScriptWitness C.PlutusScriptV1InAlonzo C.PlutusScriptV1
      (C.PScript serialisedPolicyScript) C.NoScriptDatumForMint redeemer executionUnits

    mintedValues :: C.Value
    mintedValues = C.valueFromList $ map (\(assetName, quantity) -> (C.AssetId policyId assetName, quantity)) policyAssets

commonMintingPolicy :: PlutusV1.MintingPolicy
commonMintingPolicy = PlutusV1.mkMintingPolicyScript $$(PlutusTx.compile [||\_ _ -> ()||])

-- | Recreate an indexe, useful because the sql connection to a
-- :memory: database can be reused.
mkNewIndexerBasedOnOldDb :: RI.State MintBurn.MintBurnHandle -> IO (RI.State MintBurn.MintBurnHandle)
mkNewIndexerBasedOnOldDb indexer = let
    MintBurn.MintBurnHandle sqlCon k = Lens.view RI.handle indexer
  in RI.emptyState k (MintBurn.MintBurnHandle sqlCon k)

dummyBlockHeaderHash :: C.Hash C.BlockHeader
dummyBlockHeaderHash = fromString "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef" :: C.Hash C.BlockHeader

equalSet :: (H.MonadTest m, Show a, Ord a) => [a] -> [a] -> m ()
equalSet a b = Set.fromList a === Set.fromList b

getPolicyAssets :: C.TxMintValue C.BuildTx C.AlonzoEra -> [(C.PolicyId, C.AssetName, C.Quantity)]
getPolicyAssets txMintValue = case txMintValue of
  (C.TxMintValue C.MultiAssetInAlonzoEra mintedValues (C.BuildTxWith _policyIdToWitnessMap)) ->
    mapMaybe (\(assetId, quantity) -> case assetId of
             C.AssetId policyId assetName -> Just (policyId, assetName, quantity)
             C.AdaAssetId                 -> Nothing
        ) $ C.valueToList mintedValues
  _ -> []

getValue :: C.TxMintValue C.BuildTx C.AlonzoEra -> Maybe C.Value
getValue = \case
  C.TxMintValue C.MultiAssetInAlonzoEra value (C.BuildTxWith _policyIdToWitnessMap) -> Just value
  _                                                                                 -> Nothing

mintsToPolicyAssets :: [MintBurn.MintAsset] -> [(C.PolicyId, C.AssetName, C.Quantity)]
mintsToPolicyAssets mints =
  map (\mint -> (MintBurn.mintAssetPolicyId mint, MintBurn.mintAssetAssetName mint, MintBurn.mintAssetQuantity mint)) mints
