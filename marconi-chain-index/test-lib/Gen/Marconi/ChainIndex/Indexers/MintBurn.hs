{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Gen.Marconi.ChainIndex.Indexers.MintBurn
    ( genIndexWithEvents
    , genMintEvents
    , genTxWithMint
    , genTxMintValue
    )
where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Codec.Serialise (serialise)
import Control.Lens ((^.))
import Control.Monad (foldM, forM, replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Coerce (coerce)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.String (fromString)
import Gen.Cardano.Api.Typed qualified as CGen
import Hedgehog (Gen, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Marconi.ChainIndex.Indexers.MintBurn qualified as MintBurn
import Marconi.ChainIndex.Logging ()
import Marconi.ChainIndex.Types (SecurityParam)
import Marconi.Core.Storable qualified as Storable
import Plutus.V1.Ledger.Api (MintingPolicy)
import Plutus.V1.Ledger.Api qualified as PlutusV1
import PlutusTx qualified

-- | The workhorse of the test: generate an indexer, then generate
-- transactions to index, then index them.
genIndexWithEvents
    :: FilePath
    -> H.PropertyT IO (MintBurn.MintBurnIndexer, [MintBurn.TxMintEvent], (SecurityParam, Int))
genIndexWithEvents dbPath = do
  (events, (bufferSize, nTx)) <- forAll genMintEvents
  -- Report buffer overflow:
  let overflow = fromIntegral bufferSize < length events
  H.classify "No events created at all" $ null events
  H.classify "Buffer doesn't overflow" $ not (null events) && not overflow
  H.classify "Buffer overflows" $ not (null events) && overflow
  indexer <- liftIO $ do
    indexer <- MintBurn.open dbPath bufferSize
    foldM (\indexer' event -> Storable.insert (MintBurn.MintBurnEvent event) indexer') indexer events
  pure (indexer, events, (bufferSize, nTx))

-- | Generate transactions which have mints inside, then extract
-- TxMintEvent's from these, then return them with buffer size and
-- number of transactions.
genMintEvents :: Gen ([MintBurn.TxMintEvent], (SecurityParam, Int))
genMintEvents = do
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
  pure (events, (fromIntegral bufferSize, nTx))

genTxWithMint
    :: C.TxMintValue C.BuildTx C.AlonzoEra
    -> Gen (Either C.TxBodyError (C.Tx C.AlonzoEra))
genTxWithMint txMintValue = do
  txbc <- CGen.genTxBodyContent C.AlonzoEra
  txIn <- CGen.genTxIn
  pparams' :: C.ProtocolParameters <- CGen.genProtocolParameters
  let
    pparams = C.BuildTxWith $ Just pparams'
      { C.protocolParamUTxOCostPerByte = Just 1
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
onlyPersisted bufferSize events = take (eventsPersisted bufferSize $ length events) events

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
  :: MintingPolicy -> [(C.AssetName, C.Quantity)]
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
    mintedValues = C.valueFromList $ map (first (C.AssetId policyId)) policyAssets

commonMintingPolicy :: MintingPolicy
commonMintingPolicy = PlutusV1.mkMintingPolicyScript $$(PlutusTx.compile [||\_ _ -> ()||])

-- | Recreate an indexe, useful because the sql connection to a
-- :memory: database can be reused.
mkNewIndexerBasedOnOldDb :: Storable.State MintBurn.MintBurnHandle -> IO (Storable.State MintBurn.MintBurnHandle)
mkNewIndexerBasedOnOldDb indexer = let
    MintBurn.MintBurnHandle sqlCon k = indexer ^. Storable.handle
  in Storable.emptyState (fromIntegral k) (MintBurn.MintBurnHandle sqlCon k)

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
mintsToPolicyAssets =
  map (\mint -> ( MintBurn.mintAssetPolicyId mint
                , MintBurn.mintAssetAssetName mint
                , MintBurn.mintAssetQuantity mint
                ))
