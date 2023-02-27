{-# LANGUAGE NamedFieldPuns #-}

module Gen.Marconi.ChainIndex.Types
  ( nonEmptySubset
  , genBlockHeader
  , genHashBlockHeader
  , genBlockNo
  , genChainPoints
  , genChainPoint
  , genChainPoint'
  , genExecutionUnits
  , genSlotNo
  , genTxBodyContentWithTxInsCollateral
  , genTxBodyWithTxIns
  , genTxIndex
  , genWitnessAndHashInEra
  , genTxOutTxContext
  , genAddressInEra
  , genTxOutValue
  , genSimpleScriptData
  , genProtocolParametersForPlutusScripts
  , genHashScriptData
  , genAssetId
  , genPolicyId
  , genQuantity
  ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Binary qualified as CBOR
import Cardano.Crypto.Hash.Class qualified as CRYPTO
import Cardano.Ledger.SafeHash (unsafeMakeSafeHash)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as BSS
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Ratio (Ratio, (%))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Word (Word64)
import GHC.Natural (Natural)
import Gen.Cardano.Api.Typed qualified as CGen
import Hedgehog (Gen, MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (Range)
import Hedgehog.Range qualified as Range
import PlutusCore (defaultCostModelParams)

nonEmptySubset :: (MonadGen m, Ord a) => Set a -> m (Set a)
nonEmptySubset s = do
    e <- Gen.element (Set.toList s)
    sub <- Gen.subset s
    pure $ Set.singleton e <> sub

genSlotNo :: Hedgehog.MonadGen m => m C.SlotNo
genSlotNo = C.SlotNo <$> Gen.word64 (Range.linear 10 1000)

genBlockNo :: Hedgehog.MonadGen m => m C.BlockNo
genBlockNo = C.BlockNo <$> Gen.word64 (Range.linear 100 1000)

validByteSizeLength :: Int
validByteSizeLength = 32

genBlockHeader
  :: Hedgehog.MonadGen m
  => m C.BlockNo
  -> m C.SlotNo
  -> m C.BlockHeader
genBlockHeader genB genS = do
  bs <- Gen.bytes(Range.singleton validByteSizeLength)
  sn <- genS
  bn <- genB
  let (hsh :: C.Hash C.BlockHeader) =
        fromJust $ C.deserialiseFromRawBytes(C.proxyToAsType Proxy) bs
  pure (C.BlockHeader sn hsh bn)

genHashBlockHeader :: (MonadGen m) => m (C.Hash C.BlockHeader)
genHashBlockHeader = C.HeaderHash . BSS.toShort <$> Gen.bytes (Range.singleton 32)

genChainPoints :: (MonadGen m) => Word64 -> Word64 -> m [C.ChainPoint]
genChainPoints b e = do
    maxSlots <- Gen.word64 (Range.linear b e)
    mapM (\s -> C.ChainPoint (C.SlotNo s) <$> genHashBlockHeader) [0..maxSlots]

genChainPoint'
  :: Hedgehog.MonadGen m
  => m C.BlockNo
  -> m C.SlotNo
  -> m C.ChainPoint
genChainPoint' genB genS = do
  (C.BlockHeader sn hsh _) <- genBlockHeader genB genS
  pure $ C.ChainPoint sn hsh

genChainPoint :: Hedgehog.MonadGen m => m C.ChainPoint
genChainPoint =
  Gen.frequency
  [ (95, genChainPoint' genBlockNo genSlotNo)
  , (5, pure C.ChainPointAtGenesis)
  ]

genTxIndex :: Gen C.TxIx
genTxIndex = C.TxIx . fromIntegral <$> Gen.word16 Range.constantBounded

genTxBodyWithTxIns
  :: C.IsCardanoEra era
  => C.CardanoEra era
  -> [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era))]
  -> C.TxInsCollateral era
  -> Gen (C.TxBody era)
genTxBodyWithTxIns era txIns txInsCollateral = do
  txBodyContent <- genTxBodyContentWithTxInsCollateral era txIns txInsCollateral
  case C.makeTransactionBody txBodyContent of
    Left err     -> fail $ C.displayError err
    Right txBody -> pure txBody

genTxBodyContentWithTxInsCollateral
  :: C.CardanoEra era
  -> [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era))]
  -> C.TxInsCollateral era
  -> Gen (C.TxBodyContent C.BuildTx era)
genTxBodyContentWithTxInsCollateral era txIns txInsCollateral = do
  txbody <- CGen.genTxBodyContent era
  txProtocolParams <- C.BuildTxWith . Just <$> CGen.genProtocolParameters
  pure $ txbody
    { C.txIns
    , C.txInsCollateral
    , C.txProtocolParams
    }

genWitnessAndHashInEra :: C.CardanoEra era -> Gen (C.Witness C.WitCtxTxIn era, C.ScriptHash)
genWitnessAndHashInEra era = do
  C.ScriptInEra scriptLanguageInEra script <- CGen.genScriptInEra era
  witness :: C.Witness C.WitCtxTxIn era1 <- C.ScriptWitness C.ScriptWitnessForSpending <$> case script of
    C.PlutusScript version plutusScript -> do
      scriptData <- CGen.genScriptData
      executionUnits <- genExecutionUnits
      pure $ C.PlutusScriptWitness
        scriptLanguageInEra
        version
        (C.PScript plutusScript)
        (C.ScriptDatumForTxIn scriptData)
        scriptData
        executionUnits
    C.SimpleScript version simpleScript ->
      pure $ C.SimpleScriptWitness scriptLanguageInEra version (C.SScript simpleScript)
  pure (witness, C.hashScript script)

-- | TODO Copy-paste from cardano-node: cardano-api/gen/Gen/Cardano/Api/Typed.hs
-- Copied from cardano-api. Delete when this function is reexported
genExecutionUnits :: Gen C.ExecutionUnits
genExecutionUnits = C.ExecutionUnits <$> Gen.integral (Range.constant 0 1000)
                                     <*> Gen.integral (Range.constant 0 1000)

genTxOutTxContext :: C.CardanoEra era -> Gen (C.TxOut C.CtxTx era)
genTxOutTxContext era =
  C.TxOut <$> genAddressInEra era
          <*> genTxOutValue era
          <*> genSimpleTxOutDatumHashTxContext era
          <*> constantReferenceScript era

-- Copied from cardano-api. Delete when this function is reexported
genAddressInEra :: C.CardanoEra era -> Gen (C.AddressInEra era)
genAddressInEra era =
  case C.cardanoEraStyle era of
    C.LegacyByronEra ->
      C.byronAddressInEra <$> CGen.genAddressByron

    C.ShelleyBasedEra _ ->
      Gen.choice
        [ C.byronAddressInEra   <$> CGen.genAddressByron
        , C.shelleyAddressInEra <$> CGen.genAddressShelley
        ]

-- Copied from cardano-api. Delete when this function is reexported
genTxOutValue :: C.CardanoEra era -> Gen (C.TxOutValue era)
genTxOutValue era =
  case C.multiAssetSupportedInEra era of
    Left adaOnlyInEra     -> C.TxOutAdaOnly adaOnlyInEra <$> fmap (<> 1) CGen.genLovelace
    Right multiAssetInEra -> C.TxOutValue multiAssetInEra . C.lovelaceToValue <$> fmap (<> 1) CGen.genLovelace

-- Copied from cardano-api, but removed the recursive construction because it is time consuming,
-- about a factor of 20 when compared to this simple generator.
genSimpleScriptData :: Gen C.ScriptData
genSimpleScriptData =
    Gen.choice
        [ C.ScriptDataNumber <$> genInteger
        , C.ScriptDataBytes  <$> genByteString
        , C.ScriptDataConstructor <$> genInteger <*> pure []
        , pure $ C.ScriptDataList []
        , pure $ C.ScriptDataMap []
        ]
  where
    genInteger :: Gen Integer
    genInteger = Gen.integral
                  (Range.linear
                    0
                    (fromIntegral (maxBound :: Word64) :: Integer))

    genByteString :: Gen ByteString
    genByteString = BS.pack <$> Gen.list (Range.linear 0 64)
                                         (Gen.word8 Range.constantBounded)

constantReferenceScript :: C.CardanoEra era -> Gen (C.ReferenceScript era)
constantReferenceScript era =
  case C.refInsScriptsAndInlineDatsSupportedInEra era of
    Nothing -> return C.ReferenceScriptNone
    Just supp -> pure
               $ C.ReferenceScript supp
               $ C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1)
               $ C.PlutusScript C.PlutusScriptV1
               $ C.examplePlutusScriptAlwaysSucceeds C.WitCtxTxIn

genSimpleTxOutDatumHashTxContext :: C.CardanoEra era -> Gen (C.TxOutDatum C.CtxTx era)
genSimpleTxOutDatumHashTxContext era = case era of
    C.ByronEra   -> pure C.TxOutDatumNone
    C.ShelleyEra -> pure C.TxOutDatumNone
    C.AllegraEra -> pure C.TxOutDatumNone
    C.MaryEra    -> pure C.TxOutDatumNone
    C.AlonzoEra  -> Gen.choice
                    [ pure C.TxOutDatumNone
                    , C.TxOutDatumHash C.ScriptDataInAlonzoEra <$> genHashScriptData
                    , C.TxOutDatumInTx C.ScriptDataInAlonzoEra <$> genSimpleScriptData
                    ]
    C.BabbageEra -> Gen.choice
                    [ pure C.TxOutDatumNone
                    , C.TxOutDatumHash C.ScriptDataInBabbageEra <$> genHashScriptData
                    , C.TxOutDatumInTx C.ScriptDataInBabbageEra <$> genSimpleScriptData
                    , C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> genSimpleScriptData
                    ]

-- Copied from cardano-api. Delete when this function is reexported
genHashScriptData :: Gen (C.Hash C.ScriptData)
genHashScriptData = C.ScriptDataHash . unsafeMakeSafeHash . mkDummyHash <$> Gen.int (Range.linear 0 10)
  where
    mkDummyHash :: forall h a. CRYPTO.HashAlgorithm h => Int -> CRYPTO.Hash h a
    mkDummyHash = coerce . CRYPTO.hashWithSerialiser @h CBOR.toCBOR

genProtocolParametersForPlutusScripts :: Gen C.ProtocolParameters
genProtocolParametersForPlutusScripts =
  C.ProtocolParameters
    <$> ((,) <$> genNat <*> genNat)
    <*> Gen.maybe CGen.genRational
    <*> CGen.genMaybePraosNonce
    <*> genNat
    <*> genNat
    <*> genNat
    <*> genNat
    <*> genNat
    <*> Gen.maybe CGen.genLovelace
    <*> CGen.genLovelace
    <*> CGen.genLovelace
    <*> CGen.genLovelace
    <*> genEpochNo
    <*> genNat
    <*> genRationalInt64
    <*> CGen.genRational
    <*> CGen.genRational
    <*> pure Nothing -- Obsolete from babbage onwards
    <*> pure (Map.fromList
      [ (C.AnyPlutusScriptVersion C.PlutusScriptV1, C.CostModel $ fromMaybe (error "Ledger.Params: defaultCostModelParams is broken") defaultCostModelParams)
      , (C.AnyPlutusScriptVersion C.PlutusScriptV2, C.CostModel $ fromMaybe (error "Ledger.Params: defaultCostModelParams is broken") defaultCostModelParams) ])
    <*> (Just <$> genExecutionUnitPrices)
    <*> (Just <$> genExecutionUnits)
    <*> (Just <$> genExecutionUnits)
    <*> (Just <$> genNat)
    <*> (Just <$> genNat)
    <*> (Just <$> genNat)
    <*> (Just <$> CGen.genLovelace)
 where
    -- Copied from cardano-api. Delete when this function is reexported
    genRationalInt64 :: Gen Rational
    genRationalInt64 =
        (\d -> ratioToRational (1 % d)) <$> genDenominator
      where
        genDenominator :: Gen Int64
        genDenominator = Gen.integral (Range.linear 1 maxBound)

        ratioToRational :: Ratio Int64 -> Rational
        ratioToRational = toRational

    -- Copied from cardano-api. Delete when this function is reexported
    genEpochNo :: Gen C.EpochNo
    genEpochNo = C.EpochNo <$> Gen.word64 (Range.linear 0 10)

    -- Copied from cardano-api. Delete when this function is reexported
    genNat :: Gen Natural
    genNat = Gen.integral (Range.linear 0 10)

    -- Copied from cardano-api. Delete when this function is reexported
    genExecutionUnitPrices :: Gen C.ExecutionUnitPrices
    genExecutionUnitPrices = C.ExecutionUnitPrices <$> CGen.genRational <*> CGen.genRational

-- TODO Copied from cardano-api. Delete once reexported
genAssetId :: Gen C.AssetId
genAssetId = Gen.choice [ C.AssetId <$> genPolicyId <*> CGen.genAssetName
                        , return C.AdaAssetId
                        ]

-- TODO Copied from cardano-api. Delete once reexported
genPolicyId :: Gen C.PolicyId
genPolicyId =
  Gen.frequency
      -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, Gen.element [ fromString (x : replicate 55 '0') | x <- ['a'..'c'] ])

       -- and some from the full range of the type
    , (1, C.PolicyId <$> CGen.genScriptHash)
    ]

-- TODO Copied from cardano-api. Delete once reexported
genQuantity :: Range Integer -> Gen C.Quantity
genQuantity range = fromInteger <$> Gen.integral range
