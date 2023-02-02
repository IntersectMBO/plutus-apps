{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Gen.Marconi.Types
  ( genBlockHeader
  , genBlockNo
  , genChainPoint
  , genChainPoint'
  , genEvents
  , genEventAtChainPoint
  , genExecutionUnits
  , genSlotNo
  , genTxBodyContentWithTxInsCollateral
  , genTxBodyWithTxIns
  , genTxIndex
  , genUtxo
  , genWitnessAndHashInEra
  ) where

import Cardano.Api qualified as C
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)

import Hedgehog (Gen, MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Cardano.Api.Shelley qualified as Shelley
import Gen.Cardano.Api.Typed qualified as CGen
import Marconi.Index.Utxo qualified as Utxo


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

genUtxo :: Gen Utxo.Utxo
genUtxo = CGen.genAddressShelley >>= genUtxo' . C.toAddressAny

genUtxo' :: C.AddressAny -> Gen Utxo.Utxo
genUtxo' _address = do
  _txId             <- CGen.genTxId
  _txIx             <- genTxIndex
  sc <- CGen.genTxOutDatumHashTxContext C.BabbageEra
  let (_datum, _datumHash)  = Utxo.getScriptDataAndHash sc
  script            <- CGen.genReferenceScript C.ShelleyEra
  _value            <- CGen.genValueForTxOut
  let (_inlineScript, _inlineScriptHash)=  Utxo.getRefScriptAndHash script
  pure $ Utxo.Utxo {..}

genEventAtChainPoint :: C.ChainPoint -> Gen (Utxo.StorableEvent Utxo.UtxoHandle)
genEventAtChainPoint ueChainPoint = do
  ueUtxos <- Gen.set (Range.linear 1 3) genUtxo
  ueInputs <- Gen.set (Range.linear 1 2) CGen.genTxIn
  ueBlockNo <- genBlockNo
  pure $ Utxo.UtxoEvent {..}

genEvents :: Gen (Utxo.StorableEvent Utxo.UtxoHandle)
genEvents = do
  ueUtxos <- Gen.set (Range.linear 1 3) genUtxo
  genEvents' ueUtxos

genEvents'
  :: Set Utxo.Utxo
  -> Gen (Utxo.StorableEvent Utxo.UtxoHandle)
genEvents' ueUtxos = do
  ueInputs <- Gen.set (Range.linear 1 2) CGen.genTxIn
  ueBlockNo <- genBlockNo
  ueChainPoint <- genChainPoint
  pure $ Utxo.UtxoEvent {..}

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
        (Shelley.PScript plutusScript)
        (C.ScriptDatumForTxIn scriptData)
        scriptData
        executionUnits
    C.SimpleScript version simpleScript ->
      pure $ C.SimpleScriptWitness scriptLanguageInEra version (Shelley.SScript simpleScript)
  pure (witness, C.hashScript script)

-- | TODO Copy-paste from cardano-node: cardano-api/gen/Gen/Cardano/Api/Typed.hs
genExecutionUnits :: Gen C.ExecutionUnits
genExecutionUnits = C.ExecutionUnits <$> Gen.integral (Range.constant 0 1000)
                                     <*> Gen.integral (Range.constant 0 1000)
