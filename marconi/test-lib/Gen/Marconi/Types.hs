{-# LANGUAGE RecordWildCards #-}

module Gen.Marconi.Types
  ( genBlockHeader
  , genBlockNo
  , genChainPoint
  , genChainPoint'
  , genEvents
  , genEventAtChainPoint
  , genSlotNo
  , genTxIndex
  , genUtxo
  ) where

import Cardano.Api qualified as C
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Gen.Cardano.Api.Typed qualified as CGen
import Hedgehog (Gen, MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
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

-- genShelleyUtxo :: Gen Utxo.Utxo
-- genShelleyUtxo = do
--   _address          <- C.toAddressAny <$> CGen.genAddressShelley
--   _txId             <- CGen.genTxId
--   _txIx             <- genTxIndex
--   sc <- CGen.genTxOutDatumHashTxContext C.BabbageEra
--   let (_datum, _datumHash)  = Utxo.getScriptDataAndHash sc
--   script <- CGen.genReferenceScript C.ShelleyEra
--   _value            <- CGen.genValueForTxOut
--   let (_inlineScript, _inlineScriptHash)=  Utxo.getRefScriptAndHash script
--   pure $ Utxo.Utxo {..}

-- genShelleyEvents :: Gen (Utxo.StorableEvent Utxo.UtxoHandle)
-- genShelleyEvents = do
--   ueUtxos <- Gen.set (Range.linear 1 3) genShelleyUtxo
--   ueInputs <- Gen.set (Range.linear 1 2) CGen.genTxIn
--   ueBlockNo <- genBlockNo
--   ueChainPoint <- genChainPoint
--   pure $ Utxo.UtxoEvent {..}
