{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Marconi.Mamba.Api.UtxoIndexersQuery (tests) where

import Control.Concurrent.STM (atomically)
import Control.Lens.Operators ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromJust, mapMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set

import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Api qualified as C
import Gen.Cardano.Api.Typed qualified as CGen
import Marconi.Mamba.Api.Types (HasUtxoIndexerEnv (uiIndexer))

import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Types (TargetAddresses)
import Marconi.Core.Storable (StorableEvent)
import Marconi.Core.Storable qualified as Storable
import Marconi.Mamba.Api.UtxoIndexersQuery qualified as UIQ

genSlotNo :: Hedgehog.MonadGen m => m C.SlotNo
genSlotNo = C.SlotNo <$> Gen.word64 (Range.linear 10 1000)

genBlockNo :: Hedgehog.MonadGen m => m C.BlockNo
genBlockNo = C.BlockNo <$> Gen.word64 (Range.linear 100 1000)

genBlockHeader
  :: Hedgehog.MonadGen m
  => m C.BlockNo
  -> m C.SlotNo
  -> m C.BlockHeader
genBlockHeader genB genS = do
  let validByteSizeLength = 32
  bs <- Gen.bytes(Range.singleton validByteSizeLength)
  sn <- genS
  bn <- genB
  let (hsh :: C.Hash C.BlockHeader) =  fromJust $ C.deserialiseFromRawBytes(C.proxyToAsType Proxy) bs
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
genChainPoint = genChainPoint' genBlockNo genSlotNo

genTxIndex :: Gen C.TxIx
genTxIndex = C.TxIx . fromIntegral <$> Gen.word16 Range.constantBounded

genShelleyUtxo :: Gen Utxo.Utxo
genShelleyUtxo = do
  _address          <- fmap  C.toAddressAny  CGen.genAddressShelley
  _txId             <- CGen.genTxId
  _txIx             <- genTxIndex
  sc <- CGen.genTxOutDatumHashTxContext C.BabbageEra
  let (_datum, _datumHash)  = Utxo.getScriptDataAndHash sc
  script <- CGen.genReferenceScript C.ShelleyEra
  _value            <- CGen.genValueForTxOut
  let (_inlineScript, _inlineScriptHash)=  Utxo.getRefScriptAndHash script
  pure $ Utxo.Utxo {..}

genShelleyEvents :: Gen (Utxo.StorableEvent Utxo.UtxoHandle)
genShelleyEvents = do
  ueUtxos <- Gen.set (Range.linear 1 3) genShelleyUtxo
  ueInputs <- Gen.set (Range.linear 1 2) CGen.genTxIn
  ueBlockNo <- genBlockNo
  ueChainPoint <- genChainPoint
  pure $ Utxo.UtxoEvent {..}

deriving instance Eq (StorableEvent Utxo.UtxoHandle)
deriving instance Ord (StorableEvent Utxo.UtxoHandle)

-- | Proves two list are equivalant, but not identical
--
equivalentLists :: Eq a => [a] -> [a] -> Bool
equivalentLists us us' =
  length us == length us'
  &&
  all (const True) [u `elem` us'| u <- us]
  &&
  all (const True) [u `elem` us| u <- us']

-- | Insert events, and do the callback
mocUtxoWorker
  :: (UIQ.UtxoIndexer -> IO ())
  -> [Utxo.StorableEvent Utxo.UtxoHandle]
  -> Utxo.Depth
  -> IO ()
mocUtxoWorker callback events depth =
  Utxo.open ":memory:" depth >>= Storable.insertMany events >>= callback

tests :: TestTree
tests = testGroup "marconi-mamba query Api Specs"
    [testPropertyNamed
     "marconi-mamba query-target-addresses"
     "Spec. Insert events and query for utxo's with address in the generated ShelleyEra targetAddresses"
     queryTargetAddressTest
    ]

-- | generate some Utxo events, store them and fetch them.
--
queryTargetAddressTest :: Property
queryTargetAddressTest = property $ do
  events <- forAll $ Gen.list (Range.linear 1 3) genShelleyEvents
  depth <- forAll $ Gen.int (Range.linear 6 9) -- force DB writes
  let
    targetAddresses :: TargetAddresses
    targetAddresses
      = fromList
      . mapMaybe addressAnyToShelley
      . nub
      . fmap Utxo._address
      . concatMap (Set.toList . Utxo.ueUtxos)
      $ events
  env <- liftIO . UIQ.bootstrap $ targetAddresses
  let
    callback :: Utxo.UtxoIndexer -> IO ()
    callback = atomically . UIQ.writeTMVar' (env ^. uiIndexer)
  liftIO . mocUtxoWorker callback events $ Utxo.Depth depth
  fetchedRows <-
    liftIO
    . fmap (nub . concat)
    . traverse (UIQ.findByCardanoAddress env)
    . fmap C.toAddressAny
    $ targetAddresses
  let rows = nub . concatMap Utxo.eventsToRows $ events
  length fetchedRows === length rows
  Hedgehog.diff fetchedRows equivalentLists rows

addressAnyToShelley :: C.AddressAny -> Maybe (C.Address C.ShelleyAddr)
addressAnyToShelley  (C.AddressShelley a ) = Just a
addressAnyToShelley  _                     = Nothing
