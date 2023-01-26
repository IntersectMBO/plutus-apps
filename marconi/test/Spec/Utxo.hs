{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Utxo (tests) where

import Control.Lens (filtered, folded, toListOf, traversed)
import Control.Lens.Operators ((%~), (^.))
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.List.NonEmpty (nonEmpty, toList)
import Data.Maybe (fromJust, mapMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Set qualified as Set
import Database.SQLite.Simple qualified as SQL
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Api qualified as C
import Gen.Cardano.Api.Typed qualified as CGen
import Marconi.Index.Utxo qualified as Utxo
import Marconi.Types (CurrentEra, TargetAddresses)
import RewindableIndex.Storable (StorableEvent, StorableQuery)
import RewindableIndex.Storable qualified as Storable

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

genUtxo :: Gen Utxo.Utxo
genUtxo = C.toAddressAny <$> CGen.genAddressShelley >>= genUtxo'

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

tests :: TestTree
tests = testGroup "Marconi.Utxo.Indexer.Specs are:"
    [
     testPropertyNamed "marconi-utxo split-by-address property"
     "filter UtxoEvent for Utxos with address in the TargetAddress"
     eventsAtAddressTest

    , testPropertyNamed "marconi-utxo event-to-sqlRows property"
      "Roundtrip UtxoEvents to UtxoRows converion"
     eventsToRowsRoundTrip

    , testPropertyNamed "marconi-utxo storable-query address property"
      "Compute StorableQuery addresses from computed Utxo and generated Cardano.Api.Tx"
     txAddressToUtxoAddressTest

    , testPropertyNamed "marconi-utxo storage-roundtrip property"
      "Roundtrip storage test"
      utxoStorageTest

    , testPropertyNamed "marconi-utxo insert-query property"
      "Insert Events, and then query for events by address test"
      utxoInsertAndQueryTest
    ]

deriving instance Eq (StorableEvent Utxo.UtxoHandle)
deriving instance Ord (StorableEvent Utxo.UtxoHandle)

-- | Proves two list are equivalant, but not identical

-- NOTE --
-- | UtxoEvents equivalent relationship
-- Not all utxoEvent attributes have defined `Eq` and/or `Ord` relationship defined.
-- As events are disassembled and reassembled, the Ordering of these sub-parts may change in the coresponding collections.
-- Therefore we used the Equivalence relationship to show two event are morally equal.
--
equivalentLists :: Eq a => [a] -> [a] -> Bool
equivalentLists us us' =
  length us == length us'
  &&
  all (const True) [u `elem` us'| u <- us]
  &&
  all (const True) [u `elem` us| u <- us']

-- convert events to sql rows and back to events.
--
eventsToRowsRoundTrip :: Property
eventsToRowsRoundTrip  = property $ do
  events <- forAll $ Gen.list (Range.linear 1 5 )genEvents
  let f :: C.ChainPoint -> IO (Set C.TxIn)
      f _ = pure . Utxo.ueInputs $ head events
      rows = concatMap Utxo.toUtxoRows events
  computedEvent <- liftIO . Utxo.rowsToEvents f $ rows
  length computedEvent === (length . fmap Utxo.ueChainPoint $ events)
  Hedgehog.assert (equivalentLists computedEvent events)

-- Insert Utxo events in storage, and retreive the events
--
utxoStorageTest :: Property
utxoStorageTest = property $ do
  events <- forAll $ Gen.list (Range.linear 1 5) genEvents
  (storedEvents :: [StorableEvent Utxo.UtxoHandle]) <-
    (liftIO . Utxo.open ":memory:") (Utxo.Depth  10)
     >>= liftIO . Storable.insertMany events
     >>= liftIO . Storable.getEvents
  Hedgehog.assert (equivalentLists storedEvents events)

-- Insert Utxo events in storage, and retreive the events by address
--
utxoInsertAndQueryTest :: Property
utxoInsertAndQueryTest = property $ do
  events <- forAll $ Gen.list (Range.linear 1 5) genEvents
  depth <- forAll $ Gen.int (Range.linear 1 5)
  indexer <- liftIO $ Utxo.open ":memory:" (Utxo.Depth depth)
             >>= liftIO . Storable.insertMany events
  let
    qs :: [StorableQuery Utxo.UtxoHandle]
    qs = fmap (Utxo.UtxoAddress . Utxo._address) . concatMap (Set.toList . Utxo.ueUtxos) $ events
  results <- liftIO . traverse (Storable.query Storable.QEverything indexer) $ qs
  let rows = concatMap (\(Utxo.UtxoResult rs) -> rs ) results
  computedEvent <-
    liftIO . Utxo.rowsToEvents (Utxo.getTxIns (getConn indexer) ) $ rows
  Hedgehog.assert (equivalentLists computedEvent events)

-- This test is in supporto TargetAddresses user's may have entered through CLI
-- Remove from UtxoEvent Utxos with address not in TargetAddress list
-- TODO cleaner approach
-- Generate Addresse -> generate Utxos -> generateEvents and use a subset of those addresses to filter in/out
eventsAtAddressTest :: Property
eventsAtAddressTest = property $ do
    event <- forAll genEvents
    let (addresses :: [StorableQuery Utxo.UtxoHandle]) =
          (traversed %~ Utxo.UtxoAddress)
          . nub
          . toListOf (folded . Utxo.address)
          . Utxo.ueUtxos
          $ event
        sameAddressEvents :: [StorableEvent Utxo.UtxoHandle]
        sameAddressEvents =  Utxo.eventsAtAddress (head addresses) [event]
        (Utxo.UtxoAddress targetAddress ) =  head addresses
        (computedAddresses :: [C.AddressAny])
          = toListOf (folded . Utxo.address)
          . concatMap (Set.toList . Utxo.ueUtxos)
          $ sameAddressEvents
        (actualAddresses :: [C.AddressAny])
          = toListOf (folded
                      . Utxo.address
                      . filtered (== targetAddress) )
            $ Utxo.ueUtxos event
    computedAddresses === actualAddresses

-- Test to make sure we only make Utxo's from chain events for the TargetAddresses user has provided thrugh CLI
--
txAddressToUtxoAddressTest ::  Property
txAddressToUtxoAddressTest = property $ do
    t@(C.Tx (C.TxBody C.TxBodyContent{C.txOuts}) _)  <- forAll $ CGen.genTx C.BabbageEra
    let (targetAddresses :: Maybe TargetAddresses ) = addressesFromTxOuts txOuts
    let (utxos :: [Utxo.Utxo]) = Utxo.getUtxos targetAddresses t
    case targetAddresses of
        Nothing         ->  length utxos === length txOuts
        Just targets    ->
            ( nub
              . mapMaybe (\x -> addressAnyToShelley (x ^. Utxo.address))
              $ utxos) === (nub . toList $ targets)

-- create TargetAddresses
addressesFromTxOuts
  :: [C.TxOut C.CtxTx CurrentEra]
  -> Maybe TargetAddresses
addressesFromTxOuts [C.TxOut addressInEra _ _ _]
    = nonEmpty
    . nub
    . mapMaybe (addressAnyToShelley . Utxo.toAddr)
    $ [addressInEra]
addressesFromTxOuts _ = Nothing

addressAnyToShelley
  :: C.AddressAny
  -> Maybe (C.Address C.ShelleyAddr)
addressAnyToShelley  (C.AddressShelley a) = Just a
addressAnyToShelley  _                    = Nothing

getConn :: Storable.State Utxo.UtxoHandle -> SQL.Connection
getConn  s =
  let
    (Utxo.UtxoHandle c _)  = s ^. Storable.handle
  in c
