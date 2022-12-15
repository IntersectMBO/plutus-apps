{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Utxo (tests) where

import Control.Lens (folded, toListOf)
import Control.Lens.Operators ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.List.NonEmpty (nonEmpty, toList)
import Data.Maybe (fromJust, isJust, mapMaybe)
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

genChainPoint
  :: Hedgehog.MonadGen m
  => m C.BlockNo
  -> m C.SlotNo
  -> m C.ChainPoint
genChainPoint genB genS = do
  (C.BlockHeader sn hsh _) <- genBlockHeader genB genS
  pure $ C.ChainPoint sn hsh

genChainPoint' :: Hedgehog.MonadGen m => m C.ChainPoint
genChainPoint' = genChainPoint genBlockNo genSlotNo

genEvents :: Gen (Utxo.StorableEvent Utxo.UtxoHandle)
genEvents = do
    txs <- Gen.list (Range.linear 1 5)(CGen.genTx C.ShelleyEra)
    blkNo  <- genBlockNo
    cp <- genChainPoint'
    pure . fromJust $ Utxo.getUtxoEvents Nothing txs blkNo cp


headOption :: [a] -> Maybe a
headOption []    = Nothing
headOption (x:_) = Just x

tests :: TestTree
tests = testGroup "Marconi.Utxo.Indexer.Specs are:"
    [
     testPropertyNamed "marconi-utxo split-by-address property"
     "Split UtxoEvents by address"
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
--
equivalentLists :: Eq a => [a] -> [a] -> Bool
equivalentLists us us' =
  length us == length us'
  &&
  all (const True) [u `elem` us'| u <- us]
  &&
  all (const True) [u `elem` us| u <- us']

-- NOTE --
-- | UtxoEvents equivalent relationship
-- Not all utxoEvent attributes have defined `Eq` and/or `Ord` relationship defined.
-- As events are disassembled and reassembled, the Ordering of these sub-parts may change in the coresponding collections.
-- Therefore we used the Equivalence relationship to show two event are morally equal.

eventsToRowsRoundTrip :: Property
eventsToRowsRoundTrip  = property $ do
  events <- forAll $ Gen.list (Range.linear 1 5 )genEvents
  let f :: C.ChainPoint -> IO (Set C.TxIn)
      f _ = pure . Utxo.ueInputs $ head events
      rows = concatMap Utxo.toUtxoRows events
  computedEvent <- liftIO . Utxo.rowsToEvents f $ rows
  length computedEvent === (length . fmap Utxo.ueChainPoint $ events)
  Hedgehog.assert (equivalentLists computedEvent events)

utxoInsertAndQueryTest :: Property
utxoInsertAndQueryTest = property $ do
  events <- forAll $ Gen.list (Range.linear 1 5) genEvents
  depth <- forAll $ Gen.int (Range.linear 1 5) -- force DB writes
  indexer <- liftIO $ Utxo.open ":memory:" (Utxo.Depth depth)
             >>= liftIO . Storable.insertMany events
  let
    qs :: [StorableQuery Utxo.UtxoHandle]
    qs = fmap Utxo._address . concatMap (Set.toList . Utxo.ueUtxos) $ events
  results <- liftIO . traverse (Storable.query Storable.QEverything indexer) $ qs
  let rows = concatMap (\(Utxo.UtxoResult rs) -> rs ) results
  computedEvent <-
    liftIO . Utxo.rowsToEvents (Utxo.getTxIns (getConn indexer) ) $ rows
  Hedgehog.assert (equivalentLists computedEvent events)

getConn :: Storable.State Utxo.UtxoHandle -> SQL.Connection
getConn  s =
  let
    (Utxo.UtxoHandle c _)  = s ^. Storable.handle
  in c

eventsAtAddressTest :: Property
eventsAtAddressTest = property $ do
    event <- forAll genEvents
    let (addresses :: [StorableQuery Utxo.UtxoHandle]) =
            nub
            . toListOf (folded . Utxo.address)
            . Utxo.ueUtxos
            $ event
        sameAddressEvents :: [StorableEvent Utxo.UtxoHandle]
        sameAddressEvents =  Utxo.eventsAtAddress (head addresses) [event]
    Hedgehog.assert . not . null $ sameAddressEvents
    let eventsAddress :: Maybe (StorableQuery Utxo.UtxoHandle)
        eventsAddress =
          headOption
          . nub
          . toListOf ( folded . Utxo.address )
          . concatMap (Set.toList . Utxo.ueUtxos)
          $ sameAddressEvents
    Hedgehog.assert . isJust $ eventsAddress
    fromJust eventsAddress ===  head addresses

txAddressToUtxoAddressTest ::  Property
txAddressToUtxoAddressTest = property $ do
    t@(C.Tx (C.TxBody C.TxBodyContent{C.txOuts}) _)  <- forAll $ CGen.genTx C.BabbageEra
    let (targetAddresses :: Maybe TargetAddresses ) = addressesFromTxOuts txOuts
    let (utxos :: [Utxo.Utxo]) = Utxo.getUtxos targetAddresses t
    case targetAddresses of
        Nothing         ->  length utxos === length txOuts
        Just targets    ->
            ( nub
              . mapMaybe (\x -> storableQueryToShelleyAddr (x ^. Utxo.address))
              $ utxos) === (nub . toList $ targets)

utxoStorageTest :: Property
utxoStorageTest = property $ do
  events <- forAll $ Gen.list (Range.linear 1 5) genEvents
  (storedEvents :: [StorableEvent Utxo.UtxoHandle]) <-
    (liftIO . Utxo.open ":memory:") (Utxo.Depth  10) -- no DB writes
     >>= liftIO . Storable.insertMany events
     >>= liftIO . Storable.getEvents
  Hedgehog.assert (equivalentLists storedEvents events)

addressesFromTxOuts :: [C.TxOut C.CtxTx CurrentEra] -> Maybe TargetAddresses
addressesFromTxOuts [C.TxOut addressInEra _ _ _]
    = nonEmpty
    . nub
    . mapMaybe (addressAnyToShelley . Utxo.toAddr)
    $ [addressInEra]
addressesFromTxOuts _ = Nothing

storableQueryToShelleyAddr :: StorableQuery Utxo.UtxoHandle -> Maybe (C.Address C.ShelleyAddr)
storableQueryToShelleyAddr (Utxo.UtxoAddress (C.AddressShelley a ) ) = Just a
storableQueryToShelleyAddr _                                         = Nothing

addressAnyToShelley :: C.AddressAny -> Maybe (C.Address C.ShelleyAddr)
addressAnyToShelley  (C.AddressShelley a) = Just a
addressAnyToShelley  _                    = Nothing
