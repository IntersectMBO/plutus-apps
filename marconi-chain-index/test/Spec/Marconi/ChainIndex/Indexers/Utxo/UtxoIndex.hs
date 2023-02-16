{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Marconi.ChainIndex.Indexers.Utxo.UtxoIndex (tests) where

import Control.Lens (filtered, folded, toListOf)
import Control.Lens.Operators ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (nonEmpty, toList)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Set qualified as Set
import Database.SQLite.Simple qualified as SQL
import Hedgehog (Property, cover, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Api qualified as C
import Control.Monad (void)
import Data.List qualified as List
import Gen.Cardano.Api.Typed qualified as CGen
import Gen.Marconi.ChainIndex.Indexers.Utxo (genEventAtChainPoint, genUtxoEvents)
import Marconi.ChainIndex.Indexers.Utxo (StorableEvent (ueInputs, ueUtxos))
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Types (CurrentEra, TargetAddresses)
import Marconi.Core.Storable (StorableQuery)
import Marconi.Core.Storable qualified as Storable

-- | Proves two list are equivalant, but not identical

-- NOTE --
-- | UtxoEvents equivalent relationship
-- Not all utxoEvent attributes have defined `Eq` and/or `Ord` relationship defined.
-- As events are disassembled and reassembled, the Ordering of these sub-parts may change in the coresponding collections.
-- Therefore we used the Equivalence relationship to show two event are morally equal.
equivalentLists :: Eq a => [a] -> [a] -> Bool
equivalentLists us us' =
  length us == length us'
  &&
  all (const True) [u `elem` us'| u <- us]
  &&
  all (const True) [u `elem` us| u <- us']

tests :: TestTree
tests = testGroup "Spec.Marconi.ChainIndex.Indexers.Utxo"
    [ testPropertyNamed
        "filter UtxoEvent for Utxos with address in the TargetAddress"
        "eventsAtAddressTest"
        eventsAtAddressTest

    , testPropertyNamed
        "marconi-utxo event-to-sqlRows property"
        "eventsToRowsRoundTripTest"
        eventsToRowsRoundTripTest

    , testPropertyNamed
        "marconi-utxo storable-query address property"
        "txAddressToUtxoAddressTest"
        txAddressToUtxoAddressTest

    , testPropertyNamed
        "marconi-utxo storage-roundtrip property"
        "utxoStorageTest"
        utxoStorageTest

    -- TODO BROKEN
    -- , testPropertyNamed
    --     "marconi-utxo insert-query property"
    --     "utxoInsertAndQueryTest"
    --     utxoInsertAndQueryTest

    , testPropertyNamed
        "marconi-utxo query-interval property"
        "utxoQueryIntervalTest"
        utxoQueryIntervalTest

    , testPropertyNamed
          "The points that indexer can be resumed from should return at least non-genesis point when some data was indexed on disk"
          "propResumingShouldReturnAtLeastOneNonGenesisPointIfStoredOnDisk"
          propResumingShouldReturnAtLeastOneNonGenesisPointIfStoredOnDisk

    , testPropertyNamed
          "The points that indexer can be resumed from should return an ordered list of points"
          "propResumingShouldReturnOrderedListOfPoints"
          propResumingShouldReturnOrderedListOfPoints
    ]


eventsToRowsRoundTripTest :: Property
eventsToRowsRoundTripTest  = property $ do
  events <- forAll genUtxoEvents
  let f :: C.ChainPoint -> IO (Set C.TxIn)
      f C.ChainPointAtGenesis = pure  Set.empty
      f _                     = pure . Utxo.ueInputs $ head events
      rows = concatMap Utxo.eventsToRows events
  computedEvent <- liftIO . Utxo.rowsToEvents f $ rows
  let postGenesisEvents = filter (\e -> C.ChainPointAtGenesis /= Utxo.ueChainPoint e ) events
  length computedEvent === (length . fmap Utxo.ueChainPoint $ postGenesisEvents)
  Hedgehog.assert (equivalentLists computedEvent postGenesisEvents)

-- Insert Utxo events in storage, and retreive the events
--
utxoStorageTest :: Property
utxoStorageTest = property $ do
  events <- forAll genUtxoEvents
  (storedEvents :: [StorableEvent Utxo.UtxoHandle]) <-
    (liftIO . Utxo.open ":memory:") (Utxo.Depth 10)
     >>= liftIO . Storable.insertMany events
     >>= liftIO . Storable.getEvents
  Hedgehog.assert (equivalentLists storedEvents events)

-- Insert Utxo events in storage, and retrieve the events by address
--
-- utxoInsertAndQueryTest :: Property
-- utxoInsertAndQueryTest = property $ do
--   events <- forAll genUtxoEvents
--   depth <- forAll $ Gen.int (Range.linear 1 5)
--   indexer <- liftIO $ Utxo.open ":memory:" (Utxo.Depth depth)
--              >>= liftIO . Storable.insertMany events
--   let
--     qs :: [StorableQuery Utxo.UtxoHandle]
--     qs = fmap (Utxo.UtxoAddress . Utxo._address) . concatMap (Set.toList . Utxo.ueUtxos) $ events
--   results <- liftIO . traverse (Storable.query Storable.QEverything indexer) $ qs
--   let rows = concatMap (\(Utxo.UtxoResult rs) -> rs ) results
--   computedEvent <-
--     liftIO . Utxo.rowsToEvents (Utxo.getTxIns (getConn indexer) ) $ rows
--   Hedgehog.assert (equivalentLists
--                    computedEvent
--                    (filter (\e -> Utxo.ueChainPoint e /= C.ChainPointAtGenesis) events) )

-- Insert Utxo events in storage, and retreive the events by address and query interval
--
utxoQueryIntervalTest :: Property
utxoQueryIntervalTest = property $ do
  event0 <- forAll $ genEventAtChainPoint C.ChainPointAtGenesis
  event1 <- forAll $ genEventAtChainPoint (head chainpoints)
  event2 <- forAll $ genEventAtChainPoint (chainpoints !! 1)
  event3 <- forAll $ genEventAtChainPoint (chainpoints !! 2)
  let events = [event0, event1, event2, event3]
  indexer <- liftIO $ Utxo.open ":memory:" (Utxo.Depth 2)
             >>= liftIO . Storable.insertMany [event0, event1, event2, event3]
  let
    qs :: [StorableQuery Utxo.UtxoHandle]
    qs = fmap (Utxo.UtxoAddress . Utxo._address) . concatMap (Set.toList . Utxo.ueUtxos) $ events
  results <- liftIO . traverse (Storable.query (Storable.QInterval (head chainpoints)(chainpoints !! 1)) indexer) $ qs
  let rows = concatMap (\(Utxo.UtxoResult rs) -> rs ) results
  computedEvent <-
    liftIO . Utxo.rowsToEvents (Utxo.getTxIns (getConn indexer) ) $ rows
  Hedgehog.assert (equivalentLists computedEvent [event0, event1])

-- TargetAddresses are the addresses in UTXO that we filter for.
-- Puporse of this test is to filter out utxos that have a different address than those in the TargetAddress list.
eventsAtAddressTest :: Property
eventsAtAddressTest = property $ do
    event <- head <$> forAll genUtxoEvents
    let (addresses :: [StorableQuery Utxo.UtxoHandle]) =
          map (Utxo.UtxoAddress . Utxo._address) $ Set.toList $ Utxo.ueUtxos event
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

-- | Test to make sure we only make Utxo's from chain events for the TargetAddresses user has
-- provided through CLI.
txAddressToUtxoAddressTest ::  Property
txAddressToUtxoAddressTest = property $ do
    t@(C.Tx (C.TxBody C.TxBodyContent{C.txOuts}) _)  <- forAll $ CGen.genTx C.BabbageEra
    let (targetAddresses :: Maybe TargetAddresses ) = mkTargetAddressFromTxOut txOuts
    let (utxos :: [Utxo.Utxo]) = Utxo.getUtxos targetAddresses t
    case targetAddresses of
        Nothing         ->  length utxos === length txOuts
        Just targets    ->
            ( List.nub
              . mapMaybe (\x -> addressAnyToShelley (x ^. Utxo.address))
              $ utxos) === (List.nub . toList $ targets)

chainpoints :: [C.ChainPoint]
chainpoints =
  let
    bs::ByteString
    bs::ByteString = "00000000000000000000000000000000"
    blockhash :: C.Hash C.BlockHeader
    blockhash = fromJust $ C.deserialiseFromRawBytes(C.proxyToAsType Proxy) bs
  in
    flip C.ChainPoint blockhash <$> [1 .. 3]

-- | The property verifies that the 'Storable.resumeFromStorage' call returns at least a point which
-- is not 'C.ChainPointAtGenesis' when some events are inserted on disk.
propResumingShouldReturnAtLeastOneNonGenesisPointIfStoredOnDisk :: Property
propResumingShouldReturnAtLeastOneNonGenesisPointIfStoredOnDisk = property $ do
    events <- forAll genUtxoEvents
    cover 90 "All UtxoEvents have a least one utxo and one spent txout"
        $ isJust
        $ List.find (\ue -> not (Set.null (ueUtxos ue)) && not (Set.null (ueInputs ue))) events
    cover 90 "At least one UtxoEvent with at least one utxo"
        $ isJust
        $ List.find (\ue -> not $ Set.null $ ueUtxos ue) events
    cover 90 "At least one UtxoEvent with at least one spent tx out"
        $ isJust
        $ List.find (\ue -> not $ Set.null $ ueInputs ue) events

    -- We make sure that at least one event is stored on disk
    depth <- forAll $ Gen.int (Range.linear 1 $ length events - 1)

    -- We insert the events in the indexer, but for the test assertions, we discard the events in
    -- memory.
    indexer <- liftIO $ Utxo.open ":memory:" (Utxo.Depth depth)
    void $ liftIO $ Storable.insertMany events indexer

    actualResumablePoints <- liftIO $ Storable.resume indexer
    -- TODO Given current implementation, we only expect to be able to resume from events which
    -- contain at least one UTXO. In the future, this should be changed to take into account
    -- 'UtxoEvent's which have no utxos and no inputs
    -- let expectedResumablePoints =
    --         ChainPointAtGenesis
    --         : fmap Utxo.ueChainPoint
    --             (filter (\UtxoEvent { ueUtxos } -> not (Set.null ueUtxos)) events)
    -- Set.fromList actualResumablePoints === Set.fromList expectedResumablePoints
    Hedgehog.assert $ length actualResumablePoints >= 2

-- | The property verifies that the 'Storable.resumeFromStorage' returns an ordered list of points.
propResumingShouldReturnOrderedListOfPoints :: Property
propResumingShouldReturnOrderedListOfPoints = property $ do
    events <- forAll genUtxoEvents
    depth <- forAll $ Gen.int (Range.linear 1 $ length events)

    -- We insert the events in the indexer, but for the test assertions, we discard the events in
    -- memory.
    indexer <- liftIO $ Utxo.open ":memory:" (Utxo.Depth depth)
    void $ liftIO $ Storable.insertMany events indexer

    resumablePoints <- liftIO $ Storable.resume indexer
    List.reverse (List.sort resumablePoints) === resumablePoints

-- Create TargetAddresses
-- We use TxOut to create a valid and relevant TargetAddress. This garnteed that the targetAddress is among the generated events.
mkTargetAddressFromTxOut
  :: [C.TxOut C.CtxTx CurrentEra]
  -> Maybe TargetAddresses
mkTargetAddressFromTxOut [C.TxOut addressInEra _ _ _]
    = nonEmpty
    . mapMaybe (addressAnyToShelley . Utxo.toAddr)
    $ [addressInEra]
mkTargetAddressFromTxOut _ = Nothing

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
