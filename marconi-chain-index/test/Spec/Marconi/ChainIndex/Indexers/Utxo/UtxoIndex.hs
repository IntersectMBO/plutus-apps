{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.ChainIndex.Indexers.Utxo.UtxoIndex (tests) where

import Cardano.Api qualified as C
import Control.Lens (each, filtered, folded, toListOf)
import Control.Lens.Operators ((%~), (&), (^.))
import Control.Monad (forM, forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.List qualified as List
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Gen.Marconi.ChainIndex.Indexers.Utxo (genEventWithShelleyAddressAtChainPoint, genShelleyEraUtxoEvents,
                                             genUtxoEvents)
import Gen.Marconi.ChainIndex.Indexers.Utxo qualified as UtxoGen
import Gen.Marconi.ChainIndex.Mockchain (mockBlockTxs)
import Gen.Marconi.ChainIndex.Types (genChainPoints)
import Hedgehog (Property, cover, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helpers (addressAnyToShelley)
import Marconi.ChainIndex.Indexers.Utxo (StorableEvent (ueInputs, ueUtxos))
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Types (TargetAddresses)
import Marconi.Core.Storable (StorableQuery)
import Marconi.Core.Storable qualified as Storable
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests = testGroup "Spec.Marconi.ChainIndex.Indexers.Utxo"

    [ testPropertyNamed
        "marconi-utxo regression, save and retrieve by address must return Utxos. No Utxos should have an Spent"
        "allqueryUtxosShouldBeUnspent"
        allqueryUtxosShouldBeUnspent

    , testPropertyNamed
        "filter UtxoEvent for Utxos with address in the TargetAddress"
        "eventsAtAddressTest"
        eventsAtAddressTest

    , testPropertyNamed
        "marconi-utxo event-to-sqlRows property"
        "eventsToRowsRoundTripTest"
        eventsToRowsRoundTripTest

    , testPropertyNamed
        "getUtxoEvents with target addresses corresponding to all addresses in generated txs should return the same 'UtxoEvent' as if no target addresses were provided"
        "propUsingAllAddressesOfTxsAsTargetAddressesShouldReturnUtxosAsIfNoFilterWasApplied "
        propUsingAllAddressesOfTxsAsTargetAddressesShouldReturnUtxosAsIfNoFilterWasApplied

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

    , testPropertyNamed
          "ToJSON/FromJSON roundtrip for UtxoRow"
          "propJsonRoundtripUtxoRow"
          propJsonRoundtripUtxoRow
    ]

---------------- Regression test -------------------
-- | The purpose of thest is to make sure All Queried Utxo's are unSpent.
--  The Utxo store consists of:
--  * in-memory store:  UtxoEvents before they're flushed to SQlite
--  * SQL-database store:  UtxoRows that are stored in SQLite
--  In this regression test, We want to make sure:
--    (1) all utxo query results from SQL-database store are unspent
--    (2) all utxos query results from in-memory store are unspent
--    (3) the edge case where although we satisfy (1) and (2), one or many of the query results from SQLite store may have `Spent` in in-memory store.
--    (4) furthermore, we want to prove that there is always at least one utxoRow returned from sotre.
--  Point (4) is a consequence of the `genShelleyEraUtxoEvents` specifications:  __there is only Spent for previous generated UtxoEvent__
--
-- Assumption:  SQLite vacuum is disabled. This is insupport of (4) and may occure for `depth` of such small numbers.
-- Note:        We expect this test to fail in this branch.
allqueryUtxosShouldBeUnspent :: Property
allqueryUtxosShouldBeUnspent = property $ do
  events <- forAll genShelleyEraUtxoEvents
  let numOfEvents = length events
  -- We choose the `depth` so that we can test the bounderies to test point (3) above.
  -- this will ensure we have adequate coverage where events are in both, in-memory store and SQLite store
  -- we use constantFrom to make sure we have coverage for more than 1 event in the in-memory-store
  depth <- forAll $ Gen.int (Range.constantFrom (numOfEvents - 1) 1 (numOfEvents + 1))
  Hedgehog.classify "Query both in-memory and storage " $ depth < numOfEvents
  Hedgehog.classify "Query in-memory only" $ depth > numOfEvents

  indexer <- liftIO $ Utxo.open ":memory:" (Utxo.Depth depth)
             >>= liftIO . Storable.insertMany events
  let
    addressQueries :: [StorableQuery Utxo.UtxoHandle] -- we want to query for all addresses
    addressQueries
      = List.nub
      . fmap (Utxo.UtxoAddress . Utxo._address)
      . concatMap (Set.toList . Utxo.ueUtxos)
      $ events
  results <- liftIO . traverse (Storable.query Storable.QEverything indexer) $ addressQueries
  let retrievedUtxoRows :: [Utxo.UtxoRow] = concatMap (\(Utxo.UtxoResult rs) -> rs ) results
      txinsFromRetrievedUtsoRows :: [C.TxIn]  -- get all the TxIn from quried UtxoRows
        = retrievedUtxoRows & each %~ (\r ->
                                       C.TxIn (r ^. Utxo.urUtxo . Utxo.txId)(r ^. Utxo.urUtxo . Utxo.txIx))
      txInsFromGeneratedEvents :: [C.TxIn]    -- get all the TxIn from quried UtxoRows
        = concatMap (\(Utxo.UtxoEvent _ ins _ ) -> Set.toList ins ) events

  -- A property of the generator is that there is at least one unspent transaction
  -- this property also ensures that the next test will not succeed for the trivila case
  -- when retrievedUtxoRows is an empty list
  Hedgehog.assert (not . null $ retrievedUtxoRows)

-- There should be no `Spent` in the retrieved UtxoRows
  Hedgehog.footnote "Regression test must return at least one Utxo. Utxo's may not have any Spent in the Orig. event"
  Hedgehog.assert
    $ all (== True)
    [u `notElem` txInsFromGeneratedEvents| u <- txinsFromRetrievedUtsoRows]

eventsToRowsRoundTripTest :: Property
eventsToRowsRoundTripTest  = property $ do
  events <- forAll genShelleyEraUtxoEvents --UtxoGen.genUtxoEvents
  let f :: C.ChainPoint -> IO (Set C.TxIn)
      f C.ChainPointAtGenesis = pure  Set.empty
      f cp'                     = pure . Set.fromList . concatMap (Set.toList . Utxo.ueInputs) . filter(\(Utxo.UtxoEvent _ _ cp) -> cp == cp') $ events
      rows = concatMap Utxo.eventsToRows events
  computedEvent <- liftIO . Utxo.rowsToEvents f $ rows
  let postGenesisEvents = filter (\e -> C.ChainPointAtGenesis /= Utxo.ueChainPoint e) events
  length computedEvent === (length . fmap Utxo.ueChainPoint $ postGenesisEvents)
  List.sort computedEvent === List.sort events

-- Insert Utxo events in storage, and retreive the events
--
utxoStorageTest :: Property
utxoStorageTest = property $ do
  events <- forAll UtxoGen.genUtxoEvents
  (storedEvents :: [StorableEvent Utxo.UtxoHandle]) <-
    (liftIO . Utxo.open ":memory:") (Utxo.Depth 10)
     >>= liftIO . Storable.insertMany events
     >>= liftIO . Storable.getEvents
  Set.fromList storedEvents === Set.fromList events

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
  highSlotNo <- forAll $ Gen.integral $ Range.constantFrom 7 5 20
  chainPoints :: [C.ChainPoint]  <- forAll $ genChainPoints 2 highSlotNo
  events::[StorableEvent Utxo.UtxoHandle] <-
    forAll $ forM chainPoints genEventWithShelleyAddressAtChainPoint -- <&> concat
  let numOfEvents = length events
  depth <- forAll $ Gen.int (Range.constantFrom (numOfEvents - 1) 1 (numOfEvents + 1))
  indexer <- liftIO $ Utxo.open ":memory:" (Utxo.Depth depth)
             >>= liftIO . Storable.insertMany events
  let _start :: C.ChainPoint = head chainPoints -- the generator will alwys provide a non empty list
      _end :: C.ChainPoint = chainPoints !! (length chainPoints `div` 2)
      qInterval = Storable.QInterval _start _end
      qAddresses
        = List.nub  -- remove duplicate addresses
        . fmap (Utxo.UtxoAddress . Utxo._address)
        . concatMap (Set.toList . Utxo.ueUtxos)
        $ events
  results <- liftIO . traverse (Storable.query qInterval indexer) $ qAddresses
  let fetchedRows = concatMap (\(Utxo.UtxoResult rs) -> rs ) results
      slotNoFromStorage = List.sort . fmap Utxo._urSlotNo $ fetchedRows
      endIntervalSlotNo = case _end of
        C.ChainPointAtGenesis -> C.SlotNo 0
        C.ChainPoint sn _     -> sn

  last slotNoFromStorage === endIntervalSlotNo

-- TargetAddresses are the addresses in UTXO that we filter for.
-- Puporse of this test is to filter out utxos that have a different address than those in the TargetAddress list.
eventsAtAddressTest :: Property
eventsAtAddressTest = property $ do
    event <- head <$> forAll UtxoGen.genUtxoEvents
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

-- | Calling 'Utxo.getUtxoEvents' with target addresses that are extracted from all tx outputs from
-- the initial generated txs should return the same 'UtxoEvent's as if there was no provided target
-- addresses.
propUsingAllAddressesOfTxsAsTargetAddressesShouldReturnUtxosAsIfNoFilterWasApplied ::  Property
propUsingAllAddressesOfTxsAsTargetAddressesShouldReturnUtxosAsIfNoFilterWasApplied = property $ do
    utxoEventsWithTxs <- forAll UtxoGen.genUtxoEventsWithTxs
    forM_ utxoEventsWithTxs $ \(expectedUtxoEvent, block) -> do
        let txs = mockBlockTxs block
            expectedAddresses = mkTargetAddressFromTxs txs
        cover 50 "At least one address is used as a target address"
            $ isJust expectedAddresses
        cover 1 "No target addresses are provided"
            $ isNothing expectedAddresses
        let actualUtxoEvents =
                Utxo.getUtxoEvents expectedAddresses txs (Utxo.ueChainPoint expectedUtxoEvent)
        let filteredExpectedUtxoEvent =
                expectedUtxoEvent
                    { Utxo.ueUtxos =
                        Set.filter (\utxo -> isJust $ addressAnyToShelley $ Utxo._address utxo)
                                   (Utxo.ueUtxos expectedUtxoEvent)
                    }

        -- If the 'expectedUtxoEvent' only contain byron addresses, then 'filteredExpectedUtxoEvent'
        -- will have an empty set of utxos. In that scenario, the `getUtxoEvents` should not filter
        -- anything, so we just return 'pure ()'.
        if not (null $ Utxo.ueUtxos expectedUtxoEvent) && null (Utxo.ueUtxos filteredExpectedUtxoEvent)
           then pure ()
           else filteredExpectedUtxoEvent === actualUtxoEvents
 where
    mkTargetAddressFromTxs
      :: [C.Tx C.BabbageEra]
      -> Maybe TargetAddresses
    mkTargetAddressFromTxs txs =
        foldMap (\(C.Tx (C.TxBody C.TxBodyContent { C.txOuts }) _) -> mkTargetAddressFromTxOuts txOuts) txs

    mkTargetAddressFromTxOuts
      :: [C.TxOut C.CtxTx C.BabbageEra]
      -> Maybe TargetAddresses
    mkTargetAddressFromTxOuts txOuts =
        nonEmpty $ mapMaybe (\(C.TxOut addr _ _ _) -> addressAnyToShelley $ Utxo.toAddr addr) txOuts

-- | The property verifies that the 'Storable.resumeFromStorage' call returns at least a point which
-- is not 'C.ChainPointAtGenesis' when some events are inserted on disk.
propResumingShouldReturnAtLeastOneNonGenesisPointIfStoredOnDisk :: Property
propResumingShouldReturnAtLeastOneNonGenesisPointIfStoredOnDisk = property $ do
    events <- forAll UtxoGen.genUtxoEvents
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
    events <- forAll UtxoGen.genUtxoEvents
    depth <- forAll $ Gen.int (Range.linear 1 $ length events)

    -- We insert the events in the indexer, but for the test assertions, we discard the events in
    -- memory.
    indexer <- liftIO $ Utxo.open ":memory:" (Utxo.Depth depth)
    void $ liftIO $ Storable.insertMany events indexer

    resumablePoints <- liftIO $ Storable.resume indexer
    List.reverse (List.sort resumablePoints) === resumablePoints

propJsonRoundtripUtxoRow :: Property
propJsonRoundtripUtxoRow = property $ do
    utxoEvents <- forAll genUtxoEvents
    let utxoRows = concatMap Utxo.eventsToRows utxoEvents
    forM_ utxoRows $ \utxoRow -> Hedgehog.tripping utxoRow Aeson.encode Aeson.decode
