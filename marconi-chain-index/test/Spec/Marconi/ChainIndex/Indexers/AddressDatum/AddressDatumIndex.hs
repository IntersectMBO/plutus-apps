{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.ChainIndex.Indexers.AddressDatum.AddressDatumIndex
    ( tests
    ) where

import Cardano.Api qualified as C
import Control.Lens ((^.))
import Control.Monad (foldM, forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (fold)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Gen.Marconi.ChainIndex.Types (genAddressInEra, genChainPoints, genSimpleScriptData)
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Marconi.ChainIndex.Indexers.AddressDatum (AddressDatumDepth (AddressDatumDepth), AddressDatumHandle,
                                                 StorableEvent (AddressDatumIndexEvent),
                                                 StorableQuery (AddressDatumQuery, AddressDatumQuery, AllAddressesQuery),
                                                 StorableResult (AddressDatumResult, AllAddressesResult))
import Marconi.ChainIndex.Indexers.AddressDatum qualified as AddressDatum
import Marconi.Core.Storable qualified as Storable
import Spec.Marconi.ChainIndex.Indexers.AddressDatum.Utils (addressInEraToAddressAny)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit), testPropertyNamed)

tests :: TestTree
tests = localOption (HedgehogTestLimit $ Just 200) $
    testGroup "Spec.Marconi.Index.AddressDatum.AddressDatumIndex"
    [
      testPropertyNamed
          "All addresses from generated events are queryable from the index"
          "propAllAddressesAreQueryable"
          propAllAddressesAreQueryable
    , testPropertyNamed
          "All addresses from generated events are queryable from the index when specifying a query range within the range of indexed points"
          "propAllAddressesAreQueryableInGeneratedRange"
          propAllAddressesAreQueryableInGeneratedRange
    , testPropertyNamed
          "All addresses from generated events are queryable from the index when specifying a query range within the range of indexed points"
          "propAllAddressesAreQueryableInGeneratedRange"
          propAllAddressesAreQueryableInGeneratedRange
    , testPropertyNamed
          "No addresses from generated events are queryable from the index when specifying a query range where it's lowest slot is higher than the last indexed slot"
          "propNoAddressQueryableIfLowestSlotInQueryRangeIsHigherThanLastIndexedSlot "
          propNoAddressQueryableIfLowestSlotInQueryRangeIsHigherThanLastIndexedSlot
    , testPropertyNamed
          "All datums of each address in generated events are queryable from the index"
          "propAddressDatumAreQueryable"
          propAddressDatumAreQueryable
    -- TODO Reactivate when intervals become more clearer
    -- , testPropertyNamed
    --       "All datums of each address in generated events are queryable from the index when specifying a query range which the range of indexed points"
    --       "propAddressDatumAreQueryableInGeneratedRange"
    --       propAddressDatumAreQueryableInGeneratedRange
    , testPropertyNamed
          "Rewinding an index to a point which is later than the last indexed point should not alter then index"
          "propRewindingWithNewSlotShouldKeepIndexState "
          propRewindingWithNewSlotShouldKeepIndexState
    , testPropertyNamed
          "Rewinding an index to a point which is before than the last indexed point should bring the index to a previous state"
          "propRewindingWithOldSlotShouldBringIndexInPreviousState "
          propRewindingWithOldSlotShouldBringIndexInPreviousState
    , testPropertyNamed
          "The points that indexer can be resumed from should return at least the genesis point"
          "propResumingShouldReturnAtLeastTheGenesisPoint"
          propResumingShouldReturnAtLeastTheGenesisPoint
    , testPropertyNamed
          "The points that indexer can be resumed from should return at least non-genesis point when some data was indexed on disk"
          "propResumingShouldReturnAtLeastOneNonGenesisPointIfStoredOnDisk"
          propResumingShouldReturnAtLeastOneNonGenesisPointIfStoredOnDisk
    ]

-- | The property verifies that the addresses in those generated events are all queryable from the
-- index.
propAllAddressesAreQueryable :: Property
propAllAddressesAreQueryable = property $ do
    cps <- forAll $ genChainPoints 1 5
    events <- forAll $ forM cps genAddressDatumStorableEvent
    depth <- forAll $ Gen.int (Range.linear 1 $ length cps)
    initialIndex <- liftIO $ AddressDatum.open ":memory:" (AddressDatumDepth depth)
    finalIndex <- liftIO $ Storable.insertMany events initialIndex
    let addrs = Set.fromList
              $ concatMap (\(AddressDatumIndexEvent addressDatumMap _ _) ->
                  Map.keys addressDatumMap) events
    (AllAddressesResult actualAddrs) <- liftIO $ do
        Storable.query Storable.QEverything finalIndex AllAddressesQuery
    actualAddrs === addrs

-- | Property that any interval where it's highest slot is higher than the current indexed slot and
-- where it's lowest slot is lower that the last indexed slot should return the same as results as
-- QEverything.
propAllAddressesAreQueryableInGeneratedRange :: Property
propAllAddressesAreQueryableInGeneratedRange = property $ do
    cps <- forAll $ genChainPoints 2 5
    events <- forAll $ forM cps genAddressDatumStorableEvent
    depth <- forAll $ Gen.int (Range.linear 1 $ length cps)
    initialIndex <- liftIO $ AddressDatum.open ":memory:" (AddressDatumDepth depth)
    finalIndex <- liftIO $ Storable.insertMany events initialIndex

    r1 <- forAll $ Gen.element cps
    r2 <- forAll $ Gen.element (C.ChainPointAtGenesis : cps)

    let expectedAddrs = Set.fromList
              $ concatMap (\(AddressDatumIndexEvent addressDatumMap _ _) ->
                  Map.keys addressDatumMap)
              $ filter (\(AddressDatumIndexEvent _ _ cp) -> cp <= max r1 r2) events

    case (min r1 r2, max r1 r2) of
      (minCp, maxCp) -> do
          (AllAddressesResult actualAddrs) <- liftIO $ do
              Storable.query
                  (Storable.QInterval minCp maxCp)
                  finalIndex
                  AllAddressesQuery
          actualAddrs === expectedAddrs

          when (minCp < maxCp) $ do
              (AllAddressesResult actualAddrs') <- liftIO $ do
                  Storable.query
                      (Storable.QInterval maxCp minCp)
                      finalIndex
                      AllAddressesQuery
              Hedgehog.assert $ Set.null actualAddrs'

-- | Property that any interval where it's lowest slot is higher than the indexed slot
-- should return no result.
propNoAddressQueryableIfLowestSlotInQueryRangeIsHigherThanLastIndexedSlot :: Property
propNoAddressQueryableIfLowestSlotInQueryRangeIsHigherThanLastIndexedSlot = property $ do
    cps <- forAll $ genChainPoints 2 10
    events <- forAll $ forM (init $ init cps) genAddressDatumStorableEvent
    depth <- forAll $ Gen.int (Range.linear 1 $ length cps)
    initialIndex <- liftIO $ AddressDatum.open ":memory:" (AddressDatumDepth depth)
    finalIndex <- liftIO $ Storable.insertMany events initialIndex

    (AllAddressesResult actualAddrs) <- liftIO $ do
        Storable.query
            (Storable.QInterval (last $ init cps) (last cps))
            finalIndex
            AllAddressesQuery
    Hedgehog.assert $ Set.null actualAddrs

    (AllAddressesResult actualAddrs'') <- liftIO $ do
        Storable.query
            (Storable.QInterval (last cps) (last $ init cps))
            finalIndex
            AllAddressesQuery
    Hedgehog.assert $ Set.null actualAddrs''

    (AllAddressesResult actualAddrs''') <- liftIO $ do
        Storable.query
            (Storable.QInterval (head cps) C.ChainPointAtGenesis)
            finalIndex
            AllAddressesQuery
    Hedgehog.assert $ Set.null actualAddrs'''

    (AllAddressesResult actualAddrs'''') <- liftIO $ do
        Storable.query
            (Storable.QInterval C.ChainPointAtGenesis C.ChainPointAtGenesis)
            finalIndex
            AllAddressesQuery
    Hedgehog.assert $ Set.null actualAddrs''''

-- | The property verifies that the datums of each address in those generated events are all
-- queryable from the index.
propAddressDatumAreQueryable :: Property
propAddressDatumAreQueryable = property $ do
    cps <- forAll $ genChainPoints 1 5
    events <- forAll $ forM cps genAddressDatumStorableEvent
    depth <- forAll $ Gen.int (Range.linear 1 $ length cps)
    initialIndex <- liftIO $ AddressDatum.open ":memory:" (AddressDatumDepth depth)
    finalIndex <- liftIO $ Storable.insertMany events initialIndex
    let addressDatumsMap = indexEventsToAddressDatumsMap events
    forM_ (Map.toList addressDatumsMap) $ \(addr, expectedDatums) -> do
        (AddressDatumResult actualDatums) <- liftIO $ do
            Storable.query Storable.QEverything finalIndex $ AddressDatumQuery addr
        actualDatums === expectedDatums
 where
    indexEventsToAddressDatumsMap
         :: [Storable.StorableEvent AddressDatumHandle]
         -> Map C.AddressAny (Set C.ScriptData)
    indexEventsToAddressDatumsMap events =
        indexEventToAddressDatumsMap $ fold events

    indexEventToAddressDatumsMap
         :: Storable.StorableEvent AddressDatumHandle
         -> Map C.AddressAny (Set C.ScriptData)
    indexEventToAddressDatumsMap (AddressDatumIndexEvent addressDatumMap datumMap _chainPoint) =
        Map.fromListWith (<>)
            $ foldMap (\(addr, datumHashes) -> [(addr, resolveMapKeys datumHashes datumMap)])
            $ Map.toList addressDatumMap

    resolveMapKeys
        :: (Ord k, Ord v)
        => Set k
        -> Map k v
        -> Set v
    resolveMapKeys keys m =
        -- TODO Not efficient to convert back n forth between Set
        Set.fromList $ mapMaybe (\k -> Map.lookup k m) $ Set.toList keys

-- | The property verifies that the datums of each address in those generated events are all
-- queryable from the index given a 'C.ChainPoint' interval.
-- propAddressDatumAreQueryableInGeneratedRange  :: Property
-- propAddressDatumAreQueryableInGeneratedRange = property $ do
--     cps <- forAll $ genChainPoints 1 5
--     events <- forAll $ forM cps genAddressDatumStorableEvent
--     depth <- forAll $ Gen.int (Range.linear 1 $ length cps)
--     initialIndex <- liftIO $ AddressDatum.open ":memory:" (AddressDatumDepth depth)
--     finalIndex <- liftIO $ Storable.insertMany events initialIndex

--     r1 <- forAll $ Gen.element cps
--     r2 <- forAll $ Gen.element (C.ChainPointAtGenesis : cps)
--     let addressDatumsMap =
--             indexEventsToAddressDatumsMap
--             $ filter (\(AddressDatumIndexEvent _ _ cp) -> cp <= max r1 r2) events

--     forM_ (Map.toList addressDatumsMap) $ \(addr, expectedDatums) -> do
--         case (min r1 r2, max r1 r2) of
--           (minCp, maxCp) -> do
--               (AddressDatumResult actualDatums) <- liftIO $ do
--                   Storable.query
--                       (Storable.QInterval minCp maxCp)
--                       finalIndex
--                       (AddressDatumQuery addr)
--               actualDatums === expectedDatums

--               when (minCp < maxCp) $ do
--                   (AddressDatumResult actualDatums') <- liftIO $ do
--                       Storable.query
--                           (Storable.QInterval maxCp minCp)
--                           finalIndex
--                           (AddressDatumQuery addr)
--                   Hedgehog.assert $ Set.null actualDatums'
--  where
--     indexEventsToAddressDatumsMap
--          :: [Storable.StorableEvent AddressDatumHandle]
--          -> Map C.AddressAny (Set C.ScriptData)
--     indexEventsToAddressDatumsMap events =
--         indexEventToAddressDatumsMap $ fold events

--     indexEventToAddressDatumsMap
--          :: Storable.StorableEvent AddressDatumHandle
--          -> Map C.AddressAny (Set C.ScriptData)
--     indexEventToAddressDatumsMap (AddressDatumIndexEvent addressDatumMap datumMap _chainPoint) =
--         Map.fromListWith (<>)
--             $ foldMap (\(addr, datumHashes) -> [(addr, resolveMapKeys datumHashes datumMap)])
--             $ Map.toList addressDatumMap

--     resolveMapKeys
--         :: (Ord k, Ord v)
--         => Set k
--         -> Map k v
--         -> Set v
--     resolveMapKeys keys m =
--         -- TODO Not efficient to convert back n forth between Set
--         Set.fromList $ mapMaybe (\k -> Map.lookup k m) $ Set.toList keys

-- | The property verifies that rewinding an index to a 'C.ChainPoint' later than the last one we
-- indexed will yield an index with an unchanged state.
--
-- TODO Think about using the Conversion adapter in order to take advantage of the properties
-- defined in Spec.hs in marconi-core.
propRewindingWithNewSlotShouldKeepIndexState :: Property
propRewindingWithNewSlotShouldKeepIndexState = property $ do
    cps <- forAll $ genChainPoints 1 5
    events <- forAll $ forM cps genAddressDatumStorableEvent
    initialIndex <- liftIO $ AddressDatum.open ":memory:" (AddressDatumDepth 1)
    finalIndex <- liftIO $ foldM insertAndRewind initialIndex events
    let addrs = Set.fromList
              $ concatMap (\(AddressDatumIndexEvent addressDatumMap _ _) ->
                  Map.keys addressDatumMap) events
    (AllAddressesResult actualAddrs) <- liftIO $ do
        Storable.query Storable.QEverything finalIndex AllAddressesQuery
    actualAddrs === addrs

 where
    insertAndRewind index e@(AddressDatumIndexEvent _ _ cp) = do
        newIndex <- Storable.insert e index
        fmap (fromMaybe newIndex) (Storable.rewind cp newIndex)

-- | The property verifies that inserting events in an index and rewinding that index to a previous
-- slot will yield an empty index.
propRewindingWithOldSlotShouldBringIndexInPreviousState :: Property
propRewindingWithOldSlotShouldBringIndexInPreviousState = property $ do
    cps <- forAll $ genChainPoints 1 5
    events <- forAll $ forM cps genAddressDatumStorableEvent
    initialIndex <- liftIO $ AddressDatum.open ":memory:" (AddressDatumDepth 1)
    finalIndex <- liftIO $ foldM (insertAndRewindToPreviousPoint cps) initialIndex events
    (AllAddressesResult actualAddrs) <- liftIO $ do
        Storable.query Storable.QEverything finalIndex AllAddressesQuery
    Hedgehog.assert $ List.null actualAddrs
 where
    insertAndRewindToPreviousPoint cps index e@(AddressDatumIndexEvent _ _ cp) = do
        newIndex <- Storable.insert e index
        fmap (fromMaybe newIndex) (Storable.rewind (previousChainPoint cp cps) newIndex)

    previousChainPoint :: C.ChainPoint -> [C.ChainPoint] -> C.ChainPoint
    previousChainPoint cp cps =
        case List.elemIndex cp cps of
          Nothing -> C.ChainPointAtGenesis
          Just i ->
            case List.splitAt i cps of
              ([], _)     -> C.ChainPointAtGenesis
              (before, _) -> last before

-- | The property verifies that the 'Storable.resumeFromStorage' call returns at least the
-- 'C.ChainPointAtGenesis' point.
--
-- TODO: ChainPointAtGenesis should always be returned by default. Don't need this property test.
--
-- TODO: Add a property test which makes sure that the points are ordered.
propResumingShouldReturnAtLeastTheGenesisPoint :: Property
propResumingShouldReturnAtLeastTheGenesisPoint = property $ do
    cps <- forAll $ genChainPoints 1 5
    events <- forAll $ forM (init cps) genAddressDatumStorableEvent
    initialIndex <- liftIO $ AddressDatum.open ":memory:" (AddressDatumDepth 1)
    finalIndex <- liftIO $ Storable.insertMany events initialIndex

    resumablePoints <- liftIO $ Storable.resumeFromStorage $ finalIndex ^. Storable.handle
    Hedgehog.assert $ List.elem C.ChainPointAtGenesis resumablePoints

-- | The property verifies that the 'Storable.resumeFromStorage' call returns at least a point which
-- is not 'C.ChainPointAtGenesis' when some events are inserted on disk.
propResumingShouldReturnAtLeastOneNonGenesisPointIfStoredOnDisk :: Property
propResumingShouldReturnAtLeastOneNonGenesisPointIfStoredOnDisk = property $ do
    cps <- forAll $ genChainPoints 1 5
    events <- forAll $ forM (init cps) genAddressDatumStorableEvent
    initialIndex <- liftIO $ AddressDatum.open ":memory:" (AddressDatumDepth 1)
    finalIndex <- liftIO $ Storable.insertMany events initialIndex
        >>= Storable.insert (AddressDatum.toAddressDatumIndexEvent Nothing [] (last cps))

    resumablePoints <- liftIO $ Storable.resumeFromStorage $ finalIndex ^. Storable.handle
    Hedgehog.assert $ length resumablePoints >= 2

genAddressDatumStorableEvent :: C.ChainPoint -> Gen (Storable.StorableEvent AddressDatumHandle)
genAddressDatumStorableEvent cp = do
    addresses <- fmap addressInEraToAddressAny
             <$> Gen.list (Range.linear 1 5) (genAddressInEra C.BabbageEra)
    addressDatums <- forM addresses $ \address -> do
        scriptDats <- fmap (\dats -> fmap (\dat -> (C.hashScriptData dat, dat)) dats)
                    $ Gen.list (Range.linear 1 5) genSimpleScriptData
        datumMap <- Map.fromList <$> Gen.subsequence scriptDats
        pure (address, Set.fromList $ fmap fst scriptDats, datumMap)
    let addressDatumsMap = Map.fromList $ fmap (\(addr, datums, _) -> (addr, datums)) addressDatums
    let datumMap = foldMap (\(_, _, dm) -> dm) addressDatums
    pure $
        AddressDatumIndexEvent
            addressDatumsMap
            datumMap
            cp
