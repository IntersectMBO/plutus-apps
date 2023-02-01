{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.UtxoIndexersQuery (tests) where

import Control.Concurrent.STM (atomically)
import Control.Lens.Operators ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.List.NonEmpty (fromList)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set

import Hedgehog (Property, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Api qualified as C
import Gen.Marconi.Types (genEvents)
import Marconi.Api.Types (HasUtxoIndexerEnv (uiIndexer))
import Marconi.Api.UtxoIndexersQuery qualified as UIQ
import Marconi.Index.Utxo qualified as Utxo
import Marconi.Types (TargetAddresses)
import RewindableIndex.Storable (StorableEvent)
import RewindableIndex.Storable qualified as Storable

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
  events <- forAll $ Gen.list (Range.linear 1 3) genEvents
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
