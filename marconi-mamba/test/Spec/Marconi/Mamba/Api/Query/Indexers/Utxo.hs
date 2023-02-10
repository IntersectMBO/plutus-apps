{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Marconi.Mamba.Api.Query.Indexers.Utxo (tests) where

import Control.Concurrent.STM (atomically)
import Control.Lens.Operators ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.List.NonEmpty (nonEmpty, toList)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set

import Hedgehog (Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Api qualified as C
import Marconi.Mamba.Api.Types (HasIndexerEnv (uiIndexer))

import Gen.Marconi.ChainIndex.Types (genEvents)
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Types (TargetAddresses)
import Marconi.Core.Storable (StorableEvent)
import Marconi.Core.Storable qualified as Storable
import Marconi.Mamba.Api.Query.Indexers.Utxo qualified as UIQ


deriving instance Eq (StorableEvent Utxo.UtxoHandle)
deriving instance Ord (StorableEvent Utxo.UtxoHandle)

-- | Insert events, and perform the callback
mocUtxoWorker
  :: (UIQ.UtxoIndexer -> IO ())
  -> [Utxo.StorableEvent Utxo.UtxoHandle]
  -> Utxo.Depth
  -> IO ()
mocUtxoWorker callback events depth =
  Utxo.open ":memory:" depth >>= Storable.insertMany events >>= callback

tests :: TestTree
tests = testGroup "marconi-mamba-utxo query Api Specs"
    [testPropertyNamed
     "marconi-mamba-utxo query-target-addresses"
     "Spec. Insert events and query for utxo's with address in the generated ShelleyEra targetAddresses"
     queryTargetAddressTest
    ]

-- | generate some Utxo events, store them and fetch them.
--
queryTargetAddressTest :: Property
queryTargetAddressTest = property $ do
  events <- forAll $ Gen.list (Range.linear 2 5) genEvents
  depth <- forAll $ Gen.int (Range.linear 5 7)
  let
    targetAddresses :: Maybe TargetAddresses
    targetAddresses
      = nonEmpty
      . mapMaybe addressAnyToShelley
      . nub -- required to remove the potential duplicate addresses
      . fmap Utxo._address
      . concatMap (Set.toList . Utxo.ueUtxos)
      $ events
  env <- liftIO . UIQ.initializeEnv $ targetAddresses
  let
    callback :: Utxo.UtxoIndexer -> IO ()
    callback = atomically . UIQ.writeTMVar' (env ^. uiIndexer) -- update the indexer
  liftIO . mocUtxoWorker callback events $ Utxo.Depth depth
  fetchedRows <-
    liftIO
    . fmap concat
    . traverse (UIQ.findByAddress env)
    . fmap C.toAddressAny
    . maybe [] toList
    $ targetAddresses
  let rows = concatMap Utxo.eventsToRows events
  fetchedRows === rows

addressAnyToShelley :: C.AddressAny -> Maybe (C.Address C.ShelleyAddr)
addressAnyToShelley  (C.AddressShelley a ) = Just a
addressAnyToShelley  _                     = Nothing
