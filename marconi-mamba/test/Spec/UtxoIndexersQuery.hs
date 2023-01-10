{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Spec.UtxoIndexersQuery (tests) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar)
import Control.Lens.Operators ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (STM)
import Data.List (nub)
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromJust, isJust)

import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Api qualified as C
import Gen.Cardano.Api.Typed qualified as CGen
import Marconi.Api.Types (DBQueryEnv (DBQueryEnv), UtxoQueryTMVar (UtxoQueryTMVar))
import Marconi.Api.UtxoIndexersQuery qualified as QApi
import Marconi.Index.Utxo (Depth (Depth), open)
import Marconi.Index.Utxo qualified as Utxo
import Marconi.Types ()
import RewindableIndex.Index.VSplit qualified as Ix


tests :: TestTree
tests = testGroup "marconi-mamba query specs" $
    [testPropertyNamed "marconi-mamba query" "Spec. round-trip store and query utxos " queryUtxoTest
    ]
dbpath :: FilePath
dbpath  = ":memory:"

-- | generate some Utxo events, store them and fetch them.
--
queryUtxoTest :: Property
queryUtxoTest = property $ do
    qTMVar <- liftIO $ atomically (newEmptyTMVar :: STM (TMVar Utxo.UtxoIndex) )
    let
        callback :: QApi.UtxoIndex -> IO QApi.UtxoIndex
        callback index = (atomically $ QApi.writeTMVar qTMVar  index ) >> pure index
        utxoQueryTMVar = UtxoQueryTMVar qTMVar

    depth <- forAll $ Gen.int (Range.linear 1  5) -- get some DB writes
    ix <- liftIO $ open dbpath (Depth depth)
    event <- forAll $ genEvents
    let
        rows :: [Utxo.UtxoRow]
        rows = Utxo.toRows event
        addresses :: [C.AddressAny]
        addresses = nub . fmap (\r -> r ^. Utxo.utxoRowUtxo . Utxo.utxoAddress ) $ rows
        targetAddressPairs :: [(C.AddressAny, C.Address C.ShelleyAddr)]
        targetAddressPairs
            = fmap (\(a, s) -> (a, fromJust s))
            . filter (\(_ ,s) -> isJust s)
            . (zip addresses)
            . fmap addressAnyToShelley
            $ addresses
        targetAddresses = snd <$> targetAddressPairs
        addressesAny = fst <$> targetAddressPairs
        rowsInserted =
            filter (\r -> (r ^. Utxo.utxoRowUtxo . Utxo.utxoAddress) `elem` addressesAny) rows
        env = DBQueryEnv utxoQueryTMVar (fromList targetAddresses )
    liftIO . void . mocUtxoWorker callback ix $ event
    rowsRetrieved <-
        liftIO
        . fmap (concat )
        . traverse (QApi.findByCardanoAddress env)
        . fmap fst
        $ targetAddressPairs
    rowsRetrieved === rowsInserted

-- | Insert events, and do the callback
mocUtxoWorker
    :: (QApi.UtxoIndex -> IO (QApi.UtxoIndex))
    -> QApi.UtxoIndex
    -> Utxo.UtxoEvent
    -> IO ()
mocUtxoWorker callback ix event =
     void (Ix.insert event ix >>= callback)

genBlockNo :: Gen C.BlockNo
genBlockNo = C.BlockNo <$> Gen.word64 Range.constantBounded

genEvents :: Gen Utxo.UtxoEvent
genEvents = do
    slotNo <- CGen.genSlotNo
    blockNo  <- genBlockNo
    txs <- Gen.list (Range.linear 3 10)(CGen.genTx C.ShelleyEra)
    pure . fromJust $ Utxo.getUtxoEvents Nothing slotNo blockNo txs

addressAnyToShelley :: C.AddressAny -> Maybe (C.Address C.ShelleyAddr)
addressAnyToShelley  (C.AddressShelley a )=Just a
addressAnyToShelley  _                     = Nothing
