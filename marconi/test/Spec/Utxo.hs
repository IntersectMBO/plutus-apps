{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Utxo (tests) where

import Control.Lens (folded)
import Control.Lens.Operators ((^.), (^..))
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.List.NonEmpty (nonEmpty, toList)
import Data.Maybe (catMaybes, fromJust)

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
import RewindableIndex.Index.VSplit qualified as Ix

tests :: TestTree
tests = testGroup "Marconi.Index.Specs" $
    [testPropertyNamed
     "marconi-index utxos event builder" "Spec. Utxo from Cardano.Api.Tx with targetAddresses"
     txToUtxoTest
    , testPropertyNamed
      "marconi-index utxos event at address" "Spec. Filter UtxoEvents for an address"
      addressFilteredEventsTest
    , testPropertyNamed "marconi-index utxos in-memory store" "Spec. Save and retreive UtxoEvents"
      utxoStorageTest
    ]

addressFilteredEventsTest :: Property
addressFilteredEventsTest = property $ do
    event <- forAll $ genEvents
    let (addresses :: [C.AddressAny]) = nub( event ^. Utxo.utxoEventUtxos ^.. folded . Utxo.utxoAddress)
        event' =  Utxo.eventAtAddress (head addresses) event
    Hedgehog.assert ((null event') == False)
    1 === (length . nub) ( (head event') ^. Utxo.utxoEventUtxos ^.. folded . Utxo.utxoAddress)

txToUtxoTest ::  Property
txToUtxoTest = property $ do
    t@(C.Tx (C.TxBody C.TxBodyContent{C.txOuts}) _)  <- forAll $ CGen.genTx C.BabbageEra
    let (targetAddresses :: Maybe TargetAddresses ) = addressesFromTxOuts txOuts
    let (utxos :: [Utxo.Utxo]) = Utxo.getUtxos targetAddresses t
    case targetAddresses of
        Nothing         ->  (length utxos) === (length txOuts)
        Just targets    ->
            ( nub
              . catMaybes
              . fmap (\x -> addressAnyToShelley (x ^. Utxo.utxoAddress))
              $ utxos) === (nub . toList $ targets)

genBlockNo :: Gen C.BlockNo
genBlockNo = C.BlockNo <$> Gen.word64 Range.constantBounded

genEvents :: Gen Utxo.UtxoEvent
genEvents = do
    slotNo <- CGen.genSlotNo
    blockNo  <- genBlockNo
    txs <- Gen.list (Range.linear 2 5)(CGen.genTx C.ShelleyEra)
    pure . fromJust $ Utxo.getUtxoEvents Nothing slotNo blockNo txs

utxoStorageTest :: Property
utxoStorageTest = property $ do
    depth <- forAll $ Gen.int (Range.linear 1  5) -- force DB writes
    ndx <- liftIO $ Utxo.open ":memory:" (Utxo.Depth depth)
    events <- forAll $ Gen.list (Range.linear 1 10) genEvents
    let (rows :: [Utxo.UtxoRow]) = nub . concatMap Utxo.toRows $ events
        (addresses :: [C.AddressAny]) = nub . fmap (\r -> r ^. Utxo.utxoRowUtxo . Utxo.utxoAddress ) $ rows
    ix <- liftIO $ Ix.insertL (events) ndx
    rowsFromStore <- liftIO $ (concat . catMaybes) <$> forM addresses (\a -> Utxo.queryPlusVolatile ix a  )
    (null rowsFromStore) === False
    let (tid :: Utxo.UtxoRow -> C.AddressAny) = (\r -> r ^. Utxo.utxoRowUtxo . Utxo.utxoAddress)
    (all (`elem` (tid <$> rows)) (tid <$> rowsFromStore)) === True

addressesFromTxOuts :: [C.TxOut C.CtxTx CurrentEra] -> Maybe TargetAddresses
addressesFromTxOuts [(C.TxOut addressInEra _ _ _)]
    = nonEmpty
    . nub
    . catMaybes
    . (fmap (addressAnyToShelley . Utxo.toAddr) )
    $ [addressInEra]
addressesFromTxOuts _ = Nothing

addressAnyToShelley :: C.AddressAny -> Maybe (C.Address C.ShelleyAddr)
addressAnyToShelley  (C.AddressShelley a )=Just a
addressAnyToShelley  _                     = Nothing
