{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Index.Spec (tests) where

import Control.Lens (folded)
import Control.Lens.Operators ((^.), (^..))
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.List.NonEmpty (nonEmpty, toList)
import Data.Maybe (catMaybes, fromJust)
import Database.SQLite.Simple qualified as SQLite

import Hedgehog (Gen, Property, diff, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Api qualified as C
import Gen.Cardano.Api.Typed qualified as CGen
import Marconi.Index.Utxos qualified as Utxos
import Marconi.Indexers (uTxo, uTxoEvents)
import Marconi.Types (CurrentEra, TargetAddresses)
import RewindableIndex.Index.VSplit qualified as Ix

tests :: TestTree
tests = testGroup "Marconi.Index.Specs" $
    [testPropertyNamed "Index-utxos-build" "Spec. Utxo from Cardano.Api.Tx with targetAddresses"
     txToUtxoTest
    , testPropertyNamed "Index-utxos-event-at-address" "Spec. Filter UtxoEvents for an address"
      addressFilteredEventsTest
    , testPropertyNamed "Index-utxos-store" "Spec. Save and retreive UtxoEvents"
      utxoStorageTest
    , testPropertyNamed "Index-utxos-hot-store" "Spec. Save and retreive UtxoEvents from hot-store only"
      hotstoreUtxoEventTest
    ]

addressFilteredEventsTest :: Property
addressFilteredEventsTest = property $ do
    event <- forAll $ genEvents -- force db flush
    let (addresses :: [C.AddressAny]) = nub( event ^. Utxos.utxoEventUtxos ^.. folded . Utxos.utxoAddress)
        event' =  Utxos.eventAtAddress (head addresses) event
    Hedgehog.assert ((null event') == False)
    1 === (length . nub) ( (head event') ^. Utxos.utxoEventUtxos ^.. folded . Utxos.utxoAddress)

txToUtxoTest ::  Property
txToUtxoTest = property $ do
    t@(C.Tx (C.TxBody C.TxBodyContent{C.txOuts}) _)  <- forAll $ CGen.genTx C.BabbageEra
    let (targetAddresses :: Maybe TargetAddresses ) = addressesFromTxOuts txOuts
    let (utxos :: [Utxos.Utxo]) = uTxo targetAddresses t
    case targetAddresses of
        Nothing         ->  (length utxos) === (length txOuts)
        Just targets    ->
            ( nub
              . catMaybes
              . fmap (\x -> addressAnyToShelley (x ^. Utxos.utxoAddress))
              $ utxos) === (nub . toList $ targets)

genBlockNo :: Gen C.BlockNo
genBlockNo = C.BlockNo <$> Gen.word64 Range.constantBounded

genEvents :: Gen Utxos.UtxoEvent
genEvents = do
    slotNo <- CGen.genSlotNo
    blockNo  <- genBlockNo
    txs <- Gen.list (Range.linear 2 5)(CGen.genTx C.ShelleyEra)
    pure . fromJust $ uTxoEvents Nothing slotNo blockNo txs

hotstoreUtxoEventTest :: Property
hotstoreUtxoEventTest  = property $ do
    event <- forAll $ genEvents
    let (rows :: [Utxos.UtxoRow]) = Utxos.toRows event
        (addresses :: [C.AddressAny]) = nub . fmap (\r -> r ^. Utxos.utxoRowUtxo . Utxos.utxoAddress ) $ rows
    ndx <- liftIO $ Utxos.open ":memory:" (Utxos.Depth 2196) -- no db writes, test memory only
    ix <- liftIO $ Ix.insert event ndx
    storeResults <- liftIO $ ( forM addresses (\addr -> Utxos.queryPlusVolatile ix addr) :: IO [Utxos.Result ] )
    let (fromQuery  :: [Utxos.UtxoRow] ) = concat . catMaybes $ storeResults
    let conn =  ix ^. Ix.handle
    liftIO $ SQLite.close conn
    Hedgehog.diff (length rows) (==) (length fromQuery)
    rows === fromQuery


utxoStorageTest :: Property
utxoStorageTest  = property $ do
    ndx <- liftIO $ Utxos.open ":memory:" (Utxos.Depth 2) -- force storage to use sqlite + buffer + volatile
    events <- forAll $ Gen.list (Range.linear 2 4) genEvents -- force db flush
    let (rows :: [Utxos.UtxoRow]) = nub . concatMap Utxos.toRows $ events
        (addresses :: [C.AddressAny]) = nub . fmap (\r -> r ^. Utxos.utxoRowUtxo . Utxos.utxoAddress ) $ rows
    ix <- liftIO $ Ix.insertL (events) ndx
    rowsFromStore <- liftIO $ (concat . catMaybes) <$> forM addresses (\a -> Utxos.queryPlusVolatile ix a  )
    (null rowsFromStore) === False
    let (tid :: Utxos.UtxoRow -> C.AddressAny) = (\r -> r ^. Utxos.utxoRowUtxo . Utxos.utxoAddress)
    (all (`elem` (tid <$> rows)) (tid <$> rowsFromStore)) === True

addressesFromTxOuts :: [C.TxOut C.CtxTx CurrentEra] -> Maybe TargetAddresses
addressesFromTxOuts [(C.TxOut addressInEra _ _ _)]
    = nonEmpty
    . nub
    . catMaybes
    . (fmap (addressAnyToShelley . Utxos.toAddr) )
    $ [addressInEra]
addressesFromTxOuts _ = Nothing

addressAnyToShelley :: C.AddressAny -> Maybe (C.Address C.ShelleyAddr)
addressAnyToShelley  (C.AddressShelley a )=Just a
addressAnyToShelley  _                     = Nothing
