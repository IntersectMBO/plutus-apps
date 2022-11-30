{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Index.Spec (tests) where

import Cardano.Api qualified as C
import Control.Lens.Operators ((^.))
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.Maybe (catMaybes, fromJust)
import Gen.Cardano.Api.Typed qualified as CGen
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Marconi.Index.Utxos qualified as Utxos
import Marconi.Indexers (uTxo, uTxoEvents)
import RewindableIndex.Index.VSplit qualified as Ix
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests = testGroup "Marconi.Index.Specs" $
    [testPropertyNamed "Utxo from Tx" "Spec: Utxo from Cardano.Api.Tx" txToUtxoTest
    , testPropertyNamed "Save UtxoEvents" "Spec: Save and retreive UtxoEvents" roundTripUtxoEventsTest
    ]

txToUtxoTest ::  Property
txToUtxoTest = property $ do
    t@(C.Tx (C.TxBody C.TxBodyContent{C.txOuts}) _)  <- forAll $ CGen.genTx C.ShelleyEra
    let utxos = uTxo Nothing t  :: [Utxos.Utxo]
    (length utxos ) === (length txOuts)
    null utxos === False

genBlockNo :: Gen C.BlockNo
genBlockNo = C.BlockNo <$> Gen.word64 Range.constantBounded

genEvents :: Gen Utxos.UtxoEvent
genEvents = do
    slotNo <- CGen.genSlotNo
    blockNo  <- genBlockNo
    txs <- Gen.list (Range.linear 2 10)(CGen.genTx C.ShelleyEra)
    pure . fromJust $ uTxoEvents Nothing slotNo blockNo txs

roundTripUtxoEventsTest :: Property
roundTripUtxoEventsTest  = property $ do
    ndx <- liftIO $ Utxos.open ":memory:" (Utxos.Depth 1)
    events <- forAll $ Gen.list (Range.linear 2 5) genEvents -- force db flush
    let (rows :: [Utxos.UtxoRow]) = nub . concatMap Utxos.toRows $ events
    let (addresses :: [C.AddressAny]) = nub . fmap (\r -> r ^. Utxos.utxoRowUtxo . Utxos.utxoAddress ) $ rows
    ix <- liftIO $ Ix.insertL (events) ndx
    let queryIx addr = (ix ^. Ix.query) ix addr [] -- events finding them in the events is the trivial case
    rowsFromStore <- liftIO $ (concat . catMaybes) <$> forM addresses queryIx -- queryByAddress
    (null rowsFromStore) === False
    (all (`elem` rows) rowsFromStore) === True -- check what we stored is the same as what we fetched
