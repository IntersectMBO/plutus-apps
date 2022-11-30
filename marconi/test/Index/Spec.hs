{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Index.Spec (tests) where

import Cardano.Api qualified as C
import Control.Lens.Operators ((^.), (^..))
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Maybe (catMaybes, fromJust)
import Gen.Cardano.Api.Typed qualified as CGen
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Seed (nextInteger, random)
import Hedgehog.Range qualified as Range
import Marconi.Index.Utxos qualified as Utxos
import Marconi.Indexers (uTxo, uTxoEvents)
import Marconi.Types (CurrentEra, TargetAddresses)
import RewindableIndex.Index.VSplit qualified as Ix
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests = testGroup "Marconi.Index.Specs" $
    [testPropertyNamed  "Utxo: build utxos from Tx" "Spec: Utxo from Cardano.Api.Tx" txToUtxoTest
    , testPropertyNamed "Utxo: save and retrieve UtxoEvents" "Spec: Save and retreive UtxoEvents" roundTripUtxoEventsTest
    ]

txToUtxoTest ::  Property
txToUtxoTest = property $ do
    t@(C.Tx (C.TxBody C.TxBodyContent{C.txOuts}) _)  <- forAll $ CGen.genTx C.BabbageEra
    let (targetAddresses :: Maybe TargetAddresses ) = addressesFromTxOuts txOuts
    let (lt :: Int) =  maybe 0 List.NonEmpty.length targetAddresses
    let (utxos :: [Utxos.Utxo]) = uTxo targetAddresses t
    -- we should not see any target addresses in the utxos
    (areTargetAddressesInUtxos targetAddresses utxos) === True

    let utxos = uTxo Nothing t  :: [Utxos.Utxo]
    -- there should be some utxos
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


areTargetAddressesInUtxos :: (Maybe TargetAddresses) -> [Utxos.Utxo] -> Bool
areTargetAddressesInUtxos Nothing _ = True
areTargetAddressesInUtxos (Just targets) utxos =
    let
        addressesFromUtxos = catMaybes . fmap (\x -> addressAnyToShelley ( x ^. Utxos.utxoAddress)) $ utxos
        targetList = toList targets
    in
        all (`elem` targetList)  addressesFromUtxos


-- areAddressesInAllRow :: [C.AddressAny] -> [Utxos.UtxoRow] -> Bool
-- areAddressesInAllRow addresses rows = all (`elem` rowAddresses) addresses
--     where
--         rowAddresses = fmap (\r -> r ^. Utxos.utxoRowUtxo . Utxos.utxoAddress) rows


isAddressInUtxoRow :: C.AddressAny -> Utxos.UtxoRow -> Bool
isAddressInUtxoRow addr row = addr `elem` (row  ^. Utxos.utxoRowUtxo ^.. Utxos.utxoAddress)


-- areAddressesInUtxos :: targetAddresses -> [Utxos.Utxo] -> Bool
-- areAddressesInUtxos targets utxos =
--     let
--     where
--         isElem (Utxos.Utxo addrAny _ _ _ _ _) = addrAny `elem` targets

-- areAddressesInAllRow :: [C.AddressAny] -> [Utxos.UtxoRow] -> Bool
-- areAddressesInAllRow addresses rows = all (`elem` rowAddresses) addresses
--     where
--         rowAddresses = fmap (\r -> r ^. Utxos.utxoRowUtxo . Utxos.utxoAddress) rows


-- isAddressInUtxoRow :: C.AddressAny -> Utxos.UtxoRow -> Bool
-- isAddressInUtxoRow addr row = addr `elem` (row  ^. Utxos.utxoRowUtxo ^.. Utxos.utxoAddress)
