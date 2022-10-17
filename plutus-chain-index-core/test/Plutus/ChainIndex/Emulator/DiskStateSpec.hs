{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Plutus.ChainIndex.Emulator.DiskStateSpec (tests) where

import Control.Lens
import Data.Set qualified as Set
import Plutus.ChainIndex.Emulator.DiskState qualified as DiskState
import Plutus.ChainIndex.Tx (ChainIndexTxOut (ChainIndexTxOut, citoValue), txOutsWithRef)

import Generators qualified as Gen
import Hedgehog (Property, forAll, property, (===))
import Ledger.Ada qualified as Ada
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests = do
  testGroup "emulator"
    [ testGroup "disk state"
        [ testPropertyNamed "same txOuts between AddressMap and ChainIndexTx" "addressMapAndTxShouldShareTxOuts" addressMapAndTxShouldShareTxOuts
        , testPropertyNamed "same txOuts between AssetClassMap and ChainIndexTx" "assetClassMapAndTxShouldShareTxOuts" assetClassMapAndTxShouldShareTxOuts
        ]
    ]

-- | DiskState._AddressMap and ChainIndexTx should share the exact same set of
-- transaction outputs.
addressMapAndTxShouldShareTxOuts :: Property
addressMapAndTxShouldShareTxOuts = property $ do
    chainIndexTx <- forAll $ Gen.evalTxGenState Gen.genTx
    let diskState = DiskState.fromTx chainIndexTx
        ciTxOutRefs = Set.fromList $ fmap snd $ txOutsWithRef chainIndexTx
        addressMapTxOutRefs =
          mconcat $ diskState ^.. DiskState.addressMap . DiskState.unCredentialMap . folded
    ciTxOutRefs === addressMapTxOutRefs

assetClassMapAndTxShouldShareTxOuts :: Property
assetClassMapAndTxShouldShareTxOuts = property $ do
    chainIndexTx <- forAll $ Gen.evalTxGenState Gen.genTx
    let diskState = DiskState.fromTx chainIndexTx
        ciTxOutRefs = Set.fromList
                    $ fmap snd
                    $ filter (\(ChainIndexTxOut{citoValue}, _) -> citoValue /= Ada.toValue (Ada.fromValue citoValue))
                    $ txOutsWithRef chainIndexTx
        assetClassMapTxOutRefs =
          mconcat $ diskState ^.. DiskState.assetClassMap . DiskState.unAssetClassMap . folded
    ciTxOutRefs === assetClassMapTxOutRefs
