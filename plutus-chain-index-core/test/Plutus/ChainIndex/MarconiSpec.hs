{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Plutus.ChainIndex.MarconiSpec (tests) where

import Control.Monad.IO.Class (liftIO)

import Generators qualified as Gen
import Hedgehog.Internal.Property (Property, forAll, property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Control.Lens (filtered, folded, to, (^..), (^?))
import Control.Tracer (nullTracer)
import Data.Default (def)
import Hedgehog qualified
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Plutus.ChainIndex (ChainSyncBlock (Block), Tip (TipAtGenesis), appendBlocks, citoAddress, citxOutputs, pageItems,
                          pageOf, utxoSetAtAddress)
import Plutus.ChainIndex.Api (UtxosResponse (UtxosResponse), page)
import Plutus.ChainIndex.Marconi (ChainIndexIndexers (ChainIndexIndexers), ChainIndexIndexersMVar,
                                  RunRequirements (RunRequirements), boxChainIndexIndexers, runChainIndexEffects)
import Plutus.ChainIndex.Types (_ValidTx, chainIndexTxOutputs)

tests :: TestTree
tests = testGroup "Plutus.ChainIndex.MarconiSpec"
    [ testGroup "testSetAtAddress"
        [ testPropertyNamed "appendQueryLoop" "setAtAddressRoundtripProperty"
            setAtAddressRoundtripProperty
        ]
    ]

newChainIndexIndexers :: IO ChainIndexIndexersMVar
newChainIndexIndexers = do
  indexers <- ChainIndexIndexers
      <$> Utxo.open ":memory:" (Utxo.Depth 10)
  boxChainIndexIndexers indexers

setAtAddressRoundtripProperty :: Property
setAtAddressRoundtripProperty = property $ do
  (tip, block) <- forAll $ Gen.evalTxGenState Gen.genNonEmptyBlock
  indexers <- liftIO newChainIndexIndexers
  let fstTxAddr = block ^? folded . citxOutputs . chainIndexTxOutputs . to citoAddress
  let _originalTxOuts = block ^.. folded . citxOutputs
  _txouts <- maybe
      (pure $ Right $ UtxosResponse TipAtGenesis $ pageOf def mempty)
      (\addr -> do
          liftIO $ runChainIndexEffects (RunRequirements nullTracer indexers) $ do
              appendBlocks [Block tip (map (, def) block)]
              utxoSetAtAddress def addr
      )
      fstTxAddr
  -- TODO Check TxOut once we have a working indexer
  Hedgehog.success
