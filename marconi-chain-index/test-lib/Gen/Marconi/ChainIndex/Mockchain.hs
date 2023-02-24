{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections      #-}

module Gen.Marconi.ChainIndex.Mockchain
    ( Mockchain
    , MockBlockHeader(..)
    , MockBlock(..)
    , genMockchain
    )
where

import Cardano.Api qualified as C
import Control.Monad (foldM)
import Data.Set (Set)
import Data.Set qualified as Set
import Gen.Cardano.Api.Typed qualified as CGen
import Gen.Marconi.ChainIndex.Types (genHashBlockHeader, genTxOutTxContext, nonEmptySubset)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helpers (emptyTxBodyContent)

type Mockchain era = [MockBlock era]

data MockBlockHeader = MockBlockHeader
    { mockBlockHeaderSlotNo  :: !C.SlotNo
    , mockBlockHeaderHash    :: !(C.Hash C.BlockHeader)
    , mockBlockHeaderBlockNo :: !C.BlockNo
    } deriving (Show)

data MockBlock era = MockBlock
    { mockBlockChainPoint :: !MockBlockHeader
    , mockBlockTxs        :: ![C.Tx era]
    } deriving (Show)

genMockchain :: Gen (Mockchain C.BabbageEra)
genMockchain = do
    maxSlots <- Gen.word64 (Range.linear 1 5)
    blockHeaderHash <- genHashBlockHeader
    let blockHeaders =
            fmap (\s -> MockBlockHeader (C.SlotNo s) blockHeaderHash (C.BlockNo s))
                 [0..maxSlots]
    txIns <- Set.singleton <$> CGen.genTxIn
    snd <$> foldM f (txIns, []) blockHeaders
  where
    f :: (Set C.TxIn, Mockchain C.BabbageEra)
      -> MockBlockHeader
      -> Gen (Set C.TxIn, Mockchain C.BabbageEra)
    f (utxoSet, mockchain) bh = do
        utxosAsTxInput <- nonEmptySubset utxoSet
        txBodyContent <- genTxBodyContentFromTxIns $ Set.toList utxosAsTxInput
        txBody <- either (fail . show) pure $ C.makeTransactionBody txBodyContent
        let newTx = C.makeSignedTransaction [] txBody
        let txId = C.getTxId txBody
        let newUtxoRefs = Set.fromList
                $ fmap (\(txIx, _) -> C.TxIn txId (C.TxIx txIx))
                $ zip [0..]
                $ C.txOuts txBodyContent
        pure ( Set.union newUtxoRefs $ Set.difference utxoSet utxosAsTxInput
             , mockchain ++ [MockBlock bh [newTx]]
             )

genTxBodyContentFromTxIns
    :: [C.TxIn]
    -> Gen (C.TxBodyContent C.BuildTx C.BabbageEra)
genTxBodyContentFromTxIns inputs = do
    txBodyContent <-
        emptyTxBodyContent (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra)
            <$> CGen.genProtocolParameters
    txOuts <- Gen.list (Range.linear 1 5) $ genTxOutTxContext C.BabbageEra
    pure $ txBodyContent
        { C.txIns = fmap (, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) inputs
        , C.txOuts = txOuts
        }
