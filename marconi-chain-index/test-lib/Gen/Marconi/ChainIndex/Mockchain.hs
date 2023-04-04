{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Marconi.ChainIndex.Mockchain
    ( Mockchain
    , C.BlockHeader(..)
    , MockBlock(..)
    , genMockchain
    , genTxBodyContentFromTxinsWihtPhase2Validation
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

deriving stock instance Show C.BlockHeader

data MockBlock era = MockBlock
    { mockBlockChainPoint :: !C.BlockHeader
    , mockBlockTxs        :: ![C.Tx era]
    } deriving Show

genMockchain :: Gen (Mockchain C.BabbageEra)
genMockchain = genMockchain' genTxBodyContentFromTxIns

genMockchain'
  :: ([C.TxIn] -> Gen (C.TxBodyContent C.BuildTx C.BabbageEra))
  -> Gen (Mockchain C.BabbageEra)
genMockchain' genTxBody = do
    maxSlots <- Gen.word64 (Range.linear 1 5)
    blockHeaderHash <- genHashBlockHeader
    let blockHeaders =
            fmap (\s -> C.BlockHeader (C.SlotNo s) blockHeaderHash (C.BlockNo s))
                 [0..maxSlots]
    txIns <- Set.singleton <$> CGen.genTxIn
    snd <$> foldM f (txIns, []) blockHeaders
  where
    f :: (Set C.TxIn, Mockchain C.BabbageEra)
      -> C.BlockHeader
      -> Gen (Set C.TxIn, Mockchain C.BabbageEra)
    f (utxoSet, mockchain) bh = do
        utxosAsTxInput <- nonEmptySubset utxoSet
        txBodyContent <- genTxBody $ Set.toList utxosAsTxInput
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
    :: [C.TxIn] -> Gen (C.TxBodyContent C.BuildTx C.BabbageEra)
genTxBodyContentFromTxIns inputs = do
    txBodyContent <-
        emptyTxBodyContent (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra)
            <$> CGen.genProtocolParameters
    txOuts <- Gen.list (Range.linear 1 5) $ genTxOutTxContext C.BabbageEra
    pure $ txBodyContent
        { C.txIns = fmap (, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) inputs
        , C.txOuts = txOuts
        }

-- | Generates TxBodyContent that may or may not have Collateral
-- This generator is use for phase-2 validation test cases
genTxBodyContentFromTxinsWihtPhase2Validation
    :: [C.TxIn]
    -> Gen (C.TxBodyContent C.BuildTx C.BabbageEra)
genTxBodyContentFromTxinsWihtPhase2Validation inputs = do
    txBodyContent <-
        emptyTxBodyContent (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra)
            <$> CGen.genProtocolParameters
    txOuts <- Gen.list (Range.linear 1 5) $ genTxOutTxContext C.BabbageEra
    txInsCollateral <- genTxInsCollateral C.BabbageEra
    txReturnCollateral <- genTxReturnCollateral C.BabbageEra
    txScriptValidity <- genTxScriptValidity C.BabbageEra
    pure $ txBodyContent
        { C.txIns = fmap (, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) inputs
        , C.txOuts = txOuts
        , C.txInsCollateral = txInsCollateral
        , C.txReturnCollateral = txReturnCollateral
        , C.txScriptValidity = txScriptValidity
        }

-------------------------------------------------------------------------------------
----- The following are whole sale copy/paste from https://github.com/input-output-hk/cardano-node/blob/master/cardano-api/gen/Test/Gen/Cardano/Api/Typed.hs
----- TODO remove when we upgrade to newer version of cardano-api as the generators below are exposed in later version of cardano-api
-------------------------------------------------------------------------------------
genTxInsCollateral :: C.CardanoEra era -> Gen (C.TxInsCollateral era)
genTxInsCollateral era =
    case C.collateralSupportedInEra era of
      Nothing        -> pure C.TxInsCollateralNone
      Just supported -> Gen.choice
                          [ pure C.TxInsCollateralNone
                          , C.TxInsCollateral supported <$> Gen.list (Range.linear 0 10) CGen.genTxIn
                          ]
genTxReturnCollateral :: C.CardanoEra era -> Gen (C.TxReturnCollateral C.CtxTx era)
genTxReturnCollateral era =
  case C.totalAndReturnCollateralSupportedInEra  era of
    Nothing -> return C.TxReturnCollateralNone
    Just supp ->
      C.TxReturnCollateral supp <$>  genTxOutTxContext era

genTxScriptValidity :: C.CardanoEra era -> Gen (C.TxScriptValidity era)
genTxScriptValidity era = case C.txScriptValiditySupportedInCardanoEra era of
  Nothing      -> pure C.TxScriptValidityNone
  Just witness -> C.TxScriptValidity witness <$> genScriptValidity

genScriptValidity :: Gen C.ScriptValidity
genScriptValidity = Gen.element [C.ScriptInvalid, C.ScriptValid]
