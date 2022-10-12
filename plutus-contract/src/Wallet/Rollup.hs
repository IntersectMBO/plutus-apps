{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Wallet.Rollup
    ( doAnnotateBlockchain
    , initialRollup
    , annotateBlockchain
    , Rollup
    -- * Chain event fold
    , initialState
    , handleChainEvent
    , getAnnotatedTransactions
    ) where

import Cardano.Api qualified as C
import Control.Lens (assign, ifoldr, over, set, use, view, (&), (^.))
import Control.Lens.Combinators (itraverse)
import Control.Monad.State (StateT, evalStateT, runState)
import Data.List (groupBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Ledger (Block, Blockchain, OnChainTx (..), TxIn (TxIn), TxOut, ValidationPhase (..), Value, consumableInputs,
               eitherTx, txInRef, txOutRefId, txOutRefIdx, txOutValue)
import Ledger.Tx qualified as Tx
import Wallet.Emulator.Chain (ChainEvent (..))
import Wallet.Rollup.Types

------------------------------------------------------------
txInputKey :: TxIn -> TxKey
txInputKey TxIn {txInRef} =
    TxKey
        { _txKeyTxId = txOutRefId txInRef
        , _txKeyTxOutRefIdx = txOutRefIdx txInRef
        }

annotateTransaction ::
       Monad m => SequenceId -> OnChainTx -> StateT Rollup m AnnotatedTx
annotateTransaction sequenceId tx = do
    cPreviousOutputs <- use previousOutputs
    cRollingBalances <- use rollingBalances
    dereferencedInputs <-
        traverse
            (\txIn ->
                 let key = txInputKey txIn
                  in case Map.lookup key cPreviousOutputs of
                         Just txOut -> pure $ DereferencedInput txIn txOut
                         Nothing    -> pure $ InputNotFound key)
            (consumableInputs tx)
    let txId = eitherTx Tx.getCardanoTxId Tx.getCardanoTxId tx
        txOuts = eitherTx (const []) Tx.getCardanoTxOutputs tx
        newOutputs =
            ifoldr
                (\outputIndex ->
                     Map.insert
                         TxKey
                             { _txKeyTxId = txId
                             , _txKeyTxOutRefIdx = fromIntegral outputIndex
                             })
                cPreviousOutputs
                txOuts
        newBalances =
            foldr
                sumAccounts
                cRollingBalances
                ((over Tx.outValue' negateValue . refersTo <$> filter isFound dereferencedInputs) <>
                 txOuts)
          where
            negateValue :: C.TxOutValue era -> C.TxOutValue era
            negateValue  (C.TxOutAdaOnly wit l) = C.TxOutAdaOnly wit (negate l)
            negateValue  (C.TxOutValue wit v)   = C.TxOutValue wit (C.negateValue v)
        sumAccounts ::
               TxOut -> Map BeneficialOwner Value -> Map BeneficialOwner Value
        sumAccounts txOut =
            Map.alter sumBalances (toBeneficialOwner txOut)
          where
            sumBalances :: Maybe Value -> Maybe Value
            sumBalances Nothing         = Just (txOutValue txOut)
            sumBalances (Just oldValue) = Just (oldValue <> txOutValue txOut)
    assign previousOutputs newOutputs
    assign rollingBalances newBalances
    pure $
        AnnotatedTx
            { sequenceId
            , txId
            , tx = eitherTx id id tx
            , dereferencedInputs
            , balances = newBalances
            , valid = eitherTx (const False) (const True) tx
            }

annotateChainSlot :: Monad m => Int -> Block -> StateT Rollup m [AnnotatedTx]
annotateChainSlot slotIndex =
    itraverse (\txIndex -> annotateTransaction SequenceId {..})

annotateBlockchain :: Monad m => Blockchain -> StateT Rollup m [[AnnotatedTx]]
annotateBlockchain = fmap reverse . itraverse annotateChainSlot . reverse

initialRollup :: Rollup
initialRollup =
    Rollup {_previousOutputs = Map.empty, _rollingBalances = Map.empty}

initialState :: RollupState
initialState =
    RollupState { _rollup = initialRollup, _annotatedTransactions = [], _currentSequenceId = SequenceId 0 0 }

doAnnotateBlockchain :: Monad m => Blockchain -> m [[AnnotatedTx]]
doAnnotateBlockchain blockchain =
    evalStateT (annotateBlockchain blockchain) initialRollup

getAnnotatedTransactions :: RollupState -> [[AnnotatedTx]]
getAnnotatedTransactions = groupBy (equating (slotIndex . sequenceId)) . reverse . view annotatedTransactions

handleChainEvent :: RollupState -> ChainEvent -> RollupState
handleChainEvent s = \case
    SlotAdd _                         -> s & over currentSequenceId (set txIndexL 0 . over slotIndexL succ)
    TxnValidate _ tx                  -> addTx s (Valid tx)
    TxnValidationFail Phase2 _ tx _ _ -> addTx s (Invalid tx)
    _                                 -> s

addTx :: RollupState -> OnChainTx -> RollupState
addTx s tx =
    let (tx', newState) = runState (annotateTransaction (s ^. currentSequenceId) tx) (s ^. rollup)
    in s & over currentSequenceId (over txIndexL succ)
         & over annotatedTransactions ((:) tx')
         & set rollup newState

-- https://hackage.haskell.org/package/Cabal-3.2.1.0/docs/src/Distribution.Utils.Generic.html#equating
equating :: Eq a => (b -> a) -> b -> b -> Bool
equating p x y = p x == p y
