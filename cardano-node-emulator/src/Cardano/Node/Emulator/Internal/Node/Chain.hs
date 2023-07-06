{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Cardano.Node.Emulator.Internal.Node.Chain where

import Cardano.Node.Emulator.Internal.Node.Params (Params)
import Cardano.Node.Emulator.Internal.Node.Validation qualified as Validation
import Control.Lens (makeLenses, makePrisms, over, view, (%~), (&), (.~))
import Control.Monad.Freer (Eff, Member, Members, send, type (~>))
import Control.Monad.Freer.Extras.Log (LogMsg, logDebug, logInfo, logWarn)
import Control.Monad.Freer.State (State, gets, modify)
import Control.Monad.State qualified as S
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (traverse_)
import Data.List ((\\))
import Data.Maybe (mapMaybe)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Ledger (Block, Blockchain, CardanoTx, OnChainTx, Slot (Slot), getCardanoTxId, getCardanoTxValidityRange,
               unOnChain)
import Ledger.Index qualified as Index
import Ledger.Interval qualified as Interval
import Prettyprinter (Pretty (pretty), vsep, (<+>))

-- | Events produced by the blockchain emulator.
data ChainEvent
    = TxnValidation !Index.ValidationResult
    -- ^ A transaction has been validated and added to the blockchain.
    | SlotAdd !Slot
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty ChainEvent where
    pretty = \case
        TxnValidation res -> vsep ["TxnValidation" <+> pretty (getCardanoTxId $ Index.cardanoTxFromValidationResult res), pretty res]
        SlotAdd sl        -> "SlotAdd" <+> pretty sl

chainEventOnChainTx :: ChainEvent -> Maybe OnChainTx
chainEventOnChainTx (TxnValidation result) = Index.toOnChain result
chainEventOnChainTx _                      = Nothing

-- | A pool of transactions which have yet to be validated.
type TxPool = [CardanoTx]

data ChainState = ChainState {
    _chainNewestFirst :: !Blockchain, -- ^ The current chain, with the newest transactions first in the list.
    _txPool           :: !TxPool, -- ^ The pool of pending transactions.
    _index            :: !Index.UtxoIndex, -- ^ The UTxO index, used for validation.
    _chainCurrentSlot :: !Slot -- ^ The current slot number
} deriving (Show, Generic)

makeLenses ''ChainState

emptyChainState :: ChainState
emptyChainState = ChainState [] [] mempty 0

fromBlockchain :: Blockchain -> ChainState
fromBlockchain bc = emptyChainState
    & chainNewestFirst .~ bc
    & index .~ Index.initialise bc

data ChainControlEffect r where
    ProcessBlock :: ChainControlEffect Block
    ModifySlot :: (Slot -> Slot) -> ChainControlEffect Slot

data ChainEffect r where
    QueueTx :: CardanoTx -> ChainEffect ()
    GetCurrentSlot :: ChainEffect Slot
    GetParams :: ChainEffect Params

-- | Make a new block
processBlock :: Member ChainControlEffect effs => Eff effs Block
processBlock = send ProcessBlock

-- | Adjust the current slot number, returning the new slot.
modifySlot :: Member ChainControlEffect effs => (Slot -> Slot) -> Eff effs Slot
modifySlot = send . ModifySlot

queueTx :: Member ChainEffect effs => CardanoTx -> Eff effs ()
queueTx tx = send (QueueTx tx)

getParams :: Member ChainEffect effs => Eff effs Params
getParams = send GetParams

getCurrentSlot :: Member ChainEffect effs => Eff effs Slot
getCurrentSlot = send GetCurrentSlot

type ChainEffs = '[State ChainState, LogMsg ChainEvent]

handleControlChain :: Members ChainEffs effs => Params -> ChainControlEffect ~> Eff effs
handleControlChain params = \case
    ProcessBlock -> do
        pool  <- gets $ view txPool
        slot  <- gets $ view chainCurrentSlot
        idx   <- gets $ view index

        let ValidatedBlock block events idx' =
                validateBlock params slot idx pool

        modify $ txPool .~ []
        modify $ index .~ idx'
        modify $ addBlock block

        traverse_ logEvent events
        pure block

    ModifySlot f -> modify @ChainState (over chainCurrentSlot f) >> gets (view chainCurrentSlot)

logEvent :: Member (LogMsg ChainEvent) effs => ChainEvent -> Eff effs ()
logEvent e = case e of
    SlotAdd{}                        -> logDebug e
    TxnValidation Index.FailPhase1{} -> logWarn e
    TxnValidation Index.FailPhase2{} -> logWarn e
    TxnValidation Index.Success{}    -> logInfo e

handleChain :: (Members ChainEffs effs) => Params -> ChainEffect ~> Eff effs
handleChain params = \case
    QueueTx tx     -> modify $ over txPool (addTxToPool tx)
    GetCurrentSlot -> gets _chainCurrentSlot
    GetParams      -> pure params

-- | The result of validating a block.
data ValidatedBlock = ValidatedBlock
    { vlbValid  :: !Block
    -- ^ The transactions that have been validated in this block.
    , vlbEvents :: ![ChainEvent]
    -- ^ Transaction validation events for the transactions in this block.
    , vlbIndex  :: !Index.UtxoIndex
    -- ^ The updated UTxO index after processing the block
    }

data ValidationCtx = ValidationCtx { vctxIndex :: !Index.UtxoIndex, vctxParams :: !Params }

-- | Validate a block given the current slot and UTxO index, returning the valid
--   transactions, success/failure events and the updated UTxO set.
validateBlock :: Params -> Slot -> Index.UtxoIndex -> TxPool -> ValidatedBlock
validateBlock params slot@(Slot s) idx txns =
    let
        -- Validate transactions, updating the UTXO index each time
        (results, ValidationCtx idx' _) =
            flip S.runState (ValidationCtx idx params) $ for txns $ validateEm slot

        -- The new block contains all transaction that were validated
        -- successfully
        block = mapMaybe Index.toOnChain results

        -- Also return an `EmulatorEvent` for each transaction that was
        -- processed
        nextSlot = Slot (s + 1)
        events   = (TxnValidation <$> results) ++ [SlotAdd nextSlot]
    in ValidatedBlock block events idx'

-- | Check whether the given transaction can be validated in the given slot.
canValidateNow :: Slot -> CardanoTx -> Bool
canValidateNow slot = Interval.member slot . getCardanoTxValidityRange

-- | Validate a transaction in the current emulator state.
validateEm
    :: S.MonadState ValidationCtx m
    => Slot
    -> CardanoTx
    -> m Index.ValidationResult
validateEm h txn = do
    ctx@(ValidationCtx idx params) <- S.get
    let
        res = Validation.validateCardanoTx params h idx txn
        idx' = case res of
            Index.FailPhase1{} -> idx
            Index.FailPhase2{} -> Index.insertCollateral txn idx
            Index.Success{}    -> Index.insert txn idx
    _ <- S.put ctx{ vctxIndex = idx' }
    pure res

-- | Adds a block to ChainState, without validation.
addBlock :: Block -> ChainState -> ChainState
addBlock blk st =
  st & chainNewestFirst %~ (blk :)
     -- The block update may contain txs that are not in this client's
     -- `txPool` which will get ignored
     & txPool %~ (\\ map unOnChain blk)

addTxToPool :: CardanoTx -> TxPool -> TxPool
addTxToPool = (:)

makePrisms ''ChainEvent
