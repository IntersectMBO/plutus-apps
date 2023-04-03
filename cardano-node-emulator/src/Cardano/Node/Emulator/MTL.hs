{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
-- | If you want to run the node emulator without using the `Contract` monad, this module provides a simple MTL-based interface.
module Cardano.Node.Emulator.MTL (
  -- * Updating the blockchain
    queueTx
  , nextSlot
  , currentSlot
  , awaitSlot
  -- * Querying the blockchain
  , utxosAt
  , fundsAt
  -- * Transactions
  , balanceTx
  , submitUnbalancedTx
  , payToAddress
  -- * Types
  , EmulatorState(EmulatorState)
  , esChainState
  , esAddressMap
  , EmulatorError(..)
  , MonadEmulator
  , EmulatorT
  , EmulatorM
  , emptyEmulatorState
  , emptyEmulatorStateWithInitialDist
  , getParams
  -- * Running Eff chain effects in MTL
  , handleChain
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Node.Emulator qualified as E
import Control.Lens (alaf, makeLenses, view, (%~), (&), (^.))
import Control.Monad (void)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Freer (Eff, Member, interpret, run, type (~>))
import Control.Monad.Freer.Extras (raiseEnd)
import Control.Monad.Freer.Extras.Log (LogMessage (..), LogMsg (..))
import Control.Monad.Freer.State (State, modify, runState)
import Control.Monad.Freer.Writer qualified as F (Writer, runWriter, tell)
import Control.Monad.Identity (Identity)
import Control.Monad.RWS.Class (MonadRWS, ask, get, put, tell)
import Control.Monad.RWS.Strict (RWST)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Endo (..))
import Data.Sequence (Seq)
import Ledger (CardanoAddress, CardanoTx, DatumFromQuery, DatumHash, DecoratedTxOut, OnChainTx (..),
               PaymentPrivateKey (..), Slot, ToCardanoError, TxOut (..), TxOutRef, UtxoIndex, ValidationErrorInPhase,
               mkDecoratedTxOut)
import Ledger.AddressMap qualified as AM
import Ledger.Index (UtxoIndex (..), createGenesisTransaction, insertBlock)
import Ledger.Tx (CardanoTx (..), DatumFromQuery (..), addCardanoTxSignature, decoratedTxOutValue, getCardanoTxId)
import Ledger.Tx.CardanoAPI (CardanoBuildTx (..), fromCardanoReferenceScript, fromCardanoScriptData,
                             toCardanoTxOutValue)
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx.Builtins qualified as PlutusTx


data EmulatorState = EmulatorState
  { _esChainState :: E.ChainState
  , _esAddressMap :: AM.AddressMap
  }
  deriving (Show)

makeLenses 'EmulatorState

data EmulatorError
  = BalancingError E.BalancingError
  | ValidationError ValidationErrorInPhase
  | ToCardanoError ToCardanoError
  deriving (Show)

type MonadEmulator m = (MonadRWS E.Params (Seq E.ChainEvent) EmulatorState m, MonadError EmulatorError m)
type EmulatorT m = ExceptT EmulatorError (RWST E.Params (Seq E.ChainEvent) EmulatorState m)
type EmulatorM = EmulatorT Identity

emptyEmulatorState :: EmulatorState
emptyEmulatorState = EmulatorState E.emptyChainState mempty

emptyEmulatorStateWithInitialDist :: Map CardanoAddress C.Value -> EmulatorState
emptyEmulatorStateWithInitialDist initialDist =
  let tx = Valid $ createGenesisTransaction initialDist
  in emptyEmulatorState
    & esChainState . E.chainNewestFirst %~ ([tx] :)
    & esChainState . E.index %~ insertBlock [tx]
    & esAddressMap %~ AM.updateAllAddresses tx

getParams :: MonadEmulator m => m E.Params
getParams = ask

handleChain :: MonadEmulator m => Eff [E.ChainControlEffect, E.ChainEffect] a -> m a
handleChain eff = do
  params <- ask
  EmulatorState chainState am <- get
  let (((a, am') , newChainState), lg) = raiseEnd eff
        & interpret (E.handleControlChain params)
        & interpret (E.handleChain params)
        & interpret handleChainLogs
        & runState am
        & runState chainState
        & F.runWriter
        & run
  tell lg
  put $ EmulatorState newChainState am'
  pure a
  where
    handleChainLogs
      :: ( Member (State AM.AddressMap) effs
        , Member (F.Writer (Seq E.ChainEvent)) effs
        )
      => LogMsg E.ChainEvent ~> Eff effs
    handleChainLogs (LMessage (LogMessage _ e)) = do
      F.tell @(Seq E.ChainEvent) (pure e)
      void $ modify $ alaf Endo foldMap AM.updateAllAddresses $ E.chainEventOnChainTx e

-- | Queue the transaction, it will be processed when @nextSlot@ is called.
queueTx :: MonadEmulator m => CardanoTx -> m ()
queueTx tx = handleChain (E.queueTx tx)

-- | Process the queued transactions and increase the slot number.
nextSlot :: MonadEmulator m => m ()
nextSlot = handleChain $ do
  void E.processBlock
  void $ E.modifySlot succ

-- | Get the current slot number of the emulated node.
currentSlot :: MonadEmulator m => m Slot
currentSlot = handleChain E.getCurrentSlot

-- | Call `nextSlot` until the current slot number equals or exceeds the given slot number.
awaitSlot :: MonadEmulator m => Slot -> m ()
awaitSlot s = do
  c <- currentSlot
  if s <= c then pure ()
  else do
    nextSlot
    awaitSlot s


-- | Query the unspent transaction outputs at the given address.
utxosAt :: MonadEmulator m => CardanoAddress -> m (Map TxOutRef DecoratedTxOut)
utxosAt addr = do
  es <- get
  pure $ Map.mapMaybe toDecoratedTxOut $ es ^. esAddressMap . AM.fundsAt addr
  where
    toDecoratedTxOut :: (CardanoTx, TxOut) -> Maybe DecoratedTxOut
    toDecoratedTxOut (_, TxOut (C.TxOut addr' val dt rs)) =
      mkDecoratedTxOut addr' (C.txOutValueToValue val) (toDecoratedDatum dt) (fromCardanoReferenceScript rs)
    toDecoratedDatum :: C.TxOutDatum C.CtxTx C.BabbageEra -> Maybe (DatumHash, DatumFromQuery)
    toDecoratedDatum C.TxOutDatumNone       =
      Nothing
    toDecoratedDatum (C.TxOutDatumHash _ h) =
      Just (PV2.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h), DatumUnknown)
    toDecoratedDatum (C.TxOutDatumInTx _ d) =
      Just (PV2.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptData d)), DatumInBody $ PV2.Datum $ fromCardanoScriptData d)
    toDecoratedDatum (C.TxOutDatumInline _ d) =
      Just (PV2.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptData d)), DatumInline $ PV2.Datum $ fromCardanoScriptData d)

-- | Query the total value of the unspent transaction outputs at the given address.
fundsAt :: MonadEmulator m => CardanoAddress -> m C.Value
fundsAt addr = foldMap (view decoratedTxOutValue) <$> utxosAt addr


-- | Balance an unbalanced transaction, using funds from the given wallet if needed, and returning any remaining value to the same wallet.
balanceTx
  :: MonadEmulator m
  => UtxoIndex -- ^ Just the transaction inputs, not the entire 'UTxO'.
  -> CardanoAddress -- ^ Wallet address
  -> CardanoBuildTx
  -> m CardanoTx
balanceTx utxoIndex changeAddr utx = do
  params <- ask
  es <- get
  let
    ownUtxos = UtxoIndex $ snd <$> es ^. esAddressMap . AM.fundsAt changeAddr
    utxoProvider = E.utxoProviderFromWalletOutputs ownUtxos utx
  CardanoEmulatorEraTx <$> E.makeAutoBalancedTransactionWithUtxoProvider
      params
      utxoIndex
      changeAddr
      (either (throwError . BalancingError) pure . utxoProvider)
      (throwError . either ValidationError ToCardanoError)
      utx

-- | Balance a transaction, sign it with the given signatures, and finally queue it.
submitUnbalancedTx
  :: (MonadEmulator m, Foldable f)
  => UtxoIndex -- ^ Just the transaction inputs, not the entire 'UTxO'.
  -> CardanoAddress -- ^ Wallet address
  -> CardanoBuildTx
  -> f PaymentPrivateKey -- ^ Signatures
  -> m CardanoTx
submitUnbalancedTx utxoIndex changeAddr utx keys = do
  newTx <- balanceTx utxoIndex changeAddr utx
  let signedTx = foldr (addCardanoTxSignature . unPaymentPrivateKey) newTx keys
  queueTx signedTx
  pure signedTx

-- | Create a transaction that transfers funds from one address to another, and sign and submit it.
payToAddress :: MonadEmulator m => (CardanoAddress, PaymentPrivateKey) -> CardanoAddress -> C.Value -> m PV2.TxId
payToAddress (sourceAddr, sourcePrivKey) targetAddr value = do
  let buildTx = CardanoBuildTx $ E.emptyTxBodyContent
           { C.txOuts = [C.TxOut targetAddr (toCardanoTxOutValue value) C.TxOutDatumNone C.ReferenceScriptNone]
           }
  getCardanoTxId <$> submitUnbalancedTx mempty sourceAddr buildTx [sourcePrivKey]
