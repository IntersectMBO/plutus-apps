{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Cardano.Node.Emulator.Plain where

import Control.Lens (makeLenses)
import Control.Monad.Freer (Eff)
import Control.Monad.RWS.Strict (evalRWS, runRWS)
import Data.Map (Map)
import Data.Sequence (Seq)

import Cardano.Api qualified as C
import Cardano.Node.Emulator qualified as E
import Cardano.Node.Emulator.MTL qualified as EMTL
import Control.Monad.Except (runExceptT)
import Ledger (CardanoAddress, CardanoTx, DecoratedTxOut, PaymentPrivateKey (..), TxOutRef, UtxoIndex)
import Ledger.Tx.CardanoAPI (CardanoBuildTx)


data PlainEmulator = PlainEmulator
  { _peParams        :: E.Params
  , _peEmulatorState :: EMTL.EmulatorState
  , _peLog           :: Seq E.ChainEvent
  }
  deriving (Show)

makeLenses 'PlainEmulator

emptyPlainEmulator :: E.Params -> PlainEmulator
emptyPlainEmulator params = PlainEmulator params EMTL.emptyEmulatorState mempty

emptyPlainEmulatorWithInitialDist :: E.Params -> Map CardanoAddress C.Value -> PlainEmulator
emptyPlainEmulatorWithInitialDist params initialDist =
  PlainEmulator params (EMTL.emptyEmulatorStateWithInitialDist initialDist) mempty

evalEmulatorM :: EMTL.EmulatorM a -> PlainEmulator -> a
evalEmulatorM m (PlainEmulator params es _) = either (error . show) id $ fst $ evalRWS (runExceptT m) params es

execEmulatorM :: EMTL.EmulatorM a -> PlainEmulator -> PlainEmulator
execEmulatorM m (PlainEmulator params es lg) = case runRWS (runExceptT m) params es of
  (err, es', lg') -> either (error . show) (\_ -> PlainEmulator params es' (lg <> lg')) err

handleChain :: Eff [E.ChainControlEffect, E.ChainEffect] () -> PlainEmulator -> PlainEmulator
handleChain = execEmulatorM . EMTL.handleChain

queueTx :: CardanoTx -> PlainEmulator -> PlainEmulator
queueTx = execEmulatorM . EMTL.queueTx

nextSlot :: PlainEmulator -> PlainEmulator
nextSlot = execEmulatorM EMTL.nextSlot

utxosAt :: CardanoAddress -> PlainEmulator -> Map TxOutRef DecoratedTxOut
utxosAt = evalEmulatorM . EMTL.utxosAt

balanceTx
  :: UtxoIndex -- ^ Just the transaction inputs, not the entire 'UTxO'.
  -> CardanoAddress -- ^ Wallet address
  -> CardanoBuildTx
  -> PlainEmulator
  -> PlainEmulator
balanceTx utxoIndex changeAddr utx =
  execEmulatorM $ EMTL.balanceTx utxoIndex changeAddr utx

submitUnbalancedTx
  :: Foldable f
  => UtxoIndex -- ^ Just the transaction inputs, not the entire 'UTxO'.
  -> CardanoAddress -- ^ Wallet address
  -> CardanoBuildTx
  -> f PaymentPrivateKey -- ^ Signatures
  -> PlainEmulator
  -> PlainEmulator
submitUnbalancedTx utxoIndex changeAddr utx keys =
  execEmulatorM $ EMTL.submitUnbalancedTx utxoIndex changeAddr utx keys
