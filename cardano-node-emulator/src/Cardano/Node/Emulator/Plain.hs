{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Cardano.Node.Emulator.Plain where

import Control.Lens (makeLenses, (%~), (&), (^.))
import Control.Monad (void)
import Control.Monad.Freer (Eff, Member, interpret, run, type (~>))
import Control.Monad.Freer.Extras (raiseEnd)
import Control.Monad.Freer.Extras.Log (LogMessage (..), LogMsg (..))
import Control.Monad.Freer.State (State, modify, runState)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Sum (..))
import Data.Sequence (Seq)

import Cardano.Api qualified as C
import Cardano.Node.Emulator qualified as E
import Ledger (CardanoAddress, CardanoTx, DatumFromQuery, DatumHash, DecoratedTxOut, OnChainTx (..),
               PaymentPrivateKey (..), TxOut (..), TxOutRef, UtxoIndex, ValidationPhase (Phase2), mkDecoratedTxOut)
import Ledger.AddressMap qualified as AM
import Ledger.Index (UtxoIndex (..), createGenesisTransaction)
import Ledger.Tx (CardanoTx (..), DatumFromQuery (..), addCardanoTxSignature)
import Ledger.Tx.CardanoAPI (CardanoBuildTx, fromCardanoReferenceScript, fromCardanoScriptData)
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx.Builtins qualified as PlutusTx


data PlainEmulator = PlainEmulator
  { _peParams     :: E.Params
  , _peChainState :: E.ChainState
  , _peLog        :: Seq E.ChainEvent
  , _peAddressMap :: AM.AddressMap
  }
  deriving (Show)

makeLenses 'PlainEmulator

emptyPlainEmulator :: E.Params -> PlainEmulator
emptyPlainEmulator params = PlainEmulator params E.emptyChainState mempty mempty

emptyPlainEmulatorWithInitialDist :: E.Params -> Map CardanoAddress C.Value -> PlainEmulator
emptyPlainEmulatorWithInitialDist params initialDist =
  let tx = Valid $ createGenesisTransaction initialDist
  in emptyPlainEmulator params
    & peChainState . E.chainNewestFirst %~ ([tx] :)
    & peAddressMap %~ AM.updateAllAddresses tx

handleChainLogs
  :: ( Member (State AM.AddressMap) effs
     , Member (Writer (Seq E.ChainEvent)) effs
     )
  => LogMsg E.ChainEvent ~> Eff effs
handleChainLogs (LMessage (LogMessage _ e)) = do
  tell @(Seq E.ChainEvent) (pure e)
  case e of
    (E.TxnValidate _ tx _)                  -> void $ modify (AM.updateAllAddresses (Valid tx))
    (E.TxnValidationFail Phase2 _ tx _ _ _) -> void $ modify (AM.updateAllAddresses (Invalid tx))
    _                                       -> pure ()

handleChain :: Eff [E.ChainControlEffect, E.ChainEffect] () -> PlainEmulator -> PlainEmulator
handleChain eff (PlainEmulator params chainState lg am) =
  let (((_, am') , newChainState), lg') = raiseEnd eff
        & interpret (E.handleControlChain params)
        & interpret (E.handleChain params)
        & interpret handleChainLogs
        & runState am
        & runState chainState
        & runWriter
        & run
  in PlainEmulator params newChainState (lg <> lg') am'

queueTx :: CardanoTx -> PlainEmulator -> PlainEmulator
queueTx tx = handleChain (E.queueTx tx)

nextSlot :: PlainEmulator -> PlainEmulator
nextSlot = handleChain $ do
  void E.processBlock
  void $ E.modifySlot succ

utxosAt :: CardanoAddress -> PlainEmulator -> Map TxOutRef DecoratedTxOut
utxosAt addr pe = Map.mapMaybe toDecoratedTxOut $ pe ^. peAddressMap . AM.fundsAt addr
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

submitUnbalancedTx
  :: Foldable f
  => UtxoIndex -- ^ Just the transaction inputs, not the entire 'UTxO'.
  -> CardanoAddress -- ^ Wallet address
  -> CardanoBuildTx
  -> f PaymentPrivateKey -- ^ Signatures
  -> PlainEmulator
  -> PlainEmulator
submitUnbalancedTx utxoIndex changeAddr utx keys pe =
  let
    ownUtxos = UtxoIndex $ snd <$> pe ^. peAddressMap . AM.fundsAt changeAddr
    utxoProvider = E.utxoProviderFromWalletOutputs ownUtxos utx
    newTx = either (error . show) id $ E.makeAutoBalancedTransactionWithUtxoProvider
      (pe ^. peParams)
      utxoIndex
      changeAddr
      utxoProvider
      (error . show)
      utx
    signedTx = foldr (addCardanoTxSignature . unPaymentPrivateKey) (CardanoEmulatorEraTx newTx) keys
  in pe & queueTx signedTx

hasValidatedTransactionCountOfTotal :: Int -> Int -> PlainEmulator -> Maybe String
hasValidatedTransactionCountOfTotal valid total pe =
  let count = \case
        E.TxnValidate{}       -> (Sum 1, Sum 0)
        E.TxnValidationFail{} -> (Sum 0, Sum 1)
        _                     -> mempty
      (Sum validCount, Sum invalidCount) = foldMap count (pe ^. peLog)
  in
    if valid /= validCount then Just $ "Unexpected number of valid transactions: " ++ show validCount
    else if total - valid /= invalidCount then Just $ "Unexpected number of invalid transactions: " ++ show invalidCount
    else Nothing
