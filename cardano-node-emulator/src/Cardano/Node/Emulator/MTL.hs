{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Cardano.Node.Emulator.MTL where

import Control.Lens (makeLenses, (%~), (&), (^.))
import Control.Monad (void)
import Control.Monad.Freer (Eff, Member, interpret, run, type (~>))
import Control.Monad.Freer.Extras (raiseEnd)
import Control.Monad.Freer.Extras.Log (LogMessage (..), LogMsg (..))
import Control.Monad.Freer.State (State, modify, runState)
import Control.Monad.Freer.Writer qualified as F (Writer, runWriter, tell)
import Control.Monad.Identity (Identity)
import Control.Monad.RWS.Class (MonadRWS, ask, get, put, tell)
import Control.Monad.RWS.Strict (RWST)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Sum (..))
import Data.Sequence (Seq)
import Data.Text qualified as Text
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.Text qualified as Pretty

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


data EmulatorState = EmulatorState
  { _esChainState :: !E.ChainState
  , _esAddressMap :: !AM.AddressMap
  }
  deriving (Show)

makeLenses 'EmulatorState

type EmulatorT = RWST E.Params (Seq E.ChainEvent) EmulatorState
type EmulatorM = EmulatorT Identity
type MonadEmulator = MonadRWS E.Params (Seq E.ChainEvent) EmulatorState

emptyEmulatorState :: EmulatorState
emptyEmulatorState = EmulatorState E.emptyChainState mempty

emptyEmulatorStateWithInitialDist :: Map CardanoAddress C.Value -> EmulatorState
emptyEmulatorStateWithInitialDist initialDist =
  let tx = Valid $ createGenesisTransaction initialDist
  in emptyEmulatorState
    & esChainState . E.chainNewestFirst %~ ([tx] :)
    & esAddressMap %~ AM.updateAllAddresses tx

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
      case e of
        (E.TxnValidate _ tx _)                  -> void $ modify (AM.updateAllAddresses (Valid tx))
        (E.TxnValidationFail Phase2 _ tx _ _ _) -> void $ modify (AM.updateAllAddresses (Invalid tx))
        _                                       -> pure ()

queueTx :: MonadEmulator m => CardanoTx -> m ()
queueTx tx = handleChain (E.queueTx tx)

nextSlot :: MonadEmulator m => m ()
nextSlot = handleChain $ do
  void E.processBlock
  void $ E.modifySlot succ

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

submitUnbalancedTx
  :: (MonadEmulator m, Foldable f)
  => UtxoIndex -- ^ Just the transaction inputs, not the entire 'UTxO'.
  -> CardanoAddress -- ^ Wallet address
  -> CardanoBuildTx
  -> f PaymentPrivateKey -- ^ Signatures
  -> m ()
submitUnbalancedTx utxoIndex changeAddr utx keys = do
  params <- ask
  es <- get
  let
    ownUtxos = UtxoIndex $ snd <$> es ^. esAddressMap . AM.fundsAt changeAddr
    utxoProvider = E.utxoProviderFromWalletOutputs ownUtxos utx
    newTx = either (error . show) id $ E.makeAutoBalancedTransactionWithUtxoProvider
      params
      utxoIndex
      changeAddr
      utxoProvider
      (error . show)
      utx
    signedTx = foldr (addCardanoTxSignature . unPaymentPrivateKey) (CardanoEmulatorEraTx newTx) keys
  queueTx signedTx

hasValidatedTransactionCountOfTotal :: Int -> Int -> Seq E.ChainEvent -> Maybe String
hasValidatedTransactionCountOfTotal valid total lg =
  let count = \case
        E.TxnValidate{}       -> (Sum 1, Sum 0)
        E.TxnValidationFail{} -> (Sum 0, Sum 1)
        _                     -> mempty
      (Sum validCount, Sum invalidCount) = foldMap count lg
  in
    if valid /= validCount then Just $ "Unexpected number of valid transactions: " ++ show validCount
    else if total - valid /= invalidCount then Just $ "Unexpected number of invalid transactions: " ++ show invalidCount
    else Nothing

renderLogs :: Seq E.ChainEvent -> String
renderLogs = Text.unpack . Pretty.renderStrict . Pretty.layoutPretty Pretty.defaultLayoutOptions . Pretty.vsep . toList . fmap Pretty.pretty
