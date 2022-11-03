{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

{-| Handlers for the 'ChainIndexQueryEffect' and the 'ChainIndexControlEffect'
    in the emulator
-}
module Plutus.ChainIndex.Emulator.Handlers(
    handleQuery
    , handleControl
    , ChainIndexEmulatorState(..)
    , diskState
    , utxoIndex
    ) where

import Control.Lens (at, ix, makeLenses, over, preview, set, to, view, (&), (^?))
import Control.Monad (foldM)
import Control.Monad.Freer (Eff, Member, type (~>))
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Extras.Log (LogMsg, logDebug, logError, logWarn)
import Control.Monad.Freer.Extras.Pagination (Page (nextPageQuery, pageItems), PageQuery, pageOf)
import Control.Monad.Freer.State (State, get, gets, modify, put)
import Data.List qualified as List
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Ledger.Address (Address (addressCredential))
import Ledger.Scripts (ScriptHash (ScriptHash))
import Ledger.Tx (TxId, TxOutRef (..), Versioned)
import Ledger.Tx qualified as L (DatumFromQuery (..), DecoratedTxOut, datumInDatumFromQuery, decoratedTxOutDatum,
                                 mkPubkeyDecoratedTxOut, mkScriptDecoratedTxOut)
import Plutus.ChainIndex.Api (IsUtxoResponse (IsUtxoResponse), QueryResponse (QueryResponse),
                              TxosResponse (TxosResponse), UtxosResponse (UtxosResponse))
import Plutus.ChainIndex.ChainIndexError (ChainIndexError (..))
import Plutus.ChainIndex.ChainIndexLog (ChainIndexLog (..))
import Plutus.ChainIndex.Effects (ChainIndexControlEffect (..), ChainIndexQueryEffect (..))
import Plutus.ChainIndex.Emulator.DiskState (DiskState, addressMap, assetClassMap, dataMap, redeemerMap, scriptMap,
                                             txMap)
import Plutus.ChainIndex.Emulator.DiskState qualified as DiskState
import Plutus.ChainIndex.Tx (txOuts)
import Plutus.ChainIndex.TxUtxoBalance qualified as TxUtxoBalance
import Plutus.ChainIndex.Types (ChainIndexTx, ChainIndexTxOut (..), ChainSyncBlock (..), Diagnostics (..),
                                Point (PointAtGenesis), Tip (..), TxProcessOption (..), TxUtxoBalance (..),
                                fromReferenceScript)
import Plutus.ChainIndex.UtxoState (InsertUtxoSuccess (..), RollbackResult (..), UtxoIndex, tip, utxoState)
import Plutus.ChainIndex.UtxoState qualified as UtxoState
import Plutus.Script.Utils.Scripts (datumHash)
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential, ScriptCredential), Datum, DatumHash,
                             MintingPolicy (MintingPolicy), MintingPolicyHash (MintingPolicyHash), Script,
                             StakeValidator (StakeValidator), StakeValidatorHash (StakeValidatorHash),
                             Validator (Validator), ValidatorHash (ValidatorHash))
import Plutus.V2.Ledger.Api (OutputDatum (..))

data ChainIndexEmulatorState =
    ChainIndexEmulatorState
        { _diskState :: DiskState
        , _utxoIndex :: UtxoIndex TxUtxoBalance
        }
        deriving stock (Eq, Show, Generic)
        deriving (Semigroup, Monoid) via (GenericSemigroupMonoid ChainIndexEmulatorState)

makeLenses ''ChainIndexEmulatorState

getDatumFromHash ::
    forall effs.
    ( Member (State ChainIndexEmulatorState) effs
    )
    => DatumHash
    -> Eff effs (Maybe Datum)
getDatumFromHash h = gets (view $ diskState . dataMap . at h)

getScriptFromHash ::
    forall effs.
    ( Member (State ChainIndexEmulatorState) effs
    )
    => ScriptHash
    -> Eff effs (Maybe (Versioned Script))
getScriptFromHash h = gets (view $ diskState . scriptMap . at h)

-- | Get the 'ChainIndexTx' for a transaction ID
getTxFromTxId ::
    forall effs.
    (Member (State ChainIndexEmulatorState) effs
    , Member (LogMsg ChainIndexLog) effs
    ) => TxId
    -> Eff effs (Maybe ChainIndexTx)
getTxFromTxId i = do
    result <- gets (view $ diskState . txMap . at i)
    case result of
        Nothing -> logWarn (TxNotFound i) >> pure Nothing
        _       -> pure result


-- | Get the 'ChainIndexTxOut' for a 'TxOutRef'.
getTxOutFromRef ::
  forall effs.
  ( Member (State ChainIndexEmulatorState) effs
  , Member (LogMsg ChainIndexLog) effs
  )
  => TxOutRef
  -> Eff effs (Maybe L.DecoratedTxOut)
getTxOutFromRef ref@TxOutRef{txOutRefId, txOutRefIdx} = do
  ds <- gets (view diskState)
  -- Find the output in the tx matching the output ref
  case preview (txMap . ix txOutRefId . to txOuts . ix (fromIntegral txOutRefIdx)) ds of
    Nothing    -> logWarn (TxOutNotFound ref) >> pure Nothing
    Just txout -> makeChainIndexTxOut txout

-- | Get the 'ChainIndexTxOut' for a 'TxOutRef'.
getUtxoutFromRef ::
  forall effs.
  ( Member (State ChainIndexEmulatorState) effs
  , Member (LogMsg ChainIndexLog) effs
  )
  => TxOutRef
  -> Eff effs (Maybe L.DecoratedTxOut)
getUtxoutFromRef ref@TxOutRef{txOutRefId, txOutRefIdx} = do
  ds <- gets (view diskState)
  -- Find the output in the tx matching the output ref
  case preview (txMap . ix txOutRefId . to txOuts . ix (fromIntegral txOutRefIdx)) ds of
    Nothing    -> logWarn (TxOutNotFound ref) >> pure Nothing
    Just txout -> do makeChainIndexTxOut txout

makeChainIndexTxOut ::
  forall effs.
  ( Member (State ChainIndexEmulatorState) effs
  , Member (LogMsg ChainIndexLog) effs
  )
  => ChainIndexTxOut
  -> Eff effs (Maybe L.DecoratedTxOut)
makeChainIndexTxOut txout@(ChainIndexTxOut address value datum refScript) = do
  datumWithHash <- getDatumWithHash datum
  -- The output might come from a public key address or a script address.
  -- We need to handle them differently.
  case addressCredential $ citoAddress txout of
    PubKeyCredential _ ->
      pure $ L.mkPubkeyDecoratedTxOut address value datumWithHash script
    ScriptCredential (ValidatorHash h) -> do
      case datumWithHash of
        Just d -> do
          v <- getScriptFromHash (ScriptHash h)
          pure $ L.mkScriptDecoratedTxOut address value d script (fmap Validator <$> v)
        Nothing -> do
          -- If the txout comes from a script address, the Datum should not be Nothing
          logWarn $ NoDatumScriptAddr txout
          pure Nothing
 where
    getDatumWithHash :: OutputDatum -> Eff effs (Maybe (DatumHash, L.DatumFromQuery))
    getDatumWithHash NoOutputDatum = pure Nothing
    getDatumWithHash (OutputDatumHash dh) = do
        d <- getDatumFromHash dh
        pure $ Just (dh, maybe L.DatumUnknown L.DatumInBody d)
    getDatumWithHash (OutputDatum d) = do
        pure $ Just (datumHash d, L.DatumInline d)

    script = fromReferenceScript refScript

-- | Unspent outputs located at addresses with the given credential.
getUtxoSetAtAddress ::
  forall effs.
  ( Member (State ChainIndexEmulatorState) effs
  , Member (LogMsg ChainIndexLog) effs
  )
  => PageQuery TxOutRef
  -> Credential
  -> Eff effs UtxosResponse
getUtxoSetAtAddress pageQuery cred = do
  state <- get
  let outRefs = view (diskState . addressMap . at cred) state
      utxo = view (utxoIndex . to utxoState) state
      utxoRefs = Set.filter (flip TxUtxoBalance.isUnspentOutput utxo)
                            (fromMaybe mempty outRefs)
      page = pageOf pageQuery utxoRefs
  case tip utxo of
    TipAtGenesis -> do
      logWarn TipIsGenesis
      pure (UtxosResponse TipAtGenesis (pageOf pageQuery Set.empty))
    tp           -> pure (UtxosResponse tp page)


handleQuery ::
    forall effs.
    ( Member (State ChainIndexEmulatorState) effs
    , Member (Error ChainIndexError) effs
    , Member (LogMsg ChainIndexLog) effs
    ) => ChainIndexQueryEffect
    ~> Eff effs
handleQuery = \case
    DatumFromHash h -> getDatumFromHash h
    ValidatorFromHash (ValidatorHash h) ->  do
      fmap (fmap Validator) <$> getScriptFromHash (ScriptHash h)
    MintingPolicyFromHash (MintingPolicyHash h) ->
      fmap (fmap MintingPolicy) <$> getScriptFromHash (ScriptHash h)
    StakeValidatorFromHash (StakeValidatorHash h) ->
      fmap (fmap StakeValidator) <$> getScriptFromHash (ScriptHash h)
    UnspentTxOutFromRef ref -> getTxOutFromRef ref
    TxOutFromRef ref -> getTxOutFromRef ref
    RedeemerFromHash h -> gets (view $ diskState . redeemerMap . at h)
    TxFromTxId i -> getTxFromTxId i
    UtxoSetMembership r -> do
        utxo <- gets (utxoState . view utxoIndex)
        case tip utxo of
            TipAtGenesis -> throwError QueryFailedNoTip
            tp           -> pure (IsUtxoResponse tp (TxUtxoBalance.isUnspentOutput r utxo))
    UtxoSetAtAddress pageQuery cred -> getUtxoSetAtAddress pageQuery cred
    UnspentTxOutSetAtAddress pageQuery cred -> do
        (UtxosResponse tp page) <- getUtxoSetAtAddress pageQuery cred
        case tp of
          TipAtGenesis -> do
            pure $ QueryResponse [] Nothing
          _            -> do
            mtxouts <- mapM getUtxoutFromRef (pageItems page)
            let txouts = [ (t, o) | (t, mo) <- List.zip (pageItems page) mtxouts, o <- maybeToList mo]
            pure $ QueryResponse txouts (nextPageQuery page)
    DatumsAtAddress pageQuery cred -> do
      state <- get
      let outRefs = view (diskState . addressMap . at cred) state
          txoRefs = fromMaybe mempty outRefs
          utxo = view (utxoIndex . to utxoState) state
          page = pageOf pageQuery txoRefs
          resolveDatum (Just h, Nothing) = gets (view $ diskState . dataMap . at h)
          resolveDatum (_, Just d)       = pure $ Just d
          resolveDatum (_, _)            = pure Nothing
          txOutToDatum txout = fromMaybe (Nothing, Nothing) $ do
              (dh, mdatum) <- txout ^? L.decoratedTxOutDatum
              pure (Just dh, mdatum ^? L.datumInDatumFromQuery)
      txouts <- catMaybes <$> mapM getTxOutFromRef (pageItems page)
      datums <- catMaybes <$> mapM (resolveDatum . txOutToDatum) txouts
      case tip utxo of
        TipAtGenesis -> do
          logWarn TipIsGenesis
          pure $ QueryResponse [] Nothing
        _ -> pure $ QueryResponse datums (nextPageQuery page)
    UtxoSetWithCurrency pageQuery assetClass -> do
        state <- get
        let outRefs = view (diskState . assetClassMap . at assetClass) state
            utxo = view (utxoIndex . to utxoState) state
            utxoRefs = Set.filter (flip TxUtxoBalance.isUnspentOutput utxo) (fromMaybe mempty outRefs)
            page = pageOf pageQuery utxoRefs
        case tip utxo of
            TipAtGenesis -> do
                logWarn TipIsGenesis
                pure (UtxosResponse TipAtGenesis (pageOf pageQuery Set.empty))
            tp           -> pure (UtxosResponse tp page)
    TxsFromTxIds is -> catMaybes <$> mapM getTxFromTxId is
    TxoSetAtAddress pageQuery cred -> do
        state <- get
        let outRefs = view (diskState . addressMap . at cred) state
            txoRefs = fromMaybe mempty outRefs
            utxo = view (utxoIndex . to utxoState) state
            page = pageOf pageQuery txoRefs
        case tip utxo of
            TipAtGenesis -> do
                logWarn TipIsGenesis
                pure $ TxosResponse $ pageOf pageQuery Set.empty
            _            -> pure $ TxosResponse page
    GetTip ->
        gets (tip . utxoState . view utxoIndex)

appendBlocks ::
    forall effs.
    ( Member (State ChainIndexEmulatorState) effs
    , Member (LogMsg ChainIndexLog) effs
    )
    => [ChainSyncBlock] -> Eff effs ()
appendBlocks [] = pure ()
appendBlocks blocks = do
    let
        processBlock (utxoIndexState, txs) (Block tip_ transactions) = do
            if null transactions then return (utxoIndexState, txs)
            else do
                case UtxoState.insert (TxUtxoBalance.fromBlock tip_ (map fst transactions)) utxoIndexState of
                    Left err -> do
                        let reason = InsertionFailed err
                        logError $ Err reason
                        return (utxoIndexState, txs)
                    Right InsertUtxoSuccess{newIndex, insertPosition} -> do
                        logDebug $ InsertionSuccess tip_ insertPosition
                        return (newIndex, transactions ++ txs)
    oldState <- get @ChainIndexEmulatorState
    (newIndex, transactions) <- foldM processBlock (view utxoIndex oldState, []) blocks
    put $ oldState
            & set utxoIndex newIndex
            & over diskState
                (mappend $ foldMap (\(tx, opt) -> if tpoStoreTx opt then DiskState.fromTx tx else mempty) transactions)

handleControl ::
    forall effs.
    ( Member (State ChainIndexEmulatorState) effs
    , Member (Error ChainIndexError) effs
    , Member (LogMsg ChainIndexLog) effs
    )
    => ChainIndexControlEffect
    ~> Eff effs
handleControl = \case
    AppendBlocks blocks -> appendBlocks blocks
    Rollback tip_ -> do
        oldState <- get @ChainIndexEmulatorState
        case TxUtxoBalance.rollback tip_ (view utxoIndex oldState) of
            Left err -> do
                let reason = RollbackFailed err
                logError $ Err reason
                throwError reason
            Right RollbackResult{newTip, rolledBackIndex} -> do
                put $ oldState & set utxoIndex rolledBackIndex
                logDebug $ RollbackSuccess newTip
    ResumeSync PointAtGenesis -> pure ()
    ResumeSync _ ->
        -- The emulator can only resume from genesis.
        throwError ResumeNotSupported
    CollectGarbage -> do
        -- Rebuild the index using only transactions that still have at
        -- least one output in the UTXO set
        utxos <- gets $
            Set.toList
            . Set.map txOutRefId
            . TxUtxoBalance.unspentOutputs
            . UtxoState.utxoState
            . view utxoIndex
        newDiskState <- foldMap DiskState.fromTx . catMaybes <$> mapM getTxFromTxId utxos
        modify $ set diskState newDiskState
    GetDiagnostics -> diagnostics <$> get @ChainIndexEmulatorState

diagnostics :: ChainIndexEmulatorState -> Diagnostics
diagnostics (ChainIndexEmulatorState ds ui) =
    let TxUtxoBalance outputs inputs = UtxoState._usTxUtxoData $ UtxoState.utxoState ui
    in (DiskState.diagnostics ds)
        { numUnspentOutputs  = length outputs
        , numUnmatchedInputs = length inputs
        }
