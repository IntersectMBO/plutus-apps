{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-| Handlers for the 'ChainIndexQueryEffect' and the 'ChainIndexControlEffect'
    in the emulator
-}
module Plutus.ChainIndex.Indexer.Memory.Handlers(
    handleQuery
    , handleDatumFromHashIndexer
    , handleRedeemerFromHashIndexer
    , handleScriptFromHashIndexer
    , handleUtxoFromAddressIndexer
    , handleAssetClassFromAddressIndexer
    , handleUtxoIndexer
    , handleTxFromIdIndexer
    , ChainIndexEmulatorState(..)
    , diskState
    , utxoIndex
    ) where

import Control.Lens (at, ix, makeLenses, over, preview, set, to, view, (&))
import Control.Monad (foldM)
import Control.Monad.Freer (Eff, Member, type (~>))
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Extras.Log (LogMsg, logDebug, logError, logWarn)
import Control.Monad.Freer.Extras.Pagination (pageOf)
import Control.Monad.Freer.State (State, get, gets, put)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Semigroup.Generic (GenericSemigroupMonoid (GenericSemigroupMonoid))
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Ledger (Address (addressCredential), ChainIndexTxOut (PublicKeyChainIndexTxOut, ScriptChainIndexTxOut),
               MintingPolicy (MintingPolicy), MintingPolicyHash (MintingPolicyHash), StakeValidator (StakeValidator),
               StakeValidatorHash (StakeValidatorHash), TxId, TxOut (txOutAddress),
               TxOutRef (TxOutRef, txOutRefId, txOutRefIdx), Validator (Validator), ValidatorHash (ValidatorHash),
               txOutDatumHash, txOutValue)
import Ledger.Scripts (ScriptHash (ScriptHash))
import Plutus.ChainIndex.ChainIndexError (ChainIndexError (InsertionFailed, QueryFailedNoTip, ResumeNotSupported, RollbackFailed))
import Plutus.ChainIndex.ChainIndexLog (ChainIndexLog (Err, InsertionSuccess, NoDatumScriptAddr, RollbackSuccess, TipIsGenesis, TxNotFound, TxOutNotFound))
import Plutus.ChainIndex.Effects (ChainIndexControlEffect (AppendBlocks, CollectGarbage, ResumeSync, Rollback),
                                  ChainIndexQueryEffect (DatumFromHash, GetDiagnostics, GetTip, MintingPolicyFromHash, RedeemerFromHash, StakeValidatorFromHash, TxoSetAtAddress, UnspentTxOutFromRef, UtxoSetAtAddress, UtxoSetMembership, UtxoSetWithCurrency, ValidatorFromHash))
import Plutus.ChainIndex.Http.Api (IsUtxoResponse (IsUtxoResponse), TxosResponse (TxosResponse),
                                   UtxosResponse (UtxosResponse))
import Plutus.ChainIndex.Indexer.Memory.DiskState (DiskState, addressMap, assetClassMap, dataMap, redeemerMap,
                                                   scriptMap, txAssetClassMap, txCredentialMap, txMap)
import Plutus.ChainIndex.Indexer.Memory.DiskState qualified as DiskState
import Plutus.ChainIndex.Tx (ChainIndexTx, _ValidTx, citxData, citxOutputs, citxRedeemers, citxScripts, citxTxId)
import Plutus.ChainIndex.TxUtxoBalance qualified as TxUtxoBalance
import Plutus.ChainIndex.Types (ChainSyncBlock (Block), Diagnostics (numUnmatchedInputs, numUnspentOutputs),
                                Point (PointAtGenesis), Tip (TipAtGenesis), TxProcessOption (TxProcessOption),
                                TxUtxoBalance (TxUtxoBalance))
import Plutus.ChainIndex.UtxoState (InsertUtxoSuccess (InsertUtxoSuccess, insertPosition, newIndex),
                                    RollbackResult (RollbackResult), UtxoIndex, newTip, rolledBackIndex, tip, utxoState)
import Plutus.ChainIndex.UtxoState qualified as UtxoState
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential, ScriptCredential))

data ChainIndexEmulatorState =
    ChainIndexEmulatorState
        { _diskState :: DiskState
        , _utxoIndex :: UtxoIndex TxUtxoBalance
        }
        deriving stock (Eq, Show, Generic)
        deriving (Semigroup, Monoid) via (GenericSemigroupMonoid ChainIndexEmulatorState)

makeLenses ''ChainIndexEmulatorState

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
getUtxoutFromRef ::
  forall effs.
  ( Member (State ChainIndexEmulatorState) effs
  , Member (LogMsg ChainIndexLog) effs
  )
  => TxOutRef
  -> Eff effs (Maybe ChainIndexTxOut)
getUtxoutFromRef ref@TxOutRef{txOutRefId, txOutRefIdx} = do
  ds <- gets (view diskState)
  -- Find the output in the tx matching the output ref
  case preview (txMap . ix txOutRefId . citxOutputs . _ValidTx . ix (fromIntegral txOutRefIdx)) ds of
    Nothing -> logWarn (TxOutNotFound ref) >> pure Nothing
    Just txout -> do
      -- The output might come from a public key address or a script address.
      -- We need to handle them differently.
      case addressCredential $ txOutAddress txout of
        PubKeyCredential _ ->
          pure $ Just $ PublicKeyChainIndexTxOut (txOutAddress txout) (txOutValue txout)
        ScriptCredential vh@(ValidatorHash h) -> do
          case txOutDatumHash txout of
            Nothing -> do
              -- If the txout comes from a script address, the Datum should not be Nothing
              logWarn $ NoDatumScriptAddr txout
              pure Nothing
            Just dh -> do
              let v = maybe (Left vh) (Right . Validator) $ preview (scriptMap . ix (ScriptHash h)) ds
              let d = maybe (Left dh) Right $ preview (dataMap . ix dh) ds
              pure $ Just $ ScriptChainIndexTxOut (txOutAddress txout) v d (txOutValue txout)

diagnostics :: ChainIndexEmulatorState -> Diagnostics
diagnostics (ChainIndexEmulatorState ds ui) =
    let TxUtxoBalance outputs inputs = UtxoState._usTxUtxoData $ UtxoState.utxoState ui
    in (DiskState.diagnostics ds)
        { numUnspentOutputs  = length outputs
        , numUnmatchedInputs = length inputs
        }

handleQuery ::
    forall effs.
    ( Member (State ChainIndexEmulatorState) effs
    , Member (Error ChainIndexError) effs
    , Member (LogMsg ChainIndexLog) effs
    ) => ChainIndexQueryEffect
    ~> Eff effs
handleQuery = \case
    DatumFromHash h -> gets (view $ diskState . dataMap . at h)
    ValidatorFromHash (ValidatorHash h) ->  do
      gets (fmap (fmap Validator) . view $ diskState . scriptMap . at (ScriptHash h))
    MintingPolicyFromHash (MintingPolicyHash h) ->
      gets (fmap (fmap MintingPolicy) . view $ diskState . scriptMap . at (ScriptHash h))
    StakeValidatorFromHash (StakeValidatorHash h) ->
      gets (fmap (fmap StakeValidator) . view $ diskState . scriptMap . at (ScriptHash h))
    UnspentTxOutFromRef ref -> getUtxoutFromRef ref
    RedeemerFromHash h -> gets (view $ diskState . redeemerMap . at h)
    UtxoSetMembership r -> do
        utxo <- gets (utxoState . view utxoIndex)
        case tip utxo of
            TipAtGenesis -> throwError QueryFailedNoTip
            tp           -> pure (IsUtxoResponse tp (TxUtxoBalance.isUnspentOutput r utxo))
    UtxoSetAtAddress pageQuery cred -> do
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
    GetDiagnostics -> diagnostics <$> get @ChainIndexEmulatorState

handleUtxoIndexer ::
    ( Member (State ChainIndexEmulatorState) effs
    , Member (Error ChainIndexError) effs
    , Member (LogMsg ChainIndexLog) effs
    )
    => ChainIndexControlEffect
    ~> Eff effs
handleUtxoIndexer = \case
    AppendBlocks blocks -> do
        let
            processBlock utxoIndexState (Block tip_ transactions) = do
                case UtxoState.insert (TxUtxoBalance.fromBlock tip_ (map fst transactions)) utxoIndexState of
                    Left err -> do
                        let reason = InsertionFailed err
                        logError $ Err reason
                        return utxoIndexState
                    Right InsertUtxoSuccess{newIndex, insertPosition} -> do
                        logDebug $ InsertionSuccess tip_ insertPosition
                        return newIndex
        oldState <- get @ChainIndexEmulatorState
        newIndex <- foldM processBlock (view utxoIndex oldState) blocks
        put $ oldState & set utxoIndex newIndex
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
    CollectGarbage -> pure ()

handleTxFromIdIndexer ::
    ( Member (State ChainIndexEmulatorState) effs
    , Member (Error ChainIndexError) effs
    , Member (LogMsg ChainIndexLog) effs
    )
    => ChainIndexControlEffect
    ~> Eff effs
handleTxFromIdIndexer = \case
    AppendBlocks blocks -> do
        let transactions = foldMap (\(Block _ txs) -> txs) blocks
            txInsertRows (tx, TxProcessOption True) =
                set txMap (Map.singleton (view citxTxId tx) tx) mempty
            txInsertRows (_, TxProcessOption False) =
                mempty
        updateChainIndexDiskState (mappend $ foldMap txInsertRows transactions)
    Rollback _ -> pure ()
    ResumeSync PointAtGenesis -> pure ()
    ResumeSync _ ->
        -- The emulator can only resume from genesis.
        throwError ResumeNotSupported
    CollectGarbage -> do
        -- Rebuild the datum index using only transactions that still have at
        -- least one output in the UTXO set
        utxoTransactions <- getUtxoTransactions
        updateChainIndexDiskState
            (set txMap (foldMap (\tx -> Map.singleton (view citxTxId tx) tx) utxoTransactions))

handleDatumFromHashIndexer ::
    ( Member (State ChainIndexEmulatorState) effs
    , Member (Error ChainIndexError) effs
    , Member (LogMsg ChainIndexLog) effs
    )
    => ChainIndexControlEffect
    ~> Eff effs
handleDatumFromHashIndexer = \case
    AppendBlocks blocks -> do
        let transactions = foldMap (\(Block _ txs) -> txs) blocks
            datumInsertRows (tx, TxProcessOption True) =
                set dataMap (view citxData tx) mempty
            datumInsertRows (_, TxProcessOption False) =
                mempty
        updateChainIndexDiskState (mappend $ foldMap datumInsertRows transactions)
    Rollback _ -> pure ()
    ResumeSync PointAtGenesis -> pure ()
    ResumeSync _ ->
        -- The emulator can only resume from genesis.
        throwError ResumeNotSupported
    CollectGarbage -> do
        -- Rebuild the datum index using only transactions that still have at
        -- least one output in the UTXO set
        utxoTransactions <- getUtxoTransactions
        updateChainIndexDiskState (set dataMap (foldMap (view citxData) utxoTransactions))

handleRedeemerFromHashIndexer ::
    ( Member (State ChainIndexEmulatorState) effs
    , Member (Error ChainIndexError) effs
    , Member (LogMsg ChainIndexLog) effs
    )
    => ChainIndexControlEffect
    ~> Eff effs
handleRedeemerFromHashIndexer = \case
    AppendBlocks blocks -> do
        let transactions = foldMap (\(Block _ txs) -> txs) blocks
            redeemerInsertRows (tx, TxProcessOption True) =
                set redeemerMap (view citxRedeemers tx) mempty
            redeemerInsertRows (_, TxProcessOption False) =
                mempty
        updateChainIndexDiskState (mappend $ foldMap redeemerInsertRows transactions)
    Rollback _ -> pure ()
    ResumeSync PointAtGenesis -> pure ()
    ResumeSync _ ->
        -- The emulator can only resume from genesis.
        throwError ResumeNotSupported
    CollectGarbage -> do
        -- Rebuild the redeemer index using only transactions that still have at
        -- least one output in the UTXO set
        utxoTransactions <- getUtxoTransactions
        updateChainIndexDiskState (set redeemerMap (foldMap (view citxRedeemers) utxoTransactions))

handleScriptFromHashIndexer ::
    ( Member (State ChainIndexEmulatorState) effs
    , Member (Error ChainIndexError) effs
    , Member (LogMsg ChainIndexLog) effs
    )
    => ChainIndexControlEffect
    ~> Eff effs
handleScriptFromHashIndexer = \case
    AppendBlocks blocks -> do
        let transactions = foldMap (\(Block _ txs) -> txs) blocks
            scriptInsertRows (tx, TxProcessOption True) =
                set scriptMap (view citxScripts tx) mempty
            scriptInsertRows (_, TxProcessOption False) =
                mempty
        updateChainIndexDiskState (mappend $ foldMap scriptInsertRows transactions)
    Rollback _ -> pure ()
    ResumeSync PointAtGenesis -> pure ()
    ResumeSync _ ->
        -- The emulator can only resume from genesis.
        throwError ResumeNotSupported
    CollectGarbage -> do
        -- Rebuild the script index using only transactions that still have at
        -- least one output in the UTXO set
        utxoTransactions <- getUtxoTransactions
        updateChainIndexDiskState (set scriptMap (foldMap (view citxScripts) utxoTransactions))

handleUtxoFromAddressIndexer ::
    ( Member (State ChainIndexEmulatorState) effs
    , Member (Error ChainIndexError) effs
    , Member (LogMsg ChainIndexLog) effs
    )
    => ChainIndexControlEffect
    ~> Eff effs
handleUtxoFromAddressIndexer = \case
    AppendBlocks blocks -> do
        let transactions = foldMap (\(Block _ txs) -> txs) blocks
            utxoAddressInsertRows (tx, TxProcessOption True) =
                set addressMap (txCredentialMap tx) mempty
            utxoAddressInsertRows (_, TxProcessOption False) =
                mempty
        updateChainIndexDiskState (mappend $ foldMap utxoAddressInsertRows transactions)
    Rollback _ -> pure ()
    ResumeSync PointAtGenesis -> pure ()
    ResumeSync _ ->
        -- The emulator can only resume from genesis.
        throwError ResumeNotSupported
    CollectGarbage -> do
        -- Rebuild the Address->UTXO index using only transactions that still have at
        -- least one output in the UTXO set
        utxoTransactions <- getUtxoTransactions
        updateChainIndexDiskState (set addressMap (foldMap txCredentialMap utxoTransactions))

handleAssetClassFromAddressIndexer ::
    ( Member (State ChainIndexEmulatorState) effs
    , Member (Error ChainIndexError) effs
    , Member (LogMsg ChainIndexLog) effs
    )
    => ChainIndexControlEffect
    ~> Eff effs
handleAssetClassFromAddressIndexer = \case
    AppendBlocks blocks -> do
        let transactions = foldMap (\(Block _ txs) -> txs) blocks
            assetClassAddressInsertRows (tx, TxProcessOption True) =
                set assetClassMap (txAssetClassMap tx) mempty
            assetClassAddressInsertRows (_, TxProcessOption False) =
                mempty
        updateChainIndexDiskState (mappend $ foldMap assetClassAddressInsertRows transactions)
    Rollback _ -> pure ()
    ResumeSync PointAtGenesis -> pure ()
    ResumeSync _ ->
        -- The emulator can only resume from genesis.
        throwError ResumeNotSupported
    CollectGarbage -> do
        -- Rebuild the Address->AsseClass index using only transactions that still have at
        -- least one output in the UTXO set
        utxoTransactions <- getUtxoTransactions
        updateChainIndexDiskState (set assetClassMap (foldMap txAssetClassMap utxoTransactions))

updateChainIndexDiskState ::
    ( Member (State ChainIndexEmulatorState) effs
    )
    => (DiskState -> DiskState)
    -> Eff effs ()
updateChainIndexDiskState f = do
    oldState <- get @ChainIndexEmulatorState
    put $ oldState & over diskState f

-- Get transactions which are linked to an output in the UTXO set.
getUtxoTransactions ::
    ( Member (State ChainIndexEmulatorState) effs
    , Member (LogMsg ChainIndexLog) effs
    )
    => Eff effs [ChainIndexTx]
getUtxoTransactions =
    catMaybes <$> (getUtxoTxIds >>= mapM getTxFromTxId)
  where
    -- Get transaction ids which are linked to an output in the UTXO set.
    getUtxoTxIds =
        gets $ Set.toList
             . Set.map txOutRefId
             . TxUtxoBalance.unspentOutputs
             . UtxoState.utxoState
             . view utxoIndex
