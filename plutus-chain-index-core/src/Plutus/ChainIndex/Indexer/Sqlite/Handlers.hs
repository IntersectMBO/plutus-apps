{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-| Handlers for the 'ChainIndexQueryEffect' and the 'ChainIndexControlEffect' -}
module Plutus.ChainIndex.Indexer.Sqlite.Handlers
    ( handleQuery
    , handleDatumFromHashIndexer
    , handleRedeemerFromHashIndexer
    , handleScriptFromHashIndexer
    , handleUtxoFromAddressIndexer
    , handleAssetClassFromAddressIndexer
    , handleUtxoIndexer
    , restoreStateFromDb
    , getResumePoints
    , ChainIndexState
    ) where

import Cardano.Api qualified as C
import Control.Applicative (Const (..))
import Control.Monad (foldM)
import Control.Monad.Freer (Eff, Member, type (~>))
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Extras.Beam (BeamEffect (..), BeamableSqlite, combined, selectList, selectOne, selectPage)
import Control.Monad.Freer.Extras.Log (LogMsg, logDebug, logError, logWarn)
import Control.Monad.Freer.Extras.Pagination (Page (Page), PageQuery (..))
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.Freer.State (State, get, gets, put)
import Data.ByteString (ByteString)
import Data.FingerTree qualified as FT
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Word (Word64)
import Database.Beam (Columnar, Database, DatabaseEntity, DatabaseSettings, Identity, SqlSelect, TableEntity,
                      aggregate_, all_, countAll_, delete, filter_, limit_, not_, nub_, select, val_)
import Database.Beam.Backend.SQL (BeamSqlBackendCanSerialize)
import Database.Beam.Query (HasSqlEqualityCheck, asc_, desc_, exists_, orderBy_, update, (&&.), (<-.), (<.), (==.),
                            (>.))
import Database.Beam.Schema.Tables (zipTables)
import Database.Beam.Sqlite (Sqlite)
import Ledger (Address (..), ChainIndexTxOut (..), Datum, DatumHash (..), TxOut (..), TxOutRef (..))
import Ledger.Value (AssetClass (AssetClass), flattenValue)
import Plutus.ChainIndex.ChainIndexError (ChainIndexError (..))
import Plutus.ChainIndex.ChainIndexLog (ChainIndexLog (..))
import Plutus.ChainIndex.Compatibility (toCardanoPoint)
import Plutus.ChainIndex.Effects (ChainIndexControlEffect (..), ChainIndexQueryEffect (..))
import Plutus.ChainIndex.Http.Api (IsUtxoResponse (IsUtxoResponse), TxosResponse (TxosResponse),
                                   UtxosResponse (UtxosResponse))
import Plutus.ChainIndex.Indexer.Sqlite.DbSchema (AddressRowT (_addressRowCred, _addressRowOutRef),
                                                  AssetClassRowT (_assetClassRowAssetClass, _assetClassRowOutRef),
                                                  ChainIndexDb (addressRows, assetClassRows, datumRows, redeemerRows, scriptRows, tipRows, unmatchedInputRows, unspentOutputRows, utxoOutRefRows),
                                                  DatumRowT (_datumRowDatum, _datumRowHash), HasDbType (..),
                                                  PrimaryKey (TipRowId, unTipRowId),
                                                  RedeemerRowT (_redeemerRowHash, _redeemerRowRedeemer),
                                                  ScriptRowT (_scriptRowHash, _scriptRowScript), TipRow,
                                                  TipRowT (TipRow, _tipRowSlot), UnmatchedInputRow,
                                                  UnmatchedInputRowT (UnmatchedInputRow, _unmatchedInputRowOutRef, _unmatchedInputRowTip),
                                                  UnspentOutputRow,
                                                  UnspentOutputRowT (UnspentOutputRow, _unspentOutputRowOutRef, _unspentOutputRowTip),
                                                  UtxoRowT (UtxoRow, _utxoRowOutRef, _utxoRowTxOut))
import Plutus.ChainIndex.Tx (ChainIndexTx (_citxData, _citxRedeemers, _citxScripts), txOutsWithRef)
import Plutus.ChainIndex.TxUtxoBalance qualified as TxUtxoBalance
import Plutus.ChainIndex.Types (ChainSyncBlock (..), Depth (..), Diagnostics (..), Point (..), Tip (..),
                                TxProcessOption (TxProcessOption, tpoStoreTx), TxUtxoBalance (..), tipAsPoint)
import Plutus.ChainIndex.UtxoState (InsertUtxoSuccess (..), RollbackResult (..), UtxoIndex)
import Plutus.ChainIndex.UtxoState qualified as UtxoState
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Api (Credential (..))

type ChainIndexState = UtxoIndex TxUtxoBalance

getResumePoints
    :: Member BeamEffect effs
    => DatabaseSettings Sqlite ChainIndexDb
    -> Eff effs [C.ChainPoint]
getResumePoints db
    = fmap (mapMaybe (toCardanoPoint . tipAsPoint . fromDbValue . Just))
    . selectList . select . orderBy_ (desc_ . _tipRowSlot) . all_ $ tipRows db

handleQuery ::
    ( Member (State ChainIndexState) effs
    , Member (Reader (db (DatabaseEntity Sqlite db))) effs
    , Member BeamEffect effs
    , Member (Error ChainIndexError) effs
    , Member (LogMsg ChainIndexLog) effs
    , db ~ ChainIndexDb
    )
    => ChainIndexQueryEffect ~> Eff effs
handleQuery = \case
    DatumFromHash dh            -> getDatumFromHash dh
    ValidatorFromHash hash      -> getScriptFromHash hash
    MintingPolicyFromHash hash  -> getScriptFromHash hash
    RedeemerFromHash hash       -> getRedeemerFromHash hash
    StakeValidatorFromHash hash -> getScriptFromHash hash
    UnspentTxOutFromRef tor     -> getUtxoutFromRef tor
    UtxoSetMembership r -> do
        utxoState <- gets @ChainIndexState UtxoState.utxoState
        case UtxoState.tip utxoState of
            TipAtGenesis -> throwError QueryFailedNoTip
            tp           -> pure (IsUtxoResponse tp (TxUtxoBalance.isUnspentOutput r utxoState))
    UtxoSetAtAddress pageQuery cred -> getUtxoSetAtAddress pageQuery cred
    UtxoSetWithCurrency pageQuery assetClass ->
      getUtxoSetWithCurrency pageQuery assetClass
    TxoSetAtAddress pageQuery cred -> getTxoSetAtAddress pageQuery cred
    GetTip -> getTip
    GetDiagnostics -> diagnostics

getTip ::
    ( Member BeamEffect effs
    , Member (Reader (db (DatabaseEntity Sqlite db))) effs
    , db ~ ChainIndexDb
    )
    => Eff effs Tip
getTip = do
    db <- ask @(DatabaseSettings Sqlite ChainIndexDb)
    fmap fromDbValue . selectOne . select $ limit_ 1 (orderBy_ (desc_ . _tipRowSlot) (all_ (tipRows db)))

getDatumFromHash ::
    ( Member BeamEffect effs
    , Member (Reader (db (DatabaseEntity Sqlite db))) effs
    , db ~ ChainIndexDb
    )
    => DatumHash
    -> Eff effs (Maybe Datum)
getDatumFromHash dh = do
    ask >>= \db -> queryOne $ queryKeyValue db datumRows _datumRowHash _datumRowDatum dh

getScriptFromHash ::
    ( Member BeamEffect effs
    , HasDbType i
    , DbType i ~ ByteString
    , HasDbType o
    , DbType o ~ ByteString
    , Member (Reader (db (DatabaseEntity Sqlite db))) effs
    , db ~ ChainIndexDb
    )
    => i
    -> Eff effs (Maybe o)
getScriptFromHash sh =
    ask >>= \db -> queryOne $ queryKeyValue db scriptRows _scriptRowHash _scriptRowScript sh

getRedeemerFromHash ::
    ( Member BeamEffect effs
    , HasDbType i
    , DbType i ~ ByteString
    , HasDbType o
    , DbType o ~ ByteString
    , Member (Reader (db (DatabaseEntity Sqlite db))) effs
    , db ~ ChainIndexDb
    )
    => i
    -> Eff effs (Maybe o)
getRedeemerFromHash rh =
    ask >>= \db -> queryOne $ queryKeyValue db redeemerRows _redeemerRowHash _redeemerRowRedeemer rh

-- | Get the 'ChainIndexTxOut' for a 'TxOutRef'.
getUtxoutFromRef ::
  forall db effs.
  ( Member BeamEffect effs
  , Member (LogMsg ChainIndexLog) effs
  , Member (Reader (db (DatabaseEntity Sqlite db))) effs
  , db ~ ChainIndexDb
  )
  => TxOutRef
  -> Eff effs (Maybe ChainIndexTxOut)
getUtxoutFromRef txOutRef = do
    db <- ask
    mTxOut <- queryOne $ queryKeyValue db utxoOutRefRows _utxoRowOutRef _utxoRowTxOut txOutRef
    case mTxOut of
      Nothing -> logWarn (TxOutNotFound txOutRef) >> pure Nothing
      Just txout@TxOut { txOutAddress, txOutValue, txOutDatumHash } ->
        case addressCredential txOutAddress of
          PubKeyCredential _ -> pure $ Just $ PublicKeyChainIndexTxOut txOutAddress txOutValue
          ScriptCredential vh ->
            case txOutDatumHash of
              Nothing -> do
                -- If the txout comes from a script address, the Datum should not be Nothing
                logWarn $ NoDatumScriptAddr txout
                pure Nothing
              Just dh -> do
                v <- maybe (Left vh) Right <$> getScriptFromHash vh
                d <- maybe (Left dh) Right <$> getDatumFromHash dh
                pure $ Just $ ScriptChainIndexTxOut txOutAddress v d txOutValue

getUtxoSetAtAddress ::
    ( Member (State ChainIndexState) effs
    , Member BeamEffect effs
    , Member (LogMsg ChainIndexLog) effs
    , Member (Reader (db (DatabaseEntity Sqlite db))) effs
    , db ~ ChainIndexDb
    )
  => PageQuery TxOutRef
  -> Credential
  -> Eff effs UtxosResponse
getUtxoSetAtAddress pageQuery (toDbValue -> cred) = do
  db <- ask @(DatabaseSettings Sqlite ChainIndexDb)
  utxoState <- gets @ChainIndexState UtxoState.utxoState

  case UtxoState.tip utxoState of
      TipAtGenesis -> do
          logWarn TipIsGenesis
          pure (UtxosResponse TipAtGenesis (Page pageQuery Nothing []))
      tp           -> do
          let query =
                fmap _addressRowOutRef
                  $ filter_ (\row ->
                      (_addressRowCred row ==. val_ cred)
                      &&. exists_ (filter_ (\utxo -> _addressRowOutRef row ==. _unspentOutputRowOutRef utxo) (all_ (unspentOutputRows db)))
                      &&. not_ (exists_ (filter_ (\utxi -> _addressRowOutRef row ==. _unmatchedInputRowOutRef utxi) (all_ (unmatchedInputRows db))))
                      )
                  $ all_ (addressRows db)

          outRefs <- selectPage (fmap toDbValue pageQuery) query
          let page = fmap fromDbValue outRefs

          pure (UtxosResponse tp page)

getUtxoSetWithCurrency ::
    ( Member (State ChainIndexState) effs
    , Member BeamEffect effs
    , Member (LogMsg ChainIndexLog) effs
    , Member (Reader (db (DatabaseEntity Sqlite db))) effs
    , db ~ ChainIndexDb
    )
  => PageQuery TxOutRef
  -> AssetClass
  -> Eff effs UtxosResponse
getUtxoSetWithCurrency pageQuery (toDbValue -> assetClass) = do
  db <- ask @(DatabaseSettings Sqlite ChainIndexDb)
  utxoState <- gets @ChainIndexState UtxoState.utxoState

  case UtxoState.tip utxoState of
      TipAtGenesis -> do
          logWarn TipIsGenesis
          pure (UtxosResponse TipAtGenesis (Page pageQuery Nothing []))
      tp           -> do
          let query =
                fmap _assetClassRowOutRef
                  $ filter_ (\row -> (_assetClassRowAssetClass row ==. val_ assetClass)
                      &&. exists_ (filter_ (\utxo -> _assetClassRowOutRef row ==. _unspentOutputRowOutRef utxo) (all_ (unspentOutputRows db)))
                      &&. not_ (exists_ (filter_ (\utxi -> _assetClassRowOutRef row ==. _unmatchedInputRowOutRef utxi) (all_ (unmatchedInputRows db))))
                      )
                  $ all_ (assetClassRows db)

          outRefs <- selectPage (fmap toDbValue pageQuery) query
          let page = fmap fromDbValue outRefs

          pure (UtxosResponse tp page)

getTxoSetAtAddress ::
    ( Member (State ChainIndexState) effs
    , Member BeamEffect effs
    , Member (LogMsg ChainIndexLog) effs
    , Member (Reader (db (DatabaseEntity Sqlite db))) effs
    , db ~ ChainIndexDb
    )
  => PageQuery TxOutRef
  -> Credential
  -> Eff effs TxosResponse
getTxoSetAtAddress pageQuery (toDbValue -> cred) = do
  db <- ask @(DatabaseSettings Sqlite ChainIndexDb)
  utxoState <- gets @ChainIndexState UtxoState.utxoState
  case UtxoState.tip utxoState of
      TipAtGenesis -> do
          logWarn TipIsGenesis
          pure (TxosResponse (Page pageQuery Nothing []))
      _           -> do
          let query =
                fmap _addressRowOutRef
                  $ filter_ (\row -> _addressRowCred row ==. val_ cred)
                  $ all_ (addressRows db)
          txOutRefs' <- selectPage (fmap toDbValue pageQuery) query
          let page = fmap fromDbValue txOutRefs'
          pure $ TxosResponse page

diagnostics ::
    ( Member BeamEffect effs
    , Member (State ChainIndexState) effs
    , Member (Reader (db (DatabaseEntity Sqlite db))) effs
    , db ~ ChainIndexDb
    )
    => Eff effs Diagnostics
diagnostics = do
    db <- ask @(DatabaseSettings Sqlite ChainIndexDb)
    numScripts <- selectOne . select $ aggregate_ (const countAll_) (all_ (scriptRows db))
    numAddresses <- selectOne . select $ aggregate_ (const countAll_) $ nub_ $ _addressRowCred <$> all_ (addressRows db)
    numAssetClasses <- selectOne . select $ aggregate_ (const countAll_) $ nub_ $ _assetClassRowAssetClass <$> all_ (assetClassRows db)
    TxUtxoBalance outputs inputs <- UtxoState._usTxUtxoData . UtxoState.utxoState <$> get @ChainIndexState

    pure $ Diagnostics
        { numScripts         = fromMaybe (-1) numScripts
        , numAddresses       = fromMaybe (-1) numAddresses
        , numAssetClasses    = fromMaybe (-1) numAssetClasses
        , numUnspentOutputs  = length outputs
        , numUnmatchedInputs = length inputs
        }

queryKeyValue ::
    ( HasDbType key
    , HasSqlEqualityCheck Sqlite (DbType key)
    , BeamSqlBackendCanSerialize Sqlite (DbType key)
    )
    => DatabaseSettings Sqlite ChainIndexDb
    -> (forall f. ChainIndexDb f -> f (TableEntity table))
    -> (forall f. table f -> Columnar f (DbType key))
    -> (forall f. table f -> Columnar f value)
    -> key
    -> SqlSelect Sqlite value
queryKeyValue db table getKey getValue (toDbValue -> key) =
    select $ getValue <$> filter_ (\row -> getKey row ==. val_ key) (all_ (table db))

queryOne ::
    ( Member BeamEffect effs
    , HasDbType o
    )
    => SqlSelect Sqlite (DbType o)
    -> Eff effs (Maybe o)
queryOne = fmap (fmap fromDbValue) . selectOne

handleUtxoIndexer ::
    forall effs db.
    ( Member BeamEffect effs
    , Member (State ChainIndexState) effs
    , Member (Reader Depth) effs
    , Member (LogMsg ChainIndexLog) effs
    , Member (Error ChainIndexError) effs
    , db ~ ChainIndexDb
    )
    => db (DatabaseEntity Sqlite db)
    -> ChainIndexControlEffect
    ~> Eff effs
handleUtxoIndexer db = \case
    AppendBlocks blocks -> do
        let
            processBlock (utxoIndexState, txs, utxoStates) (Block tip_ transactions) = do
                let newUtxoState = TxUtxoBalance.fromBlock tip_ (map fst transactions)
                case UtxoState.insert newUtxoState utxoIndexState of
                    Left err -> do
                        logError $ Err $ InsertionFailed err
                        return (utxoIndexState, txs, utxoStates)
                    Right InsertUtxoSuccess{newIndex, insertPosition} -> do
                        logDebug $ InsertionSuccess tip_ insertPosition
                        return (newIndex, transactions ++ txs, newUtxoState : utxoStates)
        oldIndex <- get @ChainIndexState
        (newIndex, transactions, utxoStates) <- foldM processBlock (oldIndex, [], []) blocks
        depth <- ask @Depth
        reduceOldUtxoDbEffect <- case UtxoState.reduceBlockCount depth newIndex of
          UtxoState.BlockCountNotReduced -> do
            put newIndex
            pure $ Combined []
          lbcResult -> do
            put $ UtxoState.reducedIndex lbcResult
            pure $ reduceOldUtxoDb db $ UtxoState._usTip $ UtxoState.combinedState lbcResult
        combined
            [ reduceOldUtxoDbEffect
            , insertUtxoDb db (map fst transactions) utxoStates
            ]
    Rollback tip_ -> do
        oldIndex <- get @ChainIndexState
        case TxUtxoBalance.rollback tip_ oldIndex of
            Left err -> do
                let reason = RollbackFailed err
                logError $ Err reason
                throwError reason
            Right RollbackResult{newTip, rolledBackIndex} -> do
                put rolledBackIndex
                combined [rollbackUtxoDb db $ tipAsPoint newTip]
                logDebug $ RollbackSuccess newTip
    ResumeSync tip_ -> do
        combined [rollbackUtxoDb db tip_]
        newState <- restoreStateFromDb db
        put newState
    CollectGarbage -> do
        combined
            [ DeleteRows $ truncateTable (utxoOutRefRows db)
            ]
        where
            truncateTable table = delete table (const (val_ True))

handleDatumFromHashIndexer ::
    forall db effs.
    ( Member BeamEffect effs
    , db ~ ChainIndexDb
    )
    => db (DatabaseEntity Sqlite db)
    -> ChainIndexControlEffect
    ~> Eff effs
handleDatumFromHashIndexer db = \case
    AppendBlocks blocks -> do
        let transactions = foldMap (\(Block _ txs) -> txs) blocks
            datumInsertRows (tx, TxProcessOption True) =
                mempty { datumRows = insertRowsFromMap tx _citxData }
            datumInsertRows (_, TxProcessOption False) =
                mempty

        combined
            [ insertRows db $ foldMap datumInsertRows transactions
            ]
    Rollback _ -> pure ()
    ResumeSync _ -> pure ()
    CollectGarbage -> do
        combined
            [ DeleteRows $ truncateTable (datumRows db)
            ]
        where
            truncateTable table = delete table (const (val_ True))

handleRedeemerFromHashIndexer ::
    forall db effs.
    ( Member BeamEffect effs
    , db ~ ChainIndexDb
    )
    => db (DatabaseEntity Sqlite db)
    -> ChainIndexControlEffect
    ~> Eff effs
handleRedeemerFromHashIndexer db = \case
    AppendBlocks blocks -> do
        let transactions = foldMap (\(Block _ txs) -> txs) blocks
            redeemerInsertRows (tx, TxProcessOption True) =
                mempty { redeemerRows = insertRowsFromMap tx _citxRedeemers }
            redeemerInsertRows (_, TxProcessOption False) = mempty
        combined
            [ insertRows db $ foldMap redeemerInsertRows transactions
            ]
    Rollback _ -> pure ()
    ResumeSync _ -> pure ()
    CollectGarbage -> do
        combined
            [ DeleteRows $ truncateTable (redeemerRows db)
            ]
        where
            truncateTable table = delete table (const (val_ True))

handleScriptFromHashIndexer ::
    forall db effs.
    ( Member BeamEffect effs
    , db ~ ChainIndexDb
    )
    => db (DatabaseEntity Sqlite db)
    -> ChainIndexControlEffect
    ~> Eff effs
handleScriptFromHashIndexer db = \case
    AppendBlocks blocks -> do
        let transactions = foldMap (\(Block _ txs) -> txs) blocks
            scriptInsertRows (tx, TxProcessOption True) =
                mempty { scriptRows = insertRowsFromMap tx _citxScripts }
            scriptInsertRows (_, TxProcessOption False) = mempty
        combined
            [ insertRows db $ foldMap scriptInsertRows transactions
            ]
    Rollback _ -> pure ()
    ResumeSync _ -> pure ()
    CollectGarbage -> do
        combined
            [ DeleteRows $ truncateTable (scriptRows db)
            ]
        where
            truncateTable table = delete table (const (val_ True))

handleUtxoFromAddressIndexer ::
    forall db effs.
    ( Member BeamEffect effs
    , db ~ ChainIndexDb
    )
    => db (DatabaseEntity Sqlite db)
    -> ChainIndexControlEffect
    ~> Eff effs
handleUtxoFromAddressIndexer db = \case
    AppendBlocks blocks -> do
        let transactions = foldMap (\(Block _ txs) -> txs) blocks
            utxoAddressInsertRows (tx, TxProcessOption True) =
                mempty { addressRows = insertRowsFromPairs tx (fmap credential . txOutsWithRef) }
            utxoAddressInsertRows (_, TxProcessOption False) = mempty
        combined
            [ insertRows db $ foldMap utxoAddressInsertRows transactions
            ]
        where
            credential :: (TxOut, TxOutRef) -> (Credential, TxOutRef)
            credential (TxOut{txOutAddress=Address{addressCredential}}, ref) =
              (addressCredential, ref)
    Rollback _ -> pure ()
    ResumeSync _ -> pure ()
    CollectGarbage -> do
        combined
            [ DeleteRows $ truncateTable (addressRows db)
            ]
        where
            truncateTable table = delete table (const (val_ True))

handleAssetClassFromAddressIndexer ::
    forall effs db.
    ( Member BeamEffect effs
    , db ~ ChainIndexDb
    )
    => db (DatabaseEntity Sqlite db)
    -> ChainIndexControlEffect
    ~> Eff effs
handleAssetClassFromAddressIndexer db = \case
    AppendBlocks blocks -> do
        let transactions = foldMap (\(Block _ txs) -> txs) blocks
        combined
            [ insertRows db $ foldMap (\(tx, opt) -> if tpoStoreTx opt then mempty { assetClassRows = insertRowsFromPairs tx (concatMap assetClasses . txOutsWithRef) } else mempty) transactions
            ]
        where
            assetClasses :: (TxOut, TxOutRef) -> [(AssetClass, TxOutRef)]
            assetClasses (TxOut{txOutValue}, ref) =
              fmap (\(c, t, _) -> (AssetClass (c, t), ref))
                   -- We don't store the 'AssetClass' when it is the Ada currency.
                   $ filter (\(c, t, _) -> not $ Ada.adaSymbol == c && Ada.adaToken == t)
                   $ flattenValue txOutValue
    Rollback _ -> pure ()
    ResumeSync _ -> pure ()
    CollectGarbage -> do
        combined
            [ DeleteRows $ truncateTable (assetClassRows db)
            ]
        where
            truncateTable table = delete table (const (val_ True))

-- Use a batch size of 200 so that we don't hit the sql too-many-variables
-- limit.
batchSize :: Int
batchSize = 200

insertUtxoDb
    :: DatabaseSettings Sqlite ChainIndexDb
    -> [ChainIndexTx]
    -> [UtxoState.UtxoState TxUtxoBalance]
    -> BeamEffect ()
insertUtxoDb db txs utxoStates =
    let
        go acc (UtxoState.UtxoState _ TipAtGenesis) = acc
        go (tipRows, unspentRows, unmatchedRows) (UtxoState.UtxoState (TxUtxoBalance outputs inputs) tip) =
            let
                tipRowId = TipRowId (toDbValue (tipSlot tip))
                newTips = catMaybes [toDbValue tip]
                newUnspent = UnspentOutputRow tipRowId . toDbValue <$> Set.toList outputs
                newUnmatched = UnmatchedInputRow tipRowId . toDbValue <$> Set.toList inputs
            in
            ( newTips ++ tipRows
            , newUnspent ++ unspentRows
            , newUnmatched ++ unmatchedRows
            )
        (tr, ur, umr) = foldl go ([] :: [TipRow], [] :: [UnspentOutputRow], [] :: [UnmatchedInputRow]) utxoStates
        txOuts = concatMap txOutsWithRef txs
    in insertRows db $ mempty
        { tipRows = InsertRows tr
        , unspentOutputRows = InsertRows ur
        , unmatchedInputRows = InsertRows umr
        , utxoOutRefRows = InsertRows $ (\(txOut, txOutRef) -> UtxoRow (toDbValue txOutRef) (toDbValue txOut)) <$> txOuts
        }

reduceOldUtxoDb :: DatabaseSettings Sqlite ChainIndexDb -> Tip -> BeamEffect ()
reduceOldUtxoDb _ TipAtGenesis = Combined []
reduceOldUtxoDb db (Tip (toDbValue -> slot) _ _) = Combined
    -- Delete all the tips before 'slot'
    [ DeleteRows $ delete (tipRows db) (\row -> _tipRowSlot row <. val_ slot)
    -- Assign all the older utxo changes to 'slot'
    , UpdateRows $ update
        (unspentOutputRows db)
        (\row -> _unspentOutputRowTip row <-. TipRowId (val_ slot))
        (\row -> unTipRowId (_unspentOutputRowTip row) <. val_ slot)
    , UpdateRows $ update
        (unmatchedInputRows db)
        (\row -> _unmatchedInputRowTip row <-. TipRowId (val_ slot))
        (\row -> unTipRowId (_unmatchedInputRowTip row) <. val_ slot)
    -- Among these older changes, delete the matching input/output pairs
    -- We're deleting only the outputs here, the matching input is deleted by a trigger (See Main.hs)
    , DeleteRows $ delete
        (utxoOutRefRows db)
        (\utxoRow ->
            exists_ (filter_
                (\input ->
                    (unTipRowId (_unmatchedInputRowTip input) ==. val_ slot) &&.
                    (_utxoRowOutRef utxoRow ==. _unmatchedInputRowOutRef input))
                (all_ (unmatchedInputRows db))))
    , DeleteRows $ delete
        (unspentOutputRows db)
        (\output -> unTipRowId (_unspentOutputRowTip output) ==. val_ slot &&.
            exists_ (filter_
                (\input ->
                    (unTipRowId (_unmatchedInputRowTip input) ==. val_ slot) &&.
                    (_unspentOutputRowOutRef output ==. _unmatchedInputRowOutRef input))
                (all_ (unmatchedInputRows db))))
    ]

rollbackUtxoDb :: DatabaseSettings Sqlite ChainIndexDb -> Point -> BeamEffect ()
rollbackUtxoDb db PointAtGenesis = DeleteRows $ delete (tipRows db) (const (val_ True))
rollbackUtxoDb db (Point (toDbValue -> slot) _) = Combined
    [ DeleteRows $ delete (tipRows db) (\row -> _tipRowSlot row >. val_ slot)
    , DeleteRows $ delete (utxoOutRefRows db)
        (\utxoRow ->
            exists_ (filter_
                (\output ->
                    (unTipRowId (_unspentOutputRowTip output) >. val_ slot) &&.
                    (_utxoRowOutRef utxoRow ==. _unspentOutputRowOutRef output))
                (all_ (unspentOutputRows db))))
    , DeleteRows $ delete (unspentOutputRows db) (\row -> unTipRowId (_unspentOutputRowTip row) >. val_ slot)
    , DeleteRows $ delete (unmatchedInputRows db) (\row -> unTipRowId (_unmatchedInputRowTip row) >. val_ slot)
    ]

restoreStateFromDb
    :: Member BeamEffect effs
    => DatabaseSettings Sqlite ChainIndexDb
    -> Eff effs ChainIndexState
restoreStateFromDb db = do
    uo <- selectList . select $ all_ (unspentOutputRows db)
    ui <- selectList . select $ all_ (unmatchedInputRows db)
    let balances = Map.fromListWith (<>) $ fmap outputToTxUtxoBalance uo ++ fmap inputToTxUtxoBalance ui
    tips <- selectList . select
        . orderBy_ (asc_ . _tipRowSlot)
        $ all_ (tipRows db)
    pure $ FT.fromList . fmap (toUtxoState balances) $ tips
    where
        outputToTxUtxoBalance :: UnspentOutputRow -> (Word64, TxUtxoBalance)
        outputToTxUtxoBalance (UnspentOutputRow (TipRowId slot) outRef)
            = (slot, TxUtxoBalance (Set.singleton (fromDbValue outRef)) mempty)
        inputToTxUtxoBalance :: UnmatchedInputRow -> (Word64, TxUtxoBalance)
        inputToTxUtxoBalance (UnmatchedInputRow (TipRowId slot) outRef)
            = (slot, TxUtxoBalance mempty (Set.singleton (fromDbValue outRef)))
        toUtxoState :: Map.Map Word64 TxUtxoBalance -> TipRow -> UtxoState.UtxoState TxUtxoBalance
        toUtxoState balances tip@(TipRow slot _ _)
            = UtxoState.UtxoState (Map.findWithDefault mempty slot balances) (fromDbValue (Just tip))

data InsertRows te where
    InsertRows :: BeamableSqlite t => [t Identity] -> InsertRows (TableEntity t)

instance Semigroup (InsertRows te) where
    InsertRows l <> InsertRows r = InsertRows (l <> r)
instance BeamableSqlite t => Monoid (InsertRows (TableEntity t)) where
    mempty = InsertRows []

insertRows :: Database Sqlite db => DatabaseSettings Sqlite db -> db InsertRows -> BeamEffect ()
insertRows dbSettings dbRows =
    getConst
    $ zipTables @Sqlite Proxy (\tbl (InsertRows rows) -> Const $ AddRowsInBatches batchSize tbl rows) dbSettings dbRows

insertRowsFromMap
    :: (BeamableSqlite t, HasDbType (k, v), DbType (k, v) ~ t Identity)
    => tx
    -> (tx-> Map.Map k v)
    -> InsertRows (TableEntity t)
insertRowsFromMap tx l = insertRowsFromPairs tx (Map.toList . l)

insertRowsFromPairs
    :: (BeamableSqlite t, HasDbType (k, v), DbType (k, v) ~ t Identity)
    => tx
    -> (tx -> [(k, v)])
    -> InsertRows (TableEntity t)
insertRowsFromPairs tx l = InsertRows . fmap toDbValue . l $ tx
