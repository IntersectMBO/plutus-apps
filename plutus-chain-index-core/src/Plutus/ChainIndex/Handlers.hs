{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-| Handlers for the 'ChainIndexQueryEffect' and the 'ChainIndexControlEffect' -}
module Plutus.ChainIndex.Handlers
    ( handleQuery
    , handleControl
    , restoreStateFromDb
    , getResumePoints
    , ChainIndexState
    ) where

import Cardano.Api qualified as C
import Control.Applicative (Const (..))
import Control.Lens (Lens', _Just, ix, view, (^?))
import Control.Monad (foldM)
import Control.Monad.Freer (Eff, Member, type (~>))
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Extras.Beam (BeamEffect (..), BeamableSqlite, combined, selectList, selectOne, selectPage)
import Control.Monad.Freer.Extras.Log (LogMsg, logDebug, logError, logWarn)
import Control.Monad.Freer.Extras.Pagination (Page (Page, nextPageQuery, pageItems), PageQuery (..))
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.Freer.State (State, get, gets, put)
import Data.ByteString (ByteString)
import Data.FingerTree qualified as FT
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Word (Word64)
import Database.Beam (Columnar, Identity, SqlSelect, TableEntity, aggregate_, all_, countAll_, delete, filter_, in_,
                      limit_, nub_, select, val_)
import Database.Beam.Backend.SQL (BeamSqlBackendCanSerialize)
import Database.Beam.Query (HasSqlEqualityCheck, asc_, desc_, exists_, guard_, isJust_, isNothing_, leftJoin_, orderBy_,
                            update, (&&.), (<-.), (<.), (==.), (>.))
import Database.Beam.Schema.Tables (zipTables)
import Database.Beam.Sqlite (Sqlite)
import Ledger (Address (..), ChainIndexTxOut (..), TxId, TxOut (..), TxOutRef (..))
import Ledger.Value (AssetClass (AssetClass), flattenValue)
import Plutus.ChainIndex.Api (IsUtxoResponse (IsUtxoResponse), QueryResponse (QueryResponse),
                              TxosResponse (TxosResponse), UtxosResponse (UtxosResponse))
import Plutus.ChainIndex.ChainIndexError (ChainIndexError (..))
import Plutus.ChainIndex.ChainIndexLog (ChainIndexLog (..))
import Plutus.ChainIndex.Compatibility (toCardanoPoint)
import Plutus.ChainIndex.DbSchema
import Plutus.ChainIndex.Effects (ChainIndexControlEffect (..), ChainIndexQueryEffect (..))
import Plutus.ChainIndex.Tx
import Plutus.ChainIndex.TxUtxoBalance qualified as TxUtxoBalance
import Plutus.ChainIndex.Types (ChainSyncBlock (..), Depth (..), Diagnostics (..), Point (..), Tip (..),
                                TxProcessOption (..), TxUtxoBalance (..), tipAsPoint)
import Plutus.ChainIndex.UtxoState (InsertUtxoSuccess (..), RollbackResult (..), UtxoIndex)
import Plutus.ChainIndex.UtxoState qualified as UtxoState
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Api (Credential (..), Datum, DatumHash)

type ChainIndexState = UtxoIndex TxUtxoBalance

getResumePoints :: Member BeamEffect effs => Eff effs [C.ChainPoint]
getResumePoints
    = fmap (mapMaybe (toCardanoPoint . tipAsPoint . fromDbValue . Just))
    . selectList . select . orderBy_ (desc_ . _tipRowSlot) . all_ $ tipRows db

handleQuery ::
    ( Member (State ChainIndexState) effs
    , Member BeamEffect effs
    , Member (Error ChainIndexError) effs
    , Member (LogMsg ChainIndexLog) effs
    ) => ChainIndexQueryEffect
    ~> Eff effs
handleQuery = \case
    DatumFromHash dh            -> getDatumFromHash dh
    ValidatorFromHash hash      -> getScriptFromHash hash
    MintingPolicyFromHash hash  -> getScriptFromHash hash
    RedeemerFromHash hash       -> getRedeemerFromHash hash
    StakeValidatorFromHash hash -> getScriptFromHash hash
    TxFromTxId txId -> getTxFromTxId txId
    TxOutFromRef tor            -> getTxOutFromRef tor
    UnspentTxOutFromRef tor     -> getUtxoutFromRef tor
    UtxoSetMembership r -> do
        utxoState <- gets @ChainIndexState UtxoState.utxoState
        case UtxoState.tip utxoState of
            TipAtGenesis -> throwError QueryFailedNoTip
            tp           -> pure (IsUtxoResponse tp (TxUtxoBalance.isUnspentOutput r utxoState))
    UtxoSetAtAddress pageQuery cred -> getUtxoSetAtAddress pageQuery cred
    UnspentTxOutSetAtAddress pageQuery cred -> getTxOutSetAtAddress pageQuery cred
    UtxoSetWithCurrency pageQuery assetClass ->
      getUtxoSetWithCurrency pageQuery assetClass
    TxoSetAtAddress pageQuery cred -> getTxoSetAtAddress pageQuery cred
    TxsFromTxIds txids -> getTxsFromTxIds txids
    GetTip -> getTip

getTip :: Member BeamEffect effs => Eff effs Tip
getTip = fmap fromDbValue . selectOne . select $ limit_ 1 (orderBy_ (desc_ . _tipRowSlot) (all_ (tipRows db)))

getDatumFromHash :: Member BeamEffect effs => DatumHash -> Eff effs (Maybe Datum)
getDatumFromHash = queryOne . queryKeyValue datumRows _datumRowHash _datumRowDatum

getTxFromTxId :: Member BeamEffect effs => TxId -> Eff effs (Maybe ChainIndexTx)
getTxFromTxId = queryOne . queryKeyValue txRows _txRowTxId _txRowTx

getScriptFromHash ::
    ( Member BeamEffect effs
    , HasDbType i
    , DbType i ~ ByteString
    , HasDbType o
    , DbType o ~ ByteString
    ) => i
    -> Eff effs (Maybe o)
getScriptFromHash = queryOne . queryKeyValue scriptRows _scriptRowHash _scriptRowScript

getRedeemerFromHash ::
    ( Member BeamEffect effs
    , HasDbType i
    , DbType i ~ ByteString
    , HasDbType o
    , DbType o ~ ByteString
    ) => i
    -> Eff effs (Maybe o)
getRedeemerFromHash = queryOne . queryKeyValue redeemerRows _redeemerRowHash _redeemerRowRedeemer

queryKeyValue ::
    ( HasDbType key
    , HasSqlEqualityCheck Sqlite (DbType key)
    , BeamSqlBackendCanSerialize Sqlite (DbType key)
    ) => (forall f. Db f -> f (TableEntity table))
    -> (forall f. table f -> Columnar f (DbType key))
    -> (forall f. table f -> Columnar f value)
    -> key
    -> SqlSelect Sqlite value
queryKeyValue table getKey getValue (toDbValue -> key) =
    select $ getValue <$> filter_ (\row -> getKey row ==. val_ key) (all_ (table db))

queryOne ::
    ( Member BeamEffect effs
    , HasDbType o
    ) => SqlSelect Sqlite (DbType o)
    -> Eff effs (Maybe o)
queryOne = fmap (fmap fromDbValue) . selectOne

queryList ::
    ( Member BeamEffect effs
    , HasDbType o
    ) => SqlSelect Sqlite (DbType o)
    -> Eff effs [o]
queryList = fmap (fmap fromDbValue) . selectList

-- | Get the 'ChainIndexTxOut' for a 'TxOutRef'.
getTxOutFromRef ::
  forall effs.
  ( Member BeamEffect effs
  , Member (LogMsg ChainIndexLog) effs
  )
  => TxOutRef
  -> Eff effs (Maybe ChainIndexTxOut)
getTxOutFromRef ref@TxOutRef{txOutRefId, txOutRefIdx} = do
  mTx <- getTxFromTxId txOutRefId
  -- Find the output in the tx matching the output ref
  case mTx ^? _Just . citxOutputs . _ValidTx . ix (fromIntegral txOutRefIdx) of
    Nothing -> logWarn (TxOutNotFound ref) >> pure Nothing
    Just txout -> do
      -- The output might come from a public key address or a script address.
      -- We need to handle them differently.
      case addressCredential $ txOutAddress txout of
        PubKeyCredential _ ->
          pure $ Just $ PublicKeyChainIndexTxOut (txOutAddress txout) (txOutValue txout)
        ScriptCredential vh -> do
          case txOutDatumHash txout of
            Nothing -> do
              -- If the txout comes from a script address, the Datum should not be Nothing
              logWarn $ NoDatumScriptAddr txout
              pure Nothing
            Just dh -> do
                v <- maybe (Left vh) Right <$> getScriptFromHash vh
                d <- maybe (Left dh) Right <$> getDatumFromHash dh
                pure $ Just $ ScriptChainIndexTxOut (txOutAddress txout) v d (txOutValue txout)

-- | Get the 'ChainIndexTxOut' for a 'TxOutRef'.
getUtxoutFromRef ::
  forall effs.
  ( Member BeamEffect effs
  , Member (LogMsg ChainIndexLog) effs
  )
  => TxOutRef
  -> Eff effs (Maybe ChainIndexTxOut)
getUtxoutFromRef txOutRef = do
    mTxOut <- queryOne $ queryKeyValue utxoOutRefRows _utxoRowOutRef _utxoRowTxOut txOutRef
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

getUtxoSetAtAddress
  :: forall effs.
    ( Member (State ChainIndexState) effs
    , Member BeamEffect effs
    , Member (LogMsg ChainIndexLog) effs
    )
  => PageQuery TxOutRef
  -> Credential
  -> Eff effs UtxosResponse
getUtxoSetAtAddress pageQuery (toDbValue -> cred) = do
  utxoState <- gets @ChainIndexState UtxoState.utxoState

  case UtxoState.tip utxoState of
      TipAtGenesis -> do
          logWarn TipIsGenesis
          pure (UtxosResponse TipAtGenesis (Page pageQuery Nothing []))
      tp           -> do
          let query = do
                rowRef <- fmap _unspentOutputRowOutRef (all_ (unspentOutputRows db))
                rowCred <- leftJoin_
                           (fmap _addressRowOutRef (filter_ (\row -> _addressRowCred row ==. val_ cred) (all_ (addressRows db))))
                           (\row -> row ==. rowRef)
                utxi <- leftJoin_ (fmap _unmatchedInputRowOutRef $ all_ (unmatchedInputRows db)) (\utxi -> rowRef ==. utxi)
                guard_ (isNothing_ utxi)
                guard_ (isJust_ rowCred)
                pure rowRef

          outRefs <- selectPage (fmap toDbValue pageQuery) query
          let page = fmap fromDbValue outRefs

          pure (UtxosResponse tp page)


getTxOutSetAtAddress ::
  forall effs.
  ( Member (State ChainIndexState) effs
  , Member BeamEffect effs
  , Member (LogMsg ChainIndexLog) effs
  )
  => PageQuery TxOutRef
  -> Credential
  -> Eff effs (QueryResponse [(TxOutRef, ChainIndexTxOut)])
getTxOutSetAtAddress pageQuery cred = do
  (UtxosResponse tip page) <- getUtxoSetAtAddress pageQuery cred
  case tip of
    TipAtGenesis -> do
      pure (QueryResponse [] Nothing)
    _             -> do
      mtxouts <- mapM getUtxoutFromRef (pageItems page)
      let txouts = [ (t, o) | (t, mo) <- List.zip (pageItems page) mtxouts, o <- maybeToList mo]
      pure $ QueryResponse txouts (nextPageQuery page)


getUtxoSetWithCurrency
  :: forall effs.
    ( Member (State ChainIndexState) effs
    , Member BeamEffect effs
    , Member (LogMsg ChainIndexLog) effs
    )
  => PageQuery TxOutRef
  -> AssetClass
  -> Eff effs UtxosResponse
getUtxoSetWithCurrency pageQuery (toDbValue -> assetClass) = do
  utxoState <- gets @ChainIndexState UtxoState.utxoState

  case UtxoState.tip utxoState of
      TipAtGenesis -> do
          logWarn TipIsGenesis
          pure (UtxosResponse TipAtGenesis (Page pageQuery Nothing []))
      tp           -> do
          let query = do
                rowRef <- fmap _unspentOutputRowOutRef (all_ (unspentOutputRows db))
                rowClass <- leftJoin_
                            (fmap _assetClassRowOutRef (filter_ (\row -> _assetClassRowAssetClass row ==. val_ assetClass) (all_ (assetClassRows db))))
                            (\row -> row ==. rowRef)
                utxi <- leftJoin_ (fmap _unmatchedInputRowOutRef $ all_ (unmatchedInputRows db)) (\utxi -> rowRef ==. utxi)
                guard_ (isNothing_ utxi)
                guard_ (isJust_ rowClass)
                pure rowRef

          outRefs <- selectPage (fmap toDbValue pageQuery) query
          let page = fmap fromDbValue outRefs

          pure (UtxosResponse tp page)

getTxsFromTxIds
  :: forall effs.
    ( Member BeamEffect effs
    )
  => [TxId]
  -> Eff effs [ChainIndexTx]
getTxsFromTxIds txIds =
  do
    let
      txIds' = toDbValue <$> txIds
      query =
        fmap _txRowTx
          $ filter_ (\row -> _txRowTxId row `in_` fmap val_ txIds')
          $ all_ (txRows db)
    txs <- selectList $ select query
    pure $ fmap fromDbValue txs

getTxoSetAtAddress
  :: forall effs.
    ( Member (State ChainIndexState) effs
    , Member BeamEffect effs
    , Member (LogMsg ChainIndexLog) effs
    )
  => PageQuery TxOutRef
  -> Credential
  -> Eff effs TxosResponse
getTxoSetAtAddress pageQuery (toDbValue -> cred) = do
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

appendBlocks ::
    forall effs.
    ( Member (State ChainIndexState) effs
    , Member (Reader Depth) effs
    , Member BeamEffect effs
    , Member (LogMsg ChainIndexLog) effs
    )
    => [ChainSyncBlock] -> Eff effs ()
appendBlocks [] = pure ()
appendBlocks blocks = do
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
        pure $ reduceOldUtxoDb $ UtxoState._usTip $ UtxoState.combinedState lbcResult
    combined
        [ reduceOldUtxoDbEffect
        , insertRows $ foldMap (\(tx, opt) -> if tpoStoreTx opt then fromTx tx else mempty) transactions
        , insertUtxoDb (map fst transactions) utxoStates
        ]

handleControl ::
    forall effs.
    ( Member (State ChainIndexState) effs
    , Member (Reader Depth) effs
    , Member BeamEffect effs
    , Member (Error ChainIndexError) effs
    , Member (LogMsg ChainIndexLog) effs
    )
    => ChainIndexControlEffect
    ~> Eff effs
handleControl = \case
    AppendBlocks blocks -> appendBlocks blocks
    Rollback tip_ -> do
        oldIndex <- get @ChainIndexState
        case TxUtxoBalance.rollback tip_ oldIndex of
            Left err -> do
                let reason = RollbackFailed err
                logError $ Err reason
                throwError reason
            Right RollbackResult{newTip, rolledBackIndex} -> do
                put rolledBackIndex
                combined [rollbackUtxoDb $ tipAsPoint newTip]
                logDebug $ RollbackSuccess newTip
    ResumeSync tip_ -> do
        combined [rollbackUtxoDb tip_]
        newState <- restoreStateFromDb
        put newState
    CollectGarbage -> do
        -- Rebuild the index using only transactions that still have at
        -- least one output in the UTXO set
        utxos <- gets $
            Set.toList
            . Set.map txOutRefId
            . TxUtxoBalance.unspentOutputs
            . UtxoState.utxoState
        rows <- foldMap fromTx . catMaybes <$> mapM getTxFromTxId utxos
        combined $
            [ DeleteRows $ truncateTable (datumRows db)
            , DeleteRows $ truncateTable (scriptRows db)
            , DeleteRows $ truncateTable (redeemerRows db)
            , DeleteRows $ truncateTable (txRows db)
            , DeleteRows $ truncateTable (utxoOutRefRows db)
            , DeleteRows $ truncateTable (addressRows db)
            , DeleteRows $ truncateTable (assetClassRows db)
            ] ++ getConst (zipTables Proxy (\tbl (InsertRows r) -> Const [AddRowsInBatches batchSize tbl r]) db rows)

        where
            truncateTable table = delete table (const (val_ True))
    GetDiagnostics -> diagnostics


-- Use a batch size of 200 so that we don't hit the sql too-many-variables
-- limit.
batchSize :: Int
batchSize = 200

insertUtxoDb
    :: [ChainIndexTx]
    -> [UtxoState.UtxoState TxUtxoBalance]
    -> BeamEffect ()
insertUtxoDb txs utxoStates =
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
            , newUnmatched ++ unmatchedRows)
        (tr, ur, umr) = foldl go ([] :: [TipRow], [] :: [UnspentOutputRow], [] :: [UnmatchedInputRow]) utxoStates
        txOuts = concatMap txOutsWithRef txs
    in insertRows $ mempty
        { tipRows = InsertRows tr
        , unspentOutputRows = InsertRows ur
        , unmatchedInputRows = InsertRows umr
        , utxoOutRefRows = InsertRows $ (\(txOut, txOutRef) -> UtxoRow (toDbValue txOutRef) (toDbValue txOut)) <$> txOuts
        }

reduceOldUtxoDb :: Tip -> BeamEffect ()
reduceOldUtxoDb TipAtGenesis = Combined []
reduceOldUtxoDb (Tip (toDbValue -> slot) _ _) = Combined
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

rollbackUtxoDb :: Point -> BeamEffect ()
rollbackUtxoDb PointAtGenesis = DeleteRows $ delete (tipRows db) (const (val_ True))
rollbackUtxoDb (Point (toDbValue -> slot) _) = Combined
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

restoreStateFromDb :: Member BeamEffect effs => Eff effs ChainIndexState
restoreStateFromDb = do
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

insertRows :: Db InsertRows -> BeamEffect ()
insertRows = getConst . zipTables Proxy (\tbl (InsertRows rows) -> Const $ AddRowsInBatches batchSize tbl rows) db

fromTx :: ChainIndexTx -> Db InsertRows
fromTx tx = mempty
    { datumRows = fromMap citxData
    , scriptRows = fromMap citxScripts
    , redeemerRows = fromPairs (Map.toList . txRedeemersWithHash)
    , txRows = InsertRows [toDbValue (_citxTxId tx, tx)]
    , addressRows = fromPairs (fmap credential . txOutsWithRef)
    , assetClassRows = fromPairs (concatMap assetClasses . txOutsWithRef)
    }
    where
        credential :: (TxOut, TxOutRef) -> (Credential, TxOutRef)
        credential (TxOut{txOutAddress=Address{addressCredential}}, ref) =
          (addressCredential, ref)
        assetClasses :: (TxOut, TxOutRef) -> [(AssetClass, TxOutRef)]
        assetClasses (TxOut{txOutValue}, ref) =
          fmap (\(c, t, _) -> (AssetClass (c, t), ref))
               -- We don't store the 'AssetClass' when it is the Ada currency.
               $ filter (\(c, t, _) -> not $ Ada.adaSymbol == c && Ada.adaToken == t)
               $ flattenValue txOutValue
        fromMap
            :: (BeamableSqlite t, HasDbType (k, v), DbType (k, v) ~ t Identity)
            => Lens' ChainIndexTx (Map.Map k v)
            -> InsertRows (TableEntity t)
        fromMap l = fromPairs (Map.toList . view l)
        fromPairs
            :: (BeamableSqlite t, HasDbType (k, v), DbType (k, v) ~ t Identity)
            => (ChainIndexTx -> [(k, v)])
            -> InsertRows (TableEntity t)
        fromPairs l = InsertRows . fmap toDbValue . l $ tx


diagnostics ::
    ( Member BeamEffect effs
    , Member (State ChainIndexState) effs
    ) => Eff effs Diagnostics
diagnostics = do
    numTransactions <- selectOne . select $ aggregate_ (const countAll_) (all_ (txRows db))
    txIds <- queryList . select $ _txRowTxId <$> limit_ 10 (all_ (txRows db))
    numScripts <- selectOne . select $ aggregate_ (const countAll_) (all_ (scriptRows db))
    numAddresses <- selectOne . select $ aggregate_ (const countAll_) $ nub_ $ _addressRowCred <$> all_ (addressRows db)
    numAssetClasses <- selectOne . select $ aggregate_ (const countAll_) $ nub_ $ _assetClassRowAssetClass <$> all_ (assetClassRows db)
    TxUtxoBalance outputs inputs <- UtxoState._usTxUtxoData . UtxoState.utxoState <$> get @ChainIndexState

    pure $ Diagnostics
        { numTransactions    = fromMaybe (-1) numTransactions
        , numScripts         = fromMaybe (-1) numScripts
        , numAddresses       = fromMaybe (-1) numAddresses
        , numAssetClasses    = fromMaybe (-1) numAssetClasses
        , numUnspentOutputs  = length outputs
        , numUnmatchedInputs = length inputs
        , someTransactions   = txIds
        }
