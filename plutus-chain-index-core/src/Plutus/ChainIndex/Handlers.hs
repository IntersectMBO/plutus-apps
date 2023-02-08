{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TupleSections         #-}
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
import Control.Lens (_Just, ix, to, (^?))
import Control.Monad (foldM, void)
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.Freer (Eff, Member, type (~>))
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Extras.Beam (BeamEffect (..), BeamableSqlite, combined, selectList, selectOne, selectPage)
import Control.Monad.Freer.Extras.Log (LogMsg, logDebug, logError, logWarn, logInfo)
import Control.Monad.Freer.Extras.Pagination (Page (Page), PageQuery (..), PageSize (..))
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.Freer.State (State, get, put)
import Data.ByteString (ByteString)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Database.Beam (Columnar, Identity, SqlSelect, TableEntity, aggregate_, all_, countAll_, delete, filter_, in_,
                      limit_, nub_, select, val_)
import Database.Beam.Backend.SQL (BeamSqlBackendCanSerialize)
import Database.Beam.Query (HasSqlEqualityCheck, asc_, desc_, guard_, isNothing_, join_, leftJoin_, orderBy_,
                            (&&.), (/=.), (<=.), (==.), (>.))
import Database.Beam.Schema.Tables (zipTables)
import Database.Beam.Sqlite (Sqlite)
import Ledger (Datum, DatumHash (..), Slot, TxId, TxOutRef (..))
import Ledger qualified as L
import Ledger.Value (AssetClass, assetClassValueOf)
import Plutus.ChainIndex.Api (IsUtxoResponse (IsUtxoResponse), QueryResponse (QueryResponse),
                              TxosResponse (TxosResponse), UtxosResponse (UtxosResponse))
import Plutus.ChainIndex.ChainIndexError (ChainIndexError (..), RollbackFailed(..))
import Plutus.ChainIndex.ChainIndexLog (ChainIndexLog (..), BlockReductionState(..))
import Plutus.ChainIndex.Compatibility (toCardanoPoint)
import Plutus.ChainIndex.DbSchema
import Plutus.ChainIndex.Effects (ChainIndexControlEffect (..), ChainIndexQueryEffect (..))
import Plutus.ChainIndex.Tx
import Plutus.ChainIndex.Tx qualified as ChainIndex
import Plutus.ChainIndex.TxUtxoBalance qualified as TxUtxoBalance
import Plutus.ChainIndex.Types (BlockNumber(..), ChainIndexInternalTx (..), ChainSyncBlock (..), ChainSyncState (..), Depth (..),
                                Diagnostics (..), Point (..), Tip (..),
                                TxProcessOption (..), TxUtxoBalance (..), fromReferenceScript, pointsToTip, tipAsPoint)
import Plutus.ChainIndex.UtxoState qualified as UtxoState
import Plutus.Contract.CardanoAPI as CAPI
import Plutus.Script.Utils.Scripts (datumHash)
import Plutus.V2.Ledger.Api (Credential (..))
import PlutusTx.Builtins.Internal (emptyByteString)

-- we only require to keep track of the tips to perform block reduction
-- keep the utxoIndex is not necessary
-- utxoState is only used when inserting in db
type ChainIndexState = [Tip]

getResumePoints :: Member BeamEffect effs => Eff effs [C.ChainPoint]
getResumePoints
    = fmap (mapMaybe (toCardanoPoint . tipAsPoint . fromDbValue . Just))
    . selectList . select . orderBy_ (desc_ . _tipRowSlot) . all_ $ tipRows db


-- | returns the current tip only when we are not at genesis
getCurrentTip :: ChainIndexState -> Maybe Tip
getCurrentTip [] = Nothing
getCurrentTip (TipAtGenesis : _) = Nothing
getCurrentTip (tp : _) = Just tp

handleQuery ::
    ( Member (State ChainIndexState) effs
    , Member BeamEffect effs
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
    UtxoSetMembership r         -> isUtxoUnspent r
    UtxoSetAtAddress pageQuery cred -> getUtxoSetAtAddress pageQuery cred
    UnspentTxOutSetAtAddress pageQuery cred -> getTxOutSetAtAddress pageQuery cred
    DatumsAtAddress pageQuery cred -> getDatumsAtAddress pageQuery cred
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
  -> Eff effs (Maybe L.ChainIndexTxOut)
getTxOutFromRef ref@TxOutRef{txOutRefId, txOutRefIdx} = do
  mTx <- getTxFromTxId txOutRefId
  -- Find the output in the tx matching the output ref
  case mTx ^? _Just . to txOuts . ix (fromIntegral txOutRefIdx) of
    Nothing    -> logWarn (TxOutNotFound ref) >> pure Nothing
    Just txout -> makeChainIndexTxOut txout


-- | Get the 'ChainIndexTxOut' for a 'TxOutRef'.
getUtxoutFromRef ::
  forall effs.
  ( Member BeamEffect effs
  , Member (LogMsg ChainIndexLog) effs
  )
  => TxOutRef
  -> Eff effs (Maybe L.ChainIndexTxOut)
getUtxoutFromRef txOutRef = do
    mTxOut <- queryOne $ queryKeyValue utxoOutRefRows _utxoRowOutRef _utxoRowTxOut txOutRef
    case mTxOut of
      Nothing    -> logWarn (TxOutNotFound txOutRef) >> pure Nothing
      Just txout -> makeChainIndexTxOut txout


isUtxoUnspent
  :: forall effs.
    ( Member (State ChainIndexState) effs
    , Member BeamEffect effs
    )
  => TxOutRef
  -> Eff effs IsUtxoResponse
isUtxoUnspent (toDbValue -> utxo) = do
  indexState <- get @ChainIndexState
  case getCurrentTip indexState of
    Nothing -> pure $ IsUtxoResponse TipAtGenesis False
    Just tp -> do
      let query = do
            rowRef <- fmap _unspentOutputRowOutRef (all_ (unspentOutputRows db))
            utxi <- leftJoin_ (fmap _unmatchedInputRowOutRef $ all_ (unmatchedInputRows db)) (\utxi -> rowRef ==. utxi)
            guard_ (isNothing_ utxi)
            guard_ (rowRef ==. val_ utxo)
            pure rowRef
      mTxOut <- selectOne $ select query
      pure (IsUtxoResponse tp (isJust mTxOut))


makeChainIndexTxOut ::
  forall effs.
  ( Member BeamEffect effs
  , Member (LogMsg ChainIndexLog) effs
  )
  => ChainIndex.ChainIndexTxOut
  -> Eff effs (Maybe L.ChainIndexTxOut)
makeChainIndexTxOut txout@(ChainIndexTxOut address value datum refScript) = do
  datumWithHash <- getDatumWithHash datum
  case addressCredential address of
    PubKeyCredential _ ->
        pure $ Just $ L.PublicKeyChainIndexTxOut address value datumWithHash script
    ScriptCredential vh ->
      case datumWithHash of
        Just d -> do
          v <- getScriptFromHash vh
          pure $ Just $ L.ScriptChainIndexTxOut address value d script (vh, v)
        Nothing -> do
          -- If the txout comes from a script address, the Datum should not be Nothing
          logWarn $ NoDatumScriptAddr txout
          pure Nothing
  where
    getDatumWithHash :: OutputDatum -> Eff effs (Maybe (DatumHash, Maybe Datum))
    getDatumWithHash NoOutputDatum = pure Nothing
    getDatumWithHash (OutputDatumHash dh) = do
        d <- getDatumFromHash dh
        pure $ Just (dh, d)
    getDatumWithHash (OutputDatum d) = do
        pure $ Just (datumHash d, Just d)

    script = fromReferenceScript refScript

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
  indexState <- get @ChainIndexState
  case getCurrentTip indexState of
    Nothing -> do
      logWarn TipIsGenesis
      pure (UtxosResponse TipAtGenesis (Page pageQuery Nothing []))
    Just tp -> do
      let query = do
            unspent <- fmap _unspentOutputRowOutRef (all_ (unspentOutputRows db))
            void $ join_ (addressRows db) (\row -> (_addressRowOutRef row ==. unspent) &&. (_addressRowCred row ==. val_ cred))
            utxi <- leftJoin_ (fmap _unmatchedInputRowOutRef $ all_ (unmatchedInputRows db)) (\utxi -> unspent ==. utxi)
            guard_ (isNothing_ utxi)
            pure $ unspent
      outRefs <- selectPage (fmap toDbValue pageQuery) query
      let page = fmap fromDbValue outRefs
      pure (UtxosResponse tp page)


createNextPageQuery :: [(TxOutRef, a)] -> PageSize -> Maybe (PageQuery TxOutRef)
createNextPageQuery items p@(PageSize ps)
 | length items > fromIntegral ps = Just $ PageQuery p (listToMaybe $ List.tail $ List.reverse $ fst <$> items)
 | otherwise = Nothing


getTxOutSetAtAddress ::
  forall effs.
  ( Member (State ChainIndexState) effs
  , Member BeamEffect effs
  , Member (LogMsg ChainIndexLog) effs
  )
  => PageQuery TxOutRef
  -> Credential
  -> Eff effs (QueryResponse [(TxOutRef, L.ChainIndexTxOut)])
getTxOutSetAtAddress PageQuery { pageQuerySize = p@(PageSize ps), pageQueryLastItem } (toDbValue -> cred) = do
  indexState <- get @ChainIndexState
  case getCurrentTip indexState of
    Nothing -> do
      logWarn TipIsGenesis
      pure (QueryResponse [] Nothing)
    Just _ -> do
      let query = do
            unspent <- fmap _unspentOutputRowOutRef (all_ (unspentOutputRows db))
            void $ join_ (addressRows db) (\row -> (_addressRowOutRef row ==. unspent) &&. (_addressRowCred row ==. val_ cred))
            txout <- fmap _utxoRowTxOut $ join_ (utxoOutRefRows db) (\row -> (_utxoRowOutRef row ==. unspent))
            utxi <- leftJoin_ (fmap _unmatchedInputRowOutRef $ all_ (unmatchedInputRows db)) (\utxi -> unspent ==. utxi)
            guard_ (isNothing_ utxi)
            pure (unspent, txout)
      let ps' = fromIntegral ps
      utxos <- fmap (fmap (\(r, o) -> (fromDbValue r, fromDbValue o)))
               $ selectList
               $ select
               $ limit_ (ps' + 1)
               $ orderBy_ (asc_ . fst)
               $ filter_ (\(ref, _) -> maybe (val_ True)
                                       (\lastItem -> ref >. val_ lastItem)
                                       (fmap toDbValue pageQueryLastItem)
                         ) query
      utxos' <- mapMaybeM (\(r, o) -> do
                              mo <- makeChainIndexTxOut o
                              pure $ maybe Nothing (\o' -> Just (r, o')) mo
                          ) utxos
      pure $ QueryResponse utxos' (createNextPageQuery utxos p)


getDatumsAtAddress ::
  forall effs.
    ( Member (State ChainIndexState) effs
    , Member BeamEffect effs
    , Member (LogMsg ChainIndexLog) effs
    )
  => PageQuery TxOutRef
  -> Credential
  -> Eff effs (QueryResponse [Datum])
getDatumsAtAddress PageQuery { pageQuerySize = p@(PageSize ps), pageQueryLastItem } (toDbValue -> cred) = do
  indexState <- get @ChainIndexState
  case getCurrentTip indexState of
    Nothing -> do
      logWarn TipIsGenesis
      pure (QueryResponse [] Nothing)
    Just _ -> do
      let emptyHash = (toDbValue $ DatumHash emptyByteString)
          query = do
            addrRow <- filter_ (\row ->
                                  ( _addressRowCred row ==. val_ cred )
                                  &&. (_addressRowDatumHash row /=. val_ emptyHash) )
                       $ all_ (addressRows db)
            datumRow <- fmap _datumRowDatum $ join_ (datumRows db) (\row -> _datumRowHash row ==. _addressRowDatumHash addrRow)
            pure (_addressRowOutRef addrRow, datumRow)
      let ps' = fromIntegral ps
      res <- fmap (fmap (\(r, d) -> (fromDbValue r, fromDbValue d)))
             $ selectList
             $ select
             $ limit_ (ps' + 1)
             $ orderBy_ (asc_ . fst)
             $ filter_ (\(ref, _) -> maybe (val_ True)
                                     (\lastItem -> ref >. val_ lastItem)
                                     (fmap toDbValue pageQueryLastItem)
                       ) query
      pure $ QueryResponse (snd <$> res) (createNextPageQuery res p)


getUtxoSetWithCurrency
  :: forall effs.
    ( Member (State ChainIndexState) effs
    , Member BeamEffect effs
    , Member (LogMsg ChainIndexLog) effs
    )
  => PageQuery TxOutRef
  -> AssetClass
  -> Eff effs UtxosResponse
getUtxoSetWithCurrency pq@(PageQuery { pageQuerySize = p@(PageSize ps), pageQueryLastItem }) assetClass = do
  indexState <- get @ChainIndexState
  case getCurrentTip indexState of
    Nothing -> do
      logWarn TipIsGenesis
      pure (UtxosResponse TipAtGenesis (Page pq Nothing []))
    Just tip -> do
      let query = do
            unspent <- fmap _unspentOutputRowOutRef (all_ (unspentOutputRows db))
            txout <- fmap _utxoRowTxOut $ join_ (utxoOutRefRows db) (\row -> (_utxoRowOutRef row ==. unspent))
            utxi <- leftJoin_ (fmap _unmatchedInputRowOutRef $ all_ (unmatchedInputRows db)) (\utxi -> unspent ==. utxi)
            guard_ (isNothing_ utxi)
            pure (unspent, txout)
      let ps' = fromIntegral ps
      utxos <- fmap (fmap (\(r, o) -> (fromDbValue r, fromDbValue o)))
               $ selectList
               $ select
               $ limit_ (ps' + 1)
               $ orderBy_ (asc_ . fst)
               $ filter_ (\(ref, _) -> maybe (val_ True)
                                       (\lastItem -> ref >. val_ lastItem)
                                       (fmap toDbValue pageQueryLastItem)
                         ) query
      utxos' <- mapMaybeM (\(r, o) -> do
                              mo <- makeChainIndexTxOut o
                              pure $ maybe Nothing (\o' -> Just (r, o')) mo
                          ) utxos
      let txrefs = [ t | (t, o) <- utxos', (assetClassValueOf (L._ciTxOutValue o) assetClass) > 0 ]
      pure $ UtxosResponse tip (Page pq (createNextPageQuery utxos p) txrefs)


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
  indexState <- get @ChainIndexState
  case getCurrentTip indexState of
    Nothing -> do
      logWarn TipIsGenesis
      pure (TxosResponse (Page pageQuery Nothing []))
    Just _ -> do
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
    => ChainSyncState -> [ChainSyncBlock] -> Eff effs ()
appendBlocks _ [] = pure ()
appendBlocks _syncState blocks = do
    let processBlock (indexState, txs, utxoStates) (ChainIndexBlock tip_ transactions) = do
          let newUtxoState = TxUtxoBalance.fromBlock tip_
                             $ map (\(t, _) -> CAPI.toChainIndexTxEmptyScripts t) transactions
          -- new tip is added at the beginning
          return (tip_ : indexState, transactions ++ txs, newUtxoState : utxoStates)
        processBlock _ (EmulatorBlock _ _) = error "appendBlocks: Unexpected EmulatorBlock in chain-index !!!"
    oldIndex <- get @ChainIndexState
    (newIndex, transactions, utxoStates) <- foldM processBlock (oldIndex, [], []) blocks
    depth <- ask @Depth
    reducedTip <-
      case reduceBlocks depth newIndex of
        Nothing -> do
          put newIndex
          pure Nothing
        Just (reducedIndex, tp) -> do
          let !i_nbTips = fromIntegral $ length newIndex
          logInfo $ BlockReductionPhase BeginReduction depth i_nbTips (i_nbTips - (fromIntegral $ length reducedIndex))
          put reducedIndex
          combined [ reduceOldUtxoDb tp ]
          logInfo $ BlockReductionPhase EndReduction depth i_nbTips (i_nbTips - (fromIntegral $ length reducedIndex))
          pure $ Just tp
    combined
        [ insertRows $ foldMap (\(tx, opt) -> if tpoStoreTx opt then fromTx tx else mempty) transactions
        , insertUtxoDb reducedTip (map (\(t, _) -> CAPI.toChainIndexTxEmptyScripts t) transactions) utxoStates
        ]
    newIndex' <- get @ChainIndexState
    logDebug $ AppendBlockSuccess $ List.take 10 newIndex'

  where
    -- | Reduces the number of tips. The given number corresponds to the node's security parameter.
    -- The index is reduced when the number of blocks is twice this size.
    -- Returns the tip at which blocks can be ignored. Note that unlike UtxoState.reduceBlockCount, this function
    -- considers the proper block number to determine at which tip rollback cannot happen.
    reduceBlocks :: Depth -> ChainIndexState -> Maybe (ChainIndexState, Tip)
    reduceBlocks (Depth minCount) indexState =
      case getCurrentTip indexState of
        Just (Tip s bi bn) | (length indexState > 2 * minCount) ->
          -- no that ordering on tip only considers block number
          -- so no need to adjust slot number
          let bn' = BlockNumber ((unBlockNumber bn) - (fromIntegral minCount))
              (keep, old) = List.span (\t -> t > (Tip s bi bn')) indexState
          in fmap (\t -> (keep, t)) $ getCurrentTip old
        _ -> Nothing

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
    AppendBlocks isSync blocks -> appendBlocks isSync blocks
    Rollback tip_ -> do
      oldIndex <- get @ChainIndexState
      case rollBackChainState tip_ oldIndex of
        Left err -> do
          let reason = RollbackFailed err
          logError $ Err reason
          throwError reason
        Right (newTip, rolledBackIndex) -> do
          put rolledBackIndex
          combined [rollbackUtxoDb $ tipAsPoint newTip]
          logDebug $ RollbackSuccess newTip
    ResumeSync tip_ -> do
      combined [rollbackUtxoDb tip_]
      newState <- restoreStateFromDb
      put newState
    CollectGarbage -> pure ()
      -- do nothing as no more required
    GetDiagnostics -> diagnostics



-- | Perform a rollback on the chain index state.
-- Returns an error when rollback cannot be performed
rollBackChainState :: Point -> ChainIndexState -> Either RollbackFailed (Tip, ChainIndexState)
-- forcing re-synchronisation of chain index
rollBackChainState PointAtGenesis _ = Right (TipAtGenesis, mempty)
rollBackChainState targetPoint indexState =
  case getCurrentTip indexState of
    Nothing -> Right (TipAtGenesis, mempty)
      -- Partial synchronisation, starting from a given block id.
    Just currentTip ->
      -- Already at the target point
     if | targetPoint `pointsToTip` currentTip -> Right (currentTip, indexState)
        -- The rollback happened sometimes after the current tip.
        | not (targetPoint `UtxoState.pointLessThanTip` currentTip) -> Left $ TipMismatch currentTip targetPoint (List.take 10 indexState)
        | otherwise ->
           let (_after, before) = List.span (\t -> targetPoint `UtxoState.pointLessThanTip` t) indexState
           in case getCurrentTip before of
                Nothing -> Left $ OldPointNotFound targetPoint
                Just oldTip | targetPoint `pointsToTip` oldTip -> Right (oldTip, before)
                Just oldTip -> Left $ RollbackTipMismatch oldTip targetPoint (List.take 10 indexState)


-- | insertUtxoDb also performes block reduction when reduced tip is specified
-- Note that ignoring spent utxo and unspent utxos is safe for reduced blocks as
--  - an unspent utxo can be ignored only when the corresponding spent one is also in the list of blocks being handled,
--    i.e., the unspent utxo cannot reference a spent utxo on disk (not possible)
--  - spent utxo can be ignored only when the corresponding unspent is deleted due to the reason mentioned above.
insertUtxoDb
    :: Maybe Tip
    -- ^ reference tip to be used when performing block reduction on acquired state
    -> [ChainIndexTx]
    -> [UtxoState.UtxoState TxUtxoBalance]
    -> BeamEffect ()
insertUtxoDb reducedTip txs utxoStates =
    let
        go acc (UtxoState.UtxoState _ TipAtGenesis) = acc
        go (tipRows, unspentRows, unmatchedRows) (UtxoState.UtxoState (TxUtxoBalance outputs inputs) tip) =
            let !slot' = updateSlot (tipSlot tip) reducedTip
                -- update slot to reduced slot (when required) to facilitate reduction
                newTips = if slotLeqReducedTip (tipSlot tip) reducedTip then [] else [tip]
                newUnspent = map (\o -> (slot', o)) $ Set.toList outputs
                newUnmatched = map (\i -> (slot', i)) $ Set.toList inputs
            in
            ( newTips ++ tipRows
            , newUnspent ++ unspentRows
            , newUnmatched ++ unmatchedRows)
        (tr, ur, umr) = foldl go ([], [], []) utxoStates
        outs = concatMap txOutsWithRef txs
        (ur', umr', outs') = performReduction ur umr outs
    in
      insertRows $ mempty
        { tipRows = InsertRows $ catMaybes $ toDbValue <$> tr
        , unspentOutputRows = InsertRows $ map (\(s, o) -> UnspentOutputRow (TipRowId (toDbValue s)) (toDbValue o)) ur'
        , unmatchedInputRows = InsertRows $ map (\(s, i) -> UnmatchedInputRow (TipRowId (toDbValue s)) (toDbValue i)) umr'
        , utxoOutRefRows = InsertRows $ map (\(txOut, txOutRef) -> UtxoRow (toDbValue txOutRef) (toDbValue txOut)) outs'
        }
   where
     performReduction
       :: [(Slot, TxOutRef)]
       -- ^ unspent utxos
       -> [(Slot, TxOutRef)]
       -- ^ spent utxos
       -> [(ChainIndexTxOut, TxOutRef)]
       -- ^ txouts
       -> ([(Slot, TxOutRef)], [(Slot, TxOutRef)], [(ChainIndexTxOut, TxOutRef)])
     performReduction ur umr outs
      | reducedTipIsSet reducedTip =
          -- ignore unspent outputs with a matching spent input before or at reduced tip
          let !s_umr = Set.fromList umr
              !(!delete_ur, !keep_ur) = List.partition (\e@(s, _) -> Set.member e s_umr && isReducedTip s reducedTip) ur
              -- ignore spent input for which unspent output has been deleted
              -- We can only delete a spent input if the unspent output is also in the list of acquired blocks
              -- Otherwise, the unspent output in DB will never be deleted. This will create an inconsistency
              !s_dur = Set.fromList delete_ur
              !(!delete_umr, !keep_umr) = List.partition (\e -> Set.member e s_dur) umr
              -- ignored txouts with a matching deleted spent input before or at reduced tip
              !s_dumr = Set.fromList $ snd <$> delete_umr
              outs' = List.filter (\(_, r) -> not (Set.member r s_dumr)) outs
          in (keep_ur, keep_umr, outs')
      | otherwise = (ur, umr, outs)

     reducedTipIsSet :: Maybe Tip -> Bool
     reducedTipIsSet Nothing = False
     reducedTipIsSet (Just TipAtGenesis) = False
     reducedTipIsSet _ = True

     slotLeqReducedTip :: Slot -> Maybe Tip -> Bool
     slotLeqReducedTip _ Nothing = False
     slotLeqReducedTip _ (Just TipAtGenesis) = False
     slotLeqReducedTip s (Just (Tip tSlot _ _ )) = s <= tSlot

     isReducedTip :: Slot -> Maybe Tip -> Bool
     isReducedTip _ Nothing = False
     isReducedTip _ (Just TipAtGenesis) = False
     isReducedTip s (Just (Tip tSlot _ _ )) = s == tSlot

     updateSlot :: Slot -> Maybe Tip -> Slot
     updateSlot s Nothing = s
     updateSlot s (Just TipAtGenesis) = s
     updateSlot s (Just (Tip tSlot _ _ ))
      | s <= tSlot = tSlot
      | otherwise = s

reduceOldUtxoDb :: Tip -> BeamEffect ()
reduceOldUtxoDb TipAtGenesis = Combined []
reduceOldUtxoDb (Tip (toDbValue -> slot) _ _) = Combined
    -- Delete all the tips before or equal to 'slot'
    -- Indeed, reduceBlocks returns the first tip from which we have to perform reduction.
    [ DeleteRows $ delete (tipRows db) (\row -> _tipRowSlot row <=. val_ slot)
    -- Delete all TxOut and unspent utxos for which a matching input with a slot less than or equal
    -- to reduced slot can be found
    , DeleteRowsInClause
      (utxoOutRefRows db)
      _utxoRowOutRef
      ( select
        $ fmap _unmatchedInputRowOutRef
        (filter_ (\input -> unTipRowId (_unmatchedInputRowTip input) <=. val_ slot) (all_ (unmatchedInputRows db))) )
    , DeleteRowsInClause
      (unspentOutputRows db)
      _unspentOutputRowOutRef
      -- no need to check that _unspentOutputRowTip is less than slot
      -- as if an entry in unmatchedInput is found we know for sure that the utxo
      -- was produced before it was consumed.
      ( select
        $ fmap _unmatchedInputRowOutRef
        (filter_  (\input -> unTipRowId (_unmatchedInputRowTip input) <=. val_ slot) (all_ (unmatchedInputRows db))) )
    -- We can now safely delete all spent utxo with a slot less than or equal to reduced slot
    -- Indeed, if a utxo is spent then it has certainly been produced within
    -- the same block or even before.
    -- Note also that the delete trigger has to be removed as no more required.
    , DeleteRows $ delete
        (unmatchedInputRows db)
        (\input -> unTipRowId (_unmatchedInputRowTip input) <=. val_ slot)
    ]

rollbackUtxoDb :: Point -> BeamEffect ()
rollbackUtxoDb PointAtGenesis = DeleteRows $ delete (tipRows db) (const (val_ True))
rollbackUtxoDb (Point (toDbValue -> slot) _) = Combined
    [ DeleteRows $ delete (tipRows db) (\row -> _tipRowSlot row >. val_ slot)
    , DeleteRowsInClause
      (utxoOutRefRows db)
       _utxoRowOutRef
      ( select
        $ fmap _unspentOutputRowOutRef
        (filter_ (\output -> unTipRowId (_unspentOutputRowTip output) >. val_ slot) (all_ (unspentOutputRows db))) )
    , DeleteRows $ delete (unspentOutputRows db) (\row -> unTipRowId (_unspentOutputRowTip row) >. val_ slot)
    , DeleteRows $ delete (unmatchedInputRows db) (\row -> unTipRowId (_unmatchedInputRowTip row) >. val_ slot)
    ]

restoreStateFromDb :: forall effs. (Member BeamEffect effs) => Eff effs ChainIndexState
restoreStateFromDb = do
  -- tips are kept in descending order, i.e., newest tip first
  tips <- selectList . select . orderBy_ (desc_ . _tipRowSlot) . all_ $ tipRows db
  pure $ fmap (fromDbValue . Just) tips




data InsertRows te where
    InsertRows :: BeamableSqlite t => [t Identity] -> InsertRows (TableEntity t)

instance Semigroup (InsertRows te) where
    InsertRows l <> InsertRows r = InsertRows (l <> r)
instance BeamableSqlite t => Monoid (InsertRows (TableEntity t)) where
    mempty = InsertRows []


insertRows :: Db InsertRows -> BeamEffect ()
insertRows = getConst . zipTables Proxy (\tbl (InsertRows rows) -> Const $ AddRowsPreparedStatement tbl rows) db


credential :: (ChainIndex.ChainIndexTxOut, TxOutRef) -> (Credential, TxOutRef, Maybe DatumHash)
credential (ChainIndexTxOut{citoAddress=Address{addressCredential},citoDatum}, ref) =
  (addressCredential, ref, getHashFromDatum citoDatum)


updateMapWithInlineDatum :: Map.Map DatumHash Datum -> [ChainIndex.ChainIndexTxOut] -> Map.Map DatumHash Datum
updateMapWithInlineDatum witness [] = witness
updateMapWithInlineDatum witness ((ChainIndexTxOut{citoDatum=OutputDatum d}) : tl) =
  updateMapWithInlineDatum (Map.insert (datumHash d) d witness) tl
updateMapWithInlineDatum witness (_ : tl) = updateMapWithInlineDatum witness tl

getHashFromDatum :: OutputDatum -> Maybe DatumHash
getHashFromDatum NoOutputDatum        = Nothing
getHashFromDatum (OutputDatumHash dh) = Just dh
getHashFromDatum (OutputDatum d)      = Just (datumHash d)
-- note that the datum hash for inline datum is implicitly added in datumRows


fromTx :: ChainIndexInternalTx -> Db InsertRows
fromTx tx =
  mempty
    { datumRows = InsertRows . fmap toDbValue $ (Map.toList $ updateMapWithInlineDatum (_citxData tx') (txOuts tx'))
    , scriptRows = InsertRows . fmap toDbValue $ (Map.toList $  ciitxScripts tx)
    , redeemerRows = InsertRows . fmap toDbValue $ (Map.toList $ txRedeemersWithHash tx')
    , txRows = InsertRows [toDbValue (_citxTxId tx', tx')]
    , addressRows = InsertRows . fmap toDbValue $ (fmap credential utxos)
    }
  where
    tx' = CAPI.toChainIndexTxEmptyScripts tx
    !utxos = txOutsWithRef tx'


diagnostics :: ( Member BeamEffect effs ) => Eff effs Diagnostics
diagnostics = do
    numTransactions <- selectOne . select $ aggregate_ (const countAll_) (all_ (txRows db))
    txIds <- queryList . select $ _txRowTxId <$> limit_ 10 (all_ (txRows db))
    unspentTxOuts <- queryList . select $ _utxoRowTxOut <$> limit_ 10 (all_ (utxoOutRefRows db))
    numScripts <- selectOne . select $ aggregate_ (const countAll_) (all_ (scriptRows db))
    numAddresses <- selectOne . select $ aggregate_ (const countAll_) $ nub_ $ _addressRowCred <$> all_ (addressRows db)
    numOutputs <- selectOne . select $ aggregate_ (const countAll_) (all_ (unspentOutputRows db))
    numInputs <- selectOne . select $ aggregate_ (const countAll_) (all_ (unmatchedInputRows db))

    pure $ Diagnostics
        { numTransactions    = fromMaybe (-1) numTransactions
        , numScripts         = fromMaybe (-1) numScripts
        , numAddresses       = fromMaybe (-1) numAddresses
        , numAssetClasses    = -1
        , numUnspentOutputs  = fromMaybe (-1) numOutputs
        , numUnmatchedInputs = fromMaybe (-1) numInputs
        , someTransactions   = txIds
        , unspentTxOuts = unspentTxOuts
        }
