{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Plutus.Contract.Marconi.Handler where

import Cardano.Api (AddressInEra (AddressInEra), AddressTypeInEra (..), ChainPoint (..), SlotNo (SlotNo), TxIx (TxIx),
                    toAddressAny)
import Cardano.Api qualified as C
import Control.Concurrent (MVar, putMVar, readMVar)
import Control.Lens (Lens', _1, folded, makeLenses, views, (&), (.~), (^.), (^..))
import Control.Monad.Freer (Eff, LastMember, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Extras (raiseMUnderN)
import Control.Monad.Freer.Extras.Pagination (PageQuery, pageOf)
import Control.Monad.Freer.State qualified as Eff (State, get, put, runState)
import Control.Monad.Freer.TH (makeEffect)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Ledger (CardanoTx (CardanoTx))
import Ledger.Address (CardanoAddress)
import Ledger.Tx.CardanoAPI.Internal (fromCardanoTxId)
import Marconi.ChainIndex.Indexers.Utxo (StorableEvent (UtxoEvent), StorableQuery (UtxoAddress), Utxo (..), UtxoHandle,
                                         getInputs, getUtxoResult, getUtxos, txId, txIx, urUtxo)
import Marconi.Core.Storable (HasPoint, QueryInterval (QEverything), Queryable, State, StorableMonad, StorablePoint,
                              StorableResult, insertMany, query)
import Plutus.ChainIndex.Api (UtxosResponse (UtxosResponse))
import Plutus.ChainIndex.ChainIndexError (ChainIndexError)
import Plutus.ChainIndex.Compatibility (toCardanoBlockId)
import Plutus.ChainIndex.Effects (ChainIndexControlEffect (AppendBlocks), ChainIndexQueryEffect (UtxoSetAtAddress))
import Plutus.ChainIndex.Types (ChainSyncBlock (..), Point (Point, PointAtGenesis), Tip (TipAtGenesis), citxCardanoTx,
                                tipAsPoint)
import Plutus.V2.Ledger.Api (TxOutRef (TxOutRef))

data ChainIndexIndexers -- We don't use `newtype` since other indexers will be needed
   = ChainIndexIndexers
       { _utxosIndexer :: State UtxoHandle
       }

makeLenses ''ChainIndexIndexers

data ChainIndexIndexersMVar -- We don't use `newtype` since other indexers will be needed
   = ChainIndexIndexersMVar
       { _utxosIndexerMVar :: MVar (State UtxoHandle)
       }

makeLenses ''ChainIndexIndexersMVar

getChainIndexIndexers :: ChainIndexIndexersMVar -> IO ChainIndexIndexers
getChainIndexIndexers mvarCi =
    ChainIndexIndexers <$> readMVar (mvarCi ^. utxosIndexerMVar)

putChainIndexIndexers :: ChainIndexIndexers -> ChainIndexIndexersMVar -> IO ()
putChainIndexIndexers ci mvarCi = do
    putMVar (mvarCi ^. utxosIndexerMVar) (ci ^. utxosIndexer)



data MarconiEffect handle r where
  QueryIndexer :: StorableQuery handle -> MarconiEffect handle (StorableResult handle)

makeEffect ''MarconiEffect

handleMarconiQuery ::
    ( LastMember IO effs
    , Member (Eff.State ChainIndexIndexers) effs
    , StorableMonad handle ~ IO
    , HasPoint (StorableEvent handle) (StorablePoint handle)
    , Ord (StorablePoint handle)
    , Queryable handle
    )
    => Lens' ChainIndexIndexers (State handle) -> MarconiEffect handle ~> Eff effs
handleMarconiQuery l (QueryIndexer q) = do
  ci <- Eff.get
  liftIO $ query QEverything (ci ^. l) q

getUtxoSetAtAddress
  :: forall effs.
    ( Member (MarconiEffect UtxoHandle) effs
    )
  => PageQuery TxOutRef
  -> CardanoAddress
  -> Eff effs UtxosResponse
getUtxoSetAtAddress pageQuery addrInEra = let
    toTxOutRef utxo = TxOutRef
                          (fromCardanoTxId $ utxo ^. txId)
                          (toInteger . (\(TxIx x) -> x) $ utxo ^. txIx)
    addr = case addrInEra of
                AddressInEra ByronAddressInAnyEra addr'    -> toAddressAny addr'
                AddressInEra (ShelleyAddressInEra _) addr' -> toAddressAny addr'
    in UtxosResponse TipAtGenesis
           . pageOf pageQuery
           . Set.fromList
           . fmap (views urUtxo toTxOutRef)
           . getUtxoResult
           <$> queryIndexer (UtxoAddress addr)

utxosFromCardanoTx :: CardanoTx -> [Utxo]
utxosFromCardanoTx (CardanoTx c _) = getUtxos Nothing c


inputsFromCardanoTx :: CardanoTx -> Set C.TxIn
inputsFromCardanoTx (CardanoTx c _) = getInputs c


getUtxoEvents
  :: [CardanoTx]
  -> C.ChainPoint
  -> StorableEvent UtxoHandle -- ^ UtxoEvents are stored in storage after conversion to UtxoRow
getUtxoEvents txs cp =
  let

    utxos = Set.fromList $ concatMap utxosFromCardanoTx txs
    ins = foldl' Set.union Set.empty $ inputsFromCardanoTx <$> txs
  in UtxoEvent utxos ins cp

handleControl ::
    ( LastMember IO effs
    , Member (Eff.State ChainIndexIndexers) effs
    , Member (Error ChainIndexError) effs
    ) =>
    ChainIndexControlEffect ~> Eff effs
handleControl = \case
    AppendBlocks xs -> do
        ci <- Eff.get
        utxosIndexer' <- liftIO $ insertMany (extractUtxosEvent <$> xs) (ci ^. utxosIndexer)
        Eff.put (ci & utxosIndexer .~ utxosIndexer')
    _other -> throwError @ChainIndexError undefined
    where
        toCardanoPoint PointAtGenesis = ChainPointAtGenesis
        toCardanoPoint (Point slot blockId) =
            ChainPoint (SlotNo (fromIntegral slot)) (fromJust $ toCardanoBlockId blockId)
        extractUtxosEvent Block{blockTip,blockTxs} = let
            point = toCardanoPoint $ tipAsPoint blockTip
            in getUtxoEvents
                (blockTxs ^.. folded . _1 . citxCardanoTx . folded)
                point

handleQuery ::
    ( Member (MarconiEffect UtxoHandle) effs
    , Member (Error ChainIndexError) effs
    ) => ChainIndexQueryEffect
    ~> Eff effs
handleQuery = \case
   UtxoSetAtAddress pageQuery addr -> getUtxoSetAtAddress pageQuery addr
   _eff                            -> throwError @ChainIndexError undefined

-- | Handle the chain index effects from the set of all effects.
handleChainIndexEffects
    :: LastMember IO effs
    => ChainIndexIndexersMVar
    -> Eff (ChainIndexQueryEffect ': ChainIndexControlEffect ': MarconiEffect UtxoHandle ': effs) a
    -> Eff effs (Either ChainIndexError a)
handleChainIndexEffects mIndexers action = do
    indexers <- liftIO $ getChainIndexIndexers mIndexers
    (result, indexers') <- Eff.runState indexers
        $ runError @ChainIndexError
        $ interpret (handleMarconiQuery utxosIndexer)
        $ interpret handleControl
        $ interpret handleQuery
        $ raiseMUnderN @[_,_] @[_,_,_] action
    liftIO $ putChainIndexIndexers indexers' mIndexers
    pure result
