{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Plutus.Contract.Marconi.Handler where

import Cardano.Api (AddressInEra (AddressInEra), AddressTypeInEra (..), TxIx (TxIx), toAddressAny)
import Control.Concurrent (MVar, putMVar, readMVar)
import Control.Lens (Lens', makeLenses, views, (&), (.~), (^.))
import Control.Monad.Freer (Eff, LastMember, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Extras (raiseMUnderN)
import Control.Monad.Freer.Extras.Pagination (PageQuery, pageOf)
import Control.Monad.Freer.State qualified as Eff (State, get, put, runState)
import Control.Monad.Freer.TH (makeEffect)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Set qualified as Set
import Ledger.Address (CardanoAddress)
import Ledger.Tx.CardanoAPI.Internal (fromCardanoTxId)
import Marconi.ChainIndex.Indexers.Utxo (StorableQuery (UtxoAddress), UtxoHandle, getUtxoResult, txId, txIx, urUtxo)
import Marconi.Core.Storable (HasPoint, QueryInterval (QEverything), Queryable, State, StorableEvent, StorableMonad,
                              StorablePoint, StorableResult, insertMany, query)
import Plutus.ChainIndex.Api (UtxosResponse (UtxosResponse))
import Plutus.ChainIndex.ChainIndexError (ChainIndexError)
import Plutus.ChainIndex.Effects (ChainIndexControlEffect (AppendBlocks), ChainIndexQueryEffect (UtxoSetAtAddress))
import Plutus.ChainIndex.Types (ChainSyncBlock, Tip (TipAtGenesis))
import Plutus.V2.Ledger.Api (TxOutRef (TxOutRef))

data ChainIndexIndexers -- We don't use `newtype` since other indexers will be needed
   = ChainIndexIndexers
       { _utxosIndexer :: State UtxoHandle
       }

makeLenses ''ChainIndexIndexers

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
        extractUtxosEvent :: ChainSyncBlock -> StorableEvent UtxoHandle
        extractUtxosEvent = undefined

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
    => MVar ChainIndexIndexers
    -> Eff (ChainIndexQueryEffect ': ChainIndexControlEffect ': MarconiEffect UtxoHandle ': effs) a
    -> Eff effs (Either ChainIndexError a)
handleChainIndexEffects mutxos action = do
    ciIndexers <- liftIO $ readMVar mutxos
    (result, ciIndexers') <- Eff.runState ciIndexers
        $ runError @ChainIndexError
        $ interpret (handleMarconiQuery utxosIndexer)
        $ interpret handleControl
        $ interpret handleQuery
        $ raiseMUnderN @[_,_] @[_,_,_] action
    liftIO $ putMVar mutxos ciIndexers'
    pure result
