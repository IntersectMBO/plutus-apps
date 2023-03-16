{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Plutus.Contract.Marconi.Handler where

import Cardano.Api (AddressInEra (AddressInEra), AddressTypeInEra (..), ChainPoint, TxIx (TxIx), toAddressAny)
import Control.Concurrent (MVar, putMVar, readMVar)
import Control.Lens (views, (^.))
import Control.Monad.Freer (Eff, LastMember, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Extras (raiseMUnderN)
import Control.Monad.Freer.Extras.Pagination (PageQuery, pageOf)
import Control.Monad.Freer.Reader (Reader, ask, runReader)
import Control.Monad.Freer.TH (makeEffect)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Ledger.Address (CardanoAddress)
import Ledger.Tx.CardanoAPI.Internal (fromCardanoTxId)
import Marconi.ChainIndex.Indexers.Utxo (StorableQuery (UtxoAddress), UtxoHandle, UtxoIndexer, getUtxoResult, txId,
                                         txIx, urUtxo)
import Marconi.Core.Storable (HasPoint, QueryInterval (QEverything), Queryable, Rewindable, State, StorableEvent,
                              StorableMonad, StorablePoint, StorableResult, query)
import Marconi.Core.Storable qualified as Storable (rewind)
import Plutus.ChainIndex.Api (UtxosResponse (UtxosResponse))
import Plutus.ChainIndex.ChainIndexError (ChainIndexError)
import Plutus.ChainIndex.Compatibility (toCardanoPoint)
import Plutus.ChainIndex.Effects (ChainIndexControlEffect (..), ChainIndexQueryEffect (UtxoSetAtAddress))
import Plutus.ChainIndex.Types (ChainSyncBlock, Point, Tip (..))
import Plutus.V2.Ledger.Api (TxOutRef (TxOutRef))

data MarconiEffect handle r where
  QueryIndexer :: StorableQuery handle -> MarconiEffect handle (StorableResult handle)
  Rewind :: Point -> MarconiEffect handle ()
  Index  :: ChainSyncBlock -> MarconiEffect handle ()
  Resume  :: Point -> MarconiEffect handle ()


makeEffect ''MarconiEffect


handleMarconi ::
    ( LastMember IO effs
    , Member (Reader (State handle)) effs
    , StorableMonad handle ~ IO
    , StorablePoint handle ~ ChainPoint
    , HasPoint (StorableEvent handle) (StorablePoint handle)
    , Ord (StorablePoint handle)
    , Queryable handle
    , Rewindable handle
    )
    => (ChainSyncBlock -> [StorableEvent handle]) -> MarconiEffect handle ~> Eff effs
handleMarconi _ (QueryIndexer q) = do
    st <- ask
    -- At the moment we query everything, we may need to find the latest common sync point in the future
    liftIO $ query QEverything st q
handleMarconi _f (Rewind point) = do
    st <- ask
    ix <- liftIO $ readMVar st
    mix <- liftIO $ Storable.rewind (fromJust $ toCardanoPoint point) ix
    maybe
        (throwError  _a)
        (liftIO $ writeTMVar st ix)
        mix
handleMarconi _f _others = undefined

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

handleQuery ::
    ( Member (MarconiEffect UtxoHandle) effs
    , Member (Error ChainIndexError) effs
    ) => ChainIndexQueryEffect
    ~> Eff effs
handleQuery = \case
   UtxoSetAtAddress pageQuery addr -> getUtxoSetAtAddress pageQuery addr
   _eff                            -> throwError @ChainIndexError undefined

handleControl ::
    forall effs.
    ( Member (Error ChainIndexError) effs
    )
    => ChainIndexControlEffect
    ~> Eff effs
handleControl = \case
    _eff -> throwError @ChainIndexError undefined

-- | Handle the chain index effects from the set of all effects.
handleChainIndexEffects
    :: (LastMember IO effs, StorableMonad UtxoIndexer ~ IO)
    => MVar UtxoIndexer
    -> Eff (ChainIndexQueryEffect ': ChainIndexControlEffect ': MarconiEffect UtxoHandle ': effs) a
    -> Eff effs (Either ChainIndexError a)
handleChainIndexEffects mutxos action = do
    utxosIndexer <- liftIO $ readMVar mutxos
    result <- runReader utxosIndexer
        $ runError @ChainIndexError
        $ interpret (handleMarconi @_ @UtxoHandle)
        $ interpret handleControl
        $ interpret handleQuery
        $ raiseMUnderN @[_,_] @[_,_,_] action
    liftIO $ putMVar mutxos utxosIndexer
    pure result
