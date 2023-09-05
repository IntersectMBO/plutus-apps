{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Plutus.ChainIndex.Marconi where

import Cardano.Api (AddressInEra (AddressInEra), AddressTypeInEra (..), TxIx (TxIx), toAddressAny)
import Cardano.Api qualified as C
import Cardano.BM.Trace (Trace)
import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Control.Lens (Lens', _1, folded, makeLenses, views, (&), (.~), (^.), (^..))
import Control.Monad.Freer (Eff, LastMember, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Extras
import Control.Monad.Freer.Extras.Pagination (PageQuery, pageOf)
import Control.Monad.Freer.Reader (Reader, ask, runReader)
import Control.Monad.Freer.State qualified as Eff (State, get, put, runState)
import Control.Monad.Freer.TH (makeEffect)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (foldl')
import Data.Map (elems)
import Data.Set qualified as Set
import Ledger.Address (CardanoAddress)
import Ledger.Tx (CardanoTx (CardanoTx))
import Marconi.ChainIndex.Indexers.Utxo (StorableEvent (UtxoEvent), StorableQuery (UtxoByAddress), UtxoHandle,
                                         getInputs, getUtxoResult, getUtxosFromTxBody, txId, txIx, urUtxo)
import Marconi.Core.Storable (HasPoint, QueryInterval (QEverything), Queryable, State, StorableMonad, StorablePoint,
                              StorableResult, insertMany, query)
import Plutus.ChainIndex.Api (UtxosResponse (UtxosResponse))
import Plutus.ChainIndex.ChainIndexError (ChainIndexError (..))
import Plutus.ChainIndex.ChainIndexLog (ChainIndexLog)
import Plutus.ChainIndex.Compatibility (toCardanoPoint)
import Plutus.ChainIndex.Effects (ChainIndexControlEffect (AppendBlocks), ChainIndexQueryEffect (UtxoSetAtAddress))
import Plutus.ChainIndex.Types (ChainSyncBlock (..), Tip (TipAtGenesis), citxCardanoTx, tipAsPoint)
import Plutus.Contract.CardanoAPI (fromCardanoTxId)
import Plutus.Monitoring.Util (PrettyObject (PrettyObject), convertLog, runLogEffects)
import Plutus.V2.Ledger.Api (TxOutRef (TxOutRef))

{- Handling ChainIndexEffects with Marconi - Developer notes:

    The general idea is to transform `ChainIndexQueryEffect` into a @MarconiEffect@ for an handler,
    and to resolve these effects afterwards with a bunch of calls to `handleMarconiQuery`.

    The  main reason for this is that we mutualise the code that queries the handlers (see @handleMarconiQuery@) and we have a
    uniform way to query the different indexers.

    And a minor one:

        * There's no need of different handlers for @MarconiEffect@ at this stage but we leave the door open to it.
          A possible use case might be to provide a simpler indexer backend for the emulator.

    How to add new indexer to support new effect?

        1. add the indexer MVar to `ChainIndexIndexersMvar` and the corresponding indexer to `ChainIndexIndexers`
        2. edit `getChainIndexIndexers` and `putChainIndexIndexers` accordingly
        3. handle the appropriate queries of `ChainIndexQueries` in `handleQuery`
        4. Add the indexer update in the control operations
-}

data ChainIndexIndexers -- We don't use `newtype` since other indexers will be needed
   = ChainIndexIndexers
       { _utxosIndexer :: State UtxoHandle
       }

makeLenses ''ChainIndexIndexers

data ChainIndexIndexersMVar -- We don't use `newtype` since other indexers will be needed
   = ChainIndexIndexersMVar
       { _utxosIndexerMVar :: MVar (State UtxoHandle)
       }

boxChainIndexIndexers :: ChainIndexIndexers -> IO ChainIndexIndexersMVar
boxChainIndexIndexers ci =
  ChainIndexIndexersMVar <$>
       (newMVar $ ci ^. utxosIndexer)

makeLenses ''ChainIndexIndexersMVar

getChainIndexIndexers :: ChainIndexIndexersMVar -> IO ChainIndexIndexers
getChainIndexIndexers mvarCi =
    ChainIndexIndexers <$> takeMVar (mvarCi ^. utxosIndexerMVar)

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
           <$> queryIndexer (UtxoByAddress addr Nothing)


getUtxoEvents
  :: [CardanoTx]
  -> C.ChainPoint
  -> StorableEvent UtxoHandle -- ^ UtxoEvents are stored in storage after conversion to UtxoRow
getUtxoEvents txs cp =
  let utxosFromCardanoTx (CardanoTx (C.Tx txBody _) _) = elems $ getUtxosFromTxBody Nothing txBody
      inputsFromCardanoTx (CardanoTx (C.Tx txBody _) _) = getInputs txBody
      utxos = Set.fromList $ concatMap utxosFromCardanoTx txs
      ins = foldl' Set.union Set.empty $ inputsFromCardanoTx <$> txs
  in UtxoEvent utxos ins cp

-- | The required arguments to run the chain index effects.
data RunRequirements = RunRequirements
    { trace    :: !(Trace IO (PrettyObject ChainIndexLog))
    , indexers :: !ChainIndexIndexersMVar
    }


-- | Run the chain index effects.
runChainIndexEffects
    :: RunRequirements
    -> Eff '[ChainIndexQueryEffect, ChainIndexControlEffect] a
    -> IO (Either ChainIndexError a)
runChainIndexEffects runReq action =
    runLogEffects (convertLog PrettyObject $ trace runReq)
        $ runReader (indexers runReq)
        $ handleChainIndexEffects
        $ raiseEnd action

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
    _other -> throwError UnsupportedControlOperation
    where
        extractUtxosEvent Block{blockTip,blockTxs} = let
            point = toCardanoPoint $ tipAsPoint blockTip
            in getUtxoEvents
                (blockTxs ^.. folded . _1 . citxCardanoTx . folded)
                point

handleQuery ::
    ( LastMember IO effs
    , Member (Eff.State ChainIndexIndexers) effs
    , Member (Error ChainIndexError) effs
    ) => ChainIndexQueryEffect
    ~> Eff effs
handleQuery = \case
    UtxoSetAtAddress pageQuery addr ->
        interpret (handleMarconiQuery utxosIndexer) $ getUtxoSetAtAddress pageQuery addr
    _eff -> throwError UnsupportedQuery

-- | Handle the chain index effects from the set of all effects.
handleChainIndexEffects ::
    ( LastMember IO effs
    , Member (Reader ChainIndexIndexersMVar) effs
    )
    => Eff (ChainIndexQueryEffect ': ChainIndexControlEffect ': effs) a
    -> Eff effs (Either ChainIndexError a)
handleChainIndexEffects action = do
    mIndexers <- ask
    indexers <- liftIO $ getChainIndexIndexers mIndexers
    (result, indexers') <- Eff.runState indexers
        $ runError @ChainIndexError
        $ interpret handleControl
        $ interpret handleQuery
        $ raiseMUnderN @[_,_] @[_,_] action
    liftIO $ putChainIndexIndexers indexers' mIndexers
    pure result
