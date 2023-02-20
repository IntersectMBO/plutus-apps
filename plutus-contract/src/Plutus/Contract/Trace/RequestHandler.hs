{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Plutus.Contract.Trace.RequestHandler(
    RequestHandler(..)
    , RequestHandlerLogMsg(..)
    , tryHandler
    , tryHandler'
    , wrapHandler
    , extract
    , maybeToHandler
    , generalise
    -- * handlers for common requests
    , handleAdjustUnbalancedTx
    , handleOwnAddresses
    , handleSlotNotifications
    , handleCurrentNodeClientSlot
    , handleCurrentChainIndexSlot
    , handleTimeNotifications
    , handleCurrentTime
    , handleCurrentNodeClientTimeRange
    , handleTimeToSlotConversions
    , handleUnbalancedTransactions
    , handlePendingTransactions
    , handleChainIndexQueries
    , handleOwnInstanceIdQueries
    , handleYieldedUnbalancedTx
    , handleGetParams
    ) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Arrow (Arrow, Kleisli (Kleisli))
import Control.Category (Category)
import Control.Lens (Prism', Profunctor, preview)
import Control.Monad (foldM, guard, join)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error qualified as Eff
import Control.Monad.Freer.NonDet (NonDet)
import Control.Monad.Freer.NonDet qualified as NonDet
import Control.Monad.Freer.Reader (Reader, ask)
import Data.Monoid (Alt (Alt), Ap (Ap))
import Data.Text (Text)
import Data.Traversable (forM)
import Plutus.Contract.Resumable (Request (Request, itID, rqID, rqRequest),
                                  Response (Response, rspItID, rspResponse, rspRqID))

import Cardano.Node.Emulator.Params (Params (..))
import Cardano.Node.Emulator.TimeSlot qualified as TimeSlot
import Control.Monad.Freer.Extras.Log (LogMessage, LogMsg, LogObserve, logDebug, logWarn, surroundDebug)
import Data.List.NonEmpty (NonEmpty)
import Ledger (CardanoAddress, POSIXTime, POSIXTimeRange, Slot (..), SlotRange)
import Ledger.Tx (CardanoTx)
import Ledger.Tx.CardanoAPI (ToCardanoError)
import Ledger.Tx.Constraints (UnbalancedTx)
import Ledger.Tx.Constraints qualified as Constraints
import Plutus.ChainIndex (ChainIndexQueryEffect)
import Plutus.ChainIndex.Effects qualified as ChainIndexEff
import Plutus.ChainIndex.Types (Tip (..))
import Plutus.Contract.Effects (ChainIndexQuery (..), ChainIndexResponse (..))
import Wallet.API (WalletAPIError, signTxAndSubmit)
import Wallet.Effects (NodeClientEffect, WalletEffect, getClientParams, getClientSlot)
import Wallet.Effects qualified
import Wallet.Emulator.LogMessages (RequestHandlerLogMsg (AdjustingUnbalancedTx, HandleTxFailed, SlotNoticationTargetVsCurrent))
import Wallet.Types (ContractInstanceId)

-- | Request handlers that can choose whether to handle an effect (using
--   'Alternative'). This is useful if 'req' is a sum type.
newtype RequestHandler effs req resp = RequestHandler { unRequestHandler :: req -> Eff (NonDet ': effs) resp }
    deriving stock (Functor)
    deriving (Profunctor, Category, Arrow) via (Kleisli (Eff (NonDet ': effs)))
    deriving (Semigroup, Monoid) via (Ap ((->) req) (Alt (Eff (NonDet ': effs)) resp))

-- Try the handler on the requests until it succeeds for the first time, then stop.
tryHandler ::
    forall effs req resp
    . RequestHandler effs req resp
    -> [req]
    -> Eff effs (Maybe resp)
tryHandler handler = tryHandler' (Just <$> handler)

-- Try the handler on the requests, using the 'Alternative' instance of @f@
tryHandler' ::
    forall f effs req resp
    . (Alternative f, Monad f)
    => RequestHandler effs req (f resp)
    -> [req]
    -> Eff effs (f resp)
tryHandler' (RequestHandler h) =
    foldM (\e i -> fmap ((e <|>) . join) $ NonDet.makeChoiceA @f $ h i) empty

extract :: Alternative f => Prism' a b -> a -> f b
extract p = maybe empty pure . preview p

-- | Generalise a request handler
generalise ::
    forall effs req req' resp resp'
    . (req' -> Maybe req)
    -> (resp -> resp')
    -> RequestHandler effs req resp
    -> RequestHandler effs req' resp'
generalise rq rsp (RequestHandler h) = RequestHandler $ \k -> do
    case rq k of
        Nothing -> empty
        Just k' -> rsp <$> h k'

wrapHandler :: RequestHandler effs req resp -> RequestHandler effs (Request req) (Response resp)
wrapHandler (RequestHandler h) = RequestHandler $ \Request{rqID, itID, rqRequest} -> do
    r <- h rqRequest
    pure $ Response{rspRqID = rqID, rspResponse = r, rspItID = itID }

maybeToHandler :: (req -> Maybe resp) -> RequestHandler effs req resp
maybeToHandler f = RequestHandler $ maybe empty pure . f

-- handlers for common requests

handleOwnAddresses ::
    forall a effs.
    ( Member WalletEffect effs
    , Member (LogObserve (LogMessage Text)) effs
    )
    => RequestHandler effs a (NonEmpty CardanoAddress)
handleOwnAddresses =
    RequestHandler $ \_ ->
        surroundDebug @Text "handleOwnAddresses" Wallet.Effects.ownAddresses

handleSlotNotifications ::
    forall effs.
    ( Member NodeClientEffect effs
    , Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    )
    => RequestHandler effs Slot Slot
handleSlotNotifications =
    RequestHandler $ \targetSlot_ ->
        surroundDebug @Text "handleSlotNotifications" $ do
            currentSlot <- getClientSlot
            logDebug $ SlotNoticationTargetVsCurrent targetSlot_ currentSlot
            guard (currentSlot >= targetSlot_)
            pure currentSlot

handleTimeNotifications ::
    forall effs.
    ( Member NodeClientEffect effs
    , Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    )
    => RequestHandler effs POSIXTime POSIXTime
handleTimeNotifications =
    RequestHandler $ \targetTime_ ->
        surroundDebug @Text "handleTimeNotifications" $ do
            currentSlot <- getClientSlot
            Params { pSlotConfig } <- getClientParams
            let targetSlot_ = TimeSlot.posixTimeToEnclosingSlot pSlotConfig targetTime_
            logDebug $ SlotNoticationTargetVsCurrent targetSlot_ currentSlot
            guard (currentSlot >= targetSlot_)
            pure $ TimeSlot.slotToEndPOSIXTime pSlotConfig currentSlot

handleCurrentNodeClientSlot ::
    forall effs a.
    ( Member NodeClientEffect effs
    , Member (LogObserve (LogMessage Text)) effs
    )
    => RequestHandler effs a Slot
handleCurrentNodeClientSlot =
    RequestHandler $ \_ ->
        surroundDebug @Text "handleCurrentNodeClientSlot" $ do
            getClientSlot

handleCurrentChainIndexSlot ::
    forall effs a.
    ( Member (LogObserve (LogMessage Text)) effs
    , Member ChainIndexQueryEffect effs
    )
    => RequestHandler effs a Slot
handleCurrentChainIndexSlot =
    RequestHandler $ \_ ->
        surroundDebug @Text "handleCurrentChainIndexSlot" $ do
            t <- ChainIndexEff.getTip
            case t of
                TipAtGenesis   -> return $ Slot 0
                (Tip slot _ _) -> return slot

handleCurrentTime ::
    forall effs a.
    ( Member NodeClientEffect effs
    , Member (LogObserve (LogMessage Text)) effs
    )
    => RequestHandler effs a POSIXTime
handleCurrentTime =
    RequestHandler $ \_ ->
        surroundDebug @Text "handleCurrentTime" $ do
            Params { pSlotConfig }  <- getClientParams
            TimeSlot.slotToEndPOSIXTime pSlotConfig <$> getClientSlot

handleCurrentNodeClientTimeRange ::
    forall effs a.
    ( Member NodeClientEffect effs
    , Member (LogObserve (LogMessage Text)) effs
    )
    => RequestHandler effs a (POSIXTime, POSIXTime)
handleCurrentNodeClientTimeRange =
    RequestHandler $ \_ ->
        surroundDebug @Text "handleCurrentNodeClientTimeRange" $ do
            Params { pSlotConfig }  <- getClientParams
            nodeClientSlot <- getClientSlot
            pure ( TimeSlot.slotToBeginPOSIXTime pSlotConfig nodeClientSlot
                 , TimeSlot.slotToEndPOSIXTime pSlotConfig nodeClientSlot
                 )

handleTimeToSlotConversions ::
    forall effs.
    ( Member NodeClientEffect effs
    , Member (LogObserve (LogMessage Text)) effs
    )
    => RequestHandler effs POSIXTimeRange SlotRange
handleTimeToSlotConversions =
    RequestHandler $ \poxisTimeRange ->
        surroundDebug @Text "handleTimeToSlotConversions" $ do
            Params { pSlotConfig }  <- getClientParams
            pure $ TimeSlot.posixTimeRangeToContainedSlotRange pSlotConfig poxisTimeRange

handleUnbalancedTransactions ::
    forall effs.
    ( Member WalletEffect effs
    , Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    )
    => RequestHandler effs UnbalancedTx (Either WalletAPIError CardanoTx)
handleUnbalancedTransactions =
    RequestHandler $ \unbalancedTx ->
        surroundDebug @Text "handleUnbalancedTransactions" $ do
        Wallet.Effects.balanceTx unbalancedTx `Eff.handleError`
          (\err -> logWarn (HandleTxFailed err) >> pure (Left err))

handlePendingTransactions ::
    forall effs.
    ( Member WalletEffect effs
    , Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    )
    => RequestHandler effs CardanoTx (Either WalletAPIError CardanoTx)
handlePendingTransactions =
    RequestHandler $ \tx ->
        surroundDebug @Text "handlePendingTransactions" $ do
        Eff.handleError (Right <$> signTxAndSubmit tx)
                        (\err -> logWarn (HandleTxFailed err) >> pure (Left err))

handleChainIndexQueries ::
    forall effs.
    ( Member (LogObserve (LogMessage Text)) effs
    , Member ChainIndexQueryEffect effs
    )
    => RequestHandler effs ChainIndexQuery ChainIndexResponse
handleChainIndexQueries = RequestHandler $ \chainIndexQuery ->
    surroundDebug @Text "handleChainIndexQueries" $ do
      case chainIndexQuery of
        DatumFromHash h               -> DatumHashResponse <$> ChainIndexEff.datumFromHash h
        ValidatorFromHash h           -> ValidatorHashResponse <$> ChainIndexEff.validatorFromHash h
        MintingPolicyFromHash h       -> MintingPolicyHashResponse <$> ChainIndexEff.mintingPolicyFromHash h
        StakeValidatorFromHash h      -> StakeValidatorHashResponse <$> ChainIndexEff.stakeValidatorFromHash h
        RedeemerFromHash h            -> RedeemerHashResponse <$> ChainIndexEff.redeemerFromHash h
        TxOutFromRef txOutRef         -> TxOutRefResponse <$> ChainIndexEff.txOutFromRef txOutRef
        TxFromTxId txid               -> TxIdResponse <$> ChainIndexEff.txFromTxId txid
        UnspentTxOutFromRef ref       -> UnspentTxOutResponse <$> ChainIndexEff.unspentTxOutFromRef ref
        UtxoSetMembership txOutRef    -> UtxoSetMembershipResponse <$> ChainIndexEff.utxoSetMembership txOutRef
        UtxoSetAtAddress pq c         -> UtxoSetAtResponse <$> ChainIndexEff.utxoSetAtAddress pq c
        UnspentTxOutSetAtAddress pq c -> UnspentTxOutsAtResponse <$> ChainIndexEff.unspentTxOutSetAtAddress pq c
        DatumsAtAddress pq c          -> DatumsAtResponse <$> ChainIndexEff.datumsAtAddress pq c
        UtxoSetWithCurrency pq ac     -> UtxoSetWithCurrencyResponse <$> ChainIndexEff.utxoSetWithCurrency pq ac
        TxoSetAtAddress pq c          -> TxoSetAtResponse <$> ChainIndexEff.txoSetAtAddress pq c
        TxsFromTxIds txids            -> TxIdsResponse <$> ChainIndexEff.txsFromTxIds txids
        GetTip                        -> GetTipResponse <$> ChainIndexEff.getTip

handleOwnInstanceIdQueries ::
    forall effs a.
    ( Member (LogObserve (LogMessage Text)) effs
    , Member (Reader ContractInstanceId) effs
    )
    => RequestHandler effs a ContractInstanceId
handleOwnInstanceIdQueries = RequestHandler $ \_ ->
    surroundDebug @Text "handleOwnInstanceIdQueries" ask

handleYieldedUnbalancedTx ::
    forall effs.
    ( Member WalletEffect effs
    , Member (LogObserve (LogMessage Text)) effs
    )
    => RequestHandler effs UnbalancedTx ()
handleYieldedUnbalancedTx =
    RequestHandler $ \utx ->
        surroundDebug @Text "handleYieldedUnbalancedTx" $ do
            Wallet.Effects.yieldUnbalancedTx utx

handleAdjustUnbalancedTx ::
    forall effs.
    ( Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    , Member NodeClientEffect effs
    )
    => RequestHandler effs UnbalancedTx (Either ToCardanoError UnbalancedTx)
handleAdjustUnbalancedTx =
    RequestHandler $ \utx ->
        surroundDebug @Text "handleAdjustUnbalancedTx" $ do
            params <- getClientParams
            forM (Constraints.adjustUnbalancedTx (emulatorPParams params) utx) $ \(missingAdaCosts, adjusted) -> do
                logDebug $ AdjustingUnbalancedTx missingAdaCosts
                pure adjusted

handleGetParams ::
    forall effs.
    ( Member (LogObserve (LogMessage Text)) effs
    , Member NodeClientEffect effs
    )
    => RequestHandler effs () Params
handleGetParams =
    RequestHandler $ \_ ->
        surroundDebug @Text "handleGetParams" $ do
            getClientParams
