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
    , handleOwnPaymentPubKeyHash
    , handleSlotNotifications
    , handleCurrentSlot
    , handleTimeNotifications
    , handleCurrentTime
    , handleTimeToSlotConversions
    , handleUnbalancedTransactions
    , handlePendingTransactions
    , handleChainIndexQueries
    , handleOwnInstanceIdQueries
    , handleYieldedUnbalancedTx
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

import Control.Monad.Freer.Extras.Log (LogMessage, LogMsg, LogObserve, logDebug, logWarn, surroundDebug)
import Ledger (POSIXTime, POSIXTimeRange, Params (..), PaymentPubKeyHash, Slot, SlotRange)
import Ledger.Constraints.OffChain (UnbalancedTx, adjustUnbalancedTx)
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Tx (CardanoTx, ToCardanoError)
import Plutus.ChainIndex (ChainIndexQueryEffect)
import Plutus.ChainIndex.Effects qualified as ChainIndexEff
import Plutus.Contract.Effects (ChainIndexQuery (..), ChainIndexResponse (..))
import Plutus.Contract.Wallet qualified as Wallet
import Wallet.API (WalletAPIError)
import Wallet.Effects (NodeClientEffect, WalletEffect)
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
tryHandler' (RequestHandler h) requests =
    foldM (\e i -> fmap (e <|>) $ fmap join $ NonDet.makeChoiceA @f $ h i) empty requests

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

handleOwnPaymentPubKeyHash ::
    forall a effs.
    ( Member WalletEffect effs
    , Member (LogObserve (LogMessage Text)) effs
    )
    => RequestHandler effs a PaymentPubKeyHash
handleOwnPaymentPubKeyHash =
    RequestHandler $ \_ ->
        surroundDebug @Text "handleOwnPaymentPubKeyHash" Wallet.Effects.ownPaymentPubKeyHash

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
            currentSlot <- Wallet.Effects.getClientSlot
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
            currentSlot <- Wallet.Effects.getClientSlot
            Params { pSlotConfig } <- Wallet.Effects.getClientParams
            let targetSlot_ = TimeSlot.posixTimeToEnclosingSlot pSlotConfig targetTime_
            logDebug $ SlotNoticationTargetVsCurrent targetSlot_ currentSlot
            guard (currentSlot >= targetSlot_)
            pure $ TimeSlot.slotToEndPOSIXTime pSlotConfig currentSlot

handleCurrentSlot ::
    forall effs a.
    ( Member NodeClientEffect effs
    , Member (LogObserve (LogMessage Text)) effs
    )
    => RequestHandler effs a Slot
handleCurrentSlot =
    RequestHandler $ \_ ->
        surroundDebug @Text "handleCurrentSlot" $ do
            Wallet.Effects.getClientSlot

handleCurrentTime ::
    forall effs a.
    ( Member NodeClientEffect effs
    , Member (LogObserve (LogMessage Text)) effs
    )
    => RequestHandler effs a POSIXTime
handleCurrentTime =
    RequestHandler $ \_ ->
        surroundDebug @Text "handleCurrentTime" $ do
            Params { pSlotConfig }  <- Wallet.Effects.getClientParams
            TimeSlot.slotToEndPOSIXTime pSlotConfig <$> Wallet.Effects.getClientSlot

handleTimeToSlotConversions ::
    forall effs.
    ( Member NodeClientEffect effs
    , Member (LogObserve (LogMessage Text)) effs
    )
    => RequestHandler effs POSIXTimeRange SlotRange
handleTimeToSlotConversions =
    RequestHandler $ \poxisTimeRange ->
        surroundDebug @Text "handleTimeToSlotConversions" $ do
            Params { pSlotConfig }  <- Wallet.Effects.getClientParams
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
        Wallet.balanceTx unbalancedTx `Eff.handleError` (\err -> logWarn (HandleTxFailed err) >> pure (Left err))

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
        Eff.handleError (Right <$> Wallet.signTxAndSubmit tx)
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
        DatumFromHash h            -> DatumHashResponse <$> ChainIndexEff.datumFromHash h
        ValidatorFromHash h        -> ValidatorHashResponse <$> ChainIndexEff.validatorFromHash h
        MintingPolicyFromHash h    -> MintingPolicyHashResponse <$> ChainIndexEff.mintingPolicyFromHash h
        StakeValidatorFromHash h   -> StakeValidatorHashResponse <$> ChainIndexEff.stakeValidatorFromHash h
        RedeemerFromHash h         -> RedeemerHashResponse <$> ChainIndexEff.redeemerFromHash h
        TxOutFromRef txOutRef      -> TxOutRefResponse <$> ChainIndexEff.txOutFromRef txOutRef
        TxFromTxId txid            -> TxIdResponse <$> ChainIndexEff.txFromTxId txid
        UnspentTxOutFromRef ref    -> UnspentTxOutResponse <$> ChainIndexEff.unspentTxOutFromRef ref
        UtxoSetMembership txOutRef -> UtxoSetMembershipResponse <$> ChainIndexEff.utxoSetMembership txOutRef
        UtxoSetAtAddress pq c      -> UtxoSetAtResponse <$> ChainIndexEff.utxoSetAtAddress pq c
        UtxoSetWithCurrency pq ac  -> UtxoSetWithCurrencyResponse <$> ChainIndexEff.utxoSetWithCurrency pq ac
        TxoSetAtAddress pq c       -> TxoSetAtResponse <$> ChainIndexEff.txoSetAtAddress pq c
        TxsFromTxIds txids         -> TxIdsResponse <$> ChainIndexEff.txsFromTxIds txids
        GetTip                     -> GetTipResponse <$> ChainIndexEff.getTip

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
            Wallet.yieldUnbalancedTx utx

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
            params <- Wallet.Effects.getClientParams
            forM (adjustUnbalancedTx params utx) $ \(missingAdaCosts, adjusted) -> do
                logDebug $ AdjustingUnbalancedTx missingAdaCosts
                pure adjusted
