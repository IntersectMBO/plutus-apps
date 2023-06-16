{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Plutus.Contract.Request(
    -- * PAB requests
    -- ** Waiting
    awaitSlot
    , isSlot
    , currentSlot
    , currentPABSlot
    , currentNodeClientSlot
    , currentChainIndexSlot
    , waitNSlots
    , awaitTime
    , isTime
    , currentTime
    , currentNodeClientTimeRange
    , waitNMilliSeconds
    -- ** Chain index queries
    , datumFromHash
    , datumsAt
    , validatorFromHash
    , mintingPolicyFromHash
    , stakeValidatorFromHash
    , redeemerFromHash
    , txOutFromRef
    , txFromTxId
    , findReferenceValidatorScripByHash
    , unspentTxOutFromRef
    , utxoRefMembership
    , utxoRefsAt
    , utxoRefsWithCurrency
    , utxosAt
    , utxosTxOutTxFromTx
    , utxosTxOutTxAt
    , txsFromTxIds
    , txoRefsAt
    , txsAt
    , getTip
    , collectQueryResponse
    -- ** Waiting for changes to the UTXO set
    , fundsAtAddressGt
    , fundsAtAddressGeq
    , fundsAtAddressCondition
    , watchAddressUntilSlot
    , watchAddressUntilTime
    , awaitUtxoSpent
    , utxoIsSpent
    , awaitUtxoProduced
    , utxoIsProduced
    -- ** Tx and tx output confirmation
    , RollbackState(..)
    , TxStatus
    , awaitTxStatusChange
    , awaitTxConfirmed
    , isTxConfirmed
    , TxOutStatus
    , awaitTxOutStatusChange
    -- ** Contract instances
    , ownInstanceId
    -- ** Exposing endpoints
    , HasEndpoint
    , EndpointDescription(..)
    , Endpoint
    , endpoint
    , handleEndpoint
    , endpointWithMeta
    , endpointDescription
    , endpointReq
    , endpointResp
    -- ** Wallet information
    , ownPaymentPubKeyHash
    , ownPaymentPubKeyHashes
    , ownFirstPaymentPubKeyHash
    , ownAddresses
    , ownAddress
    , ownUtxos
    , getUnspentOutput
    -- ** Submitting transactions
    , adjustUnbalancedTx
    , submitUnbalancedTx
    , submitBalancedTx
    , balanceTx
    , submitTx
    , submitTxConstraints
    , submitTxConstraintsSpending
    , submitTxConstraintsWith
    , submitTxConfirmed
    , mkTxConstraints
    , yieldUnbalancedTx
    -- ** Parameters
    , getParams
    -- * Etc.
    , ContractRow
    , pabReq
    , MkTxLog(..)
    ) where

import Cardano.Api qualified as C
import Cardano.Node.Emulator.Internal.Node.Params (Params)
import Control.Lens (Prism', _2, _Just, only, preview, review, to, view)
import Control.Monad.Freer.Error qualified as E
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types qualified as JSON
import Data.Bifunctor (Bifunctor (..))
import Data.Default (Default (def))
import Data.Foldable (fold)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Row (AllUniqueLabels, HasType, KnownSymbol, type (.==))
import Data.Text qualified as Text
import Data.Text.Extras (tshow)
import Data.Void (Void)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.TypeLits (Symbol, symbolVal)
import Ledger (CardanoAddress, DiffMilliSeconds, POSIXTime, PaymentPubKeyHash (PaymentPubKeyHash), Slot, TxOutRef,
               ValidatorHash (ValidatorHash), cardanoPubKeyHash, decoratedTxOutReferenceScript, fromMilliSeconds,
               getScriptHash, scriptHash, txOutRefId)
import Ledger.Tx (CardanoTx, DecoratedTxOut, Versioned, decoratedTxOutValue, getCardanoTxId)
import Ledger.Tx.CardanoAPI (toCardanoTxIn)
import Ledger.Tx.Constraints (TxConstraints)
import Ledger.Tx.Constraints.OffChain (ScriptLookups, UnbalancedTx)
import Ledger.Tx.Constraints.OffChain qualified as Constraints
import Ledger.Typed.Scripts (Any, TypedValidator, ValidatorTypes (DatumType, RedeemerType))
import Ledger.Value.CardanoAPI (valueGeq, valueLeq)
import Plutus.ChainIndex (ChainIndexTx, Page (nextPageQuery, pageItems), PageQuery, txOutRefs)
import Plutus.ChainIndex.Api (IsUtxoResponse, QueryResponse, TxosResponse, UtxosResponse, collectQueryResponse, paget)
import Plutus.ChainIndex.Types (RollbackState (Unknown), Tip, TxOutStatus, TxStatus)
import Plutus.Contract.Effects (ActiveEndpoint (ActiveEndpoint, aeDescription, aeMetadata),
                                PABReq (AdjustUnbalancedTxReq, AwaitSlotReq, AwaitTimeReq, AwaitTxOutStatusChangeReq, AwaitTxStatusChangeReq, AwaitUtxoProducedReq, AwaitUtxoSpentReq, BalanceTxReq, ChainIndexQueryReq, CurrentChainIndexSlotReq, CurrentNodeClientSlotReq, CurrentNodeClientTimeRangeReq, CurrentTimeReq, ExposeEndpointReq, GetParamsReq, OwnAddressesReq, OwnContractInstanceIdReq, WriteBalancedTxReq, YieldUnbalancedTxReq),
                                PABResp (ExposeEndpointResp))
import Plutus.Contract.Effects qualified as E
import Plutus.Contract.Error (AsContractError (_ChainIndexContractError, _ConstraintResolutionContractError, _EndpointDecodeContractError, _OtherContractError, _ResumableContractError, _ToCardanoConvertContractError, _WalletContractError),
                              ContractError (OtherContractError), _ContractError)
import Plutus.Contract.Logging (logDebug)
import Plutus.Contract.Resumable (prompt)
import Plutus.Contract.Schema (Input, Output)
import Plutus.Contract.Types (Contract (Contract), MatchingError (WrongVariantError), Promise (Promise), mapError,
                              runError, throwError)
import Plutus.Contract.Util (loopM)
import Plutus.V1.Ledger.Api (Datum, DatumHash, MintingPolicy, MintingPolicyHash, Redeemer, RedeemerHash, StakeValidator,
                             StakeValidatorHash, TxId, Validator)
import Plutus.V1.Ledger.Value (AssetClass)
import PlutusTx qualified
import Wallet.Emulator.Error (WalletAPIError (NoPaymentPubKeyHashError))
import Wallet.Types (ContractInstanceId, EndpointDescription (EndpointDescription),
                     EndpointValue (EndpointValue, unEndpointValue))

-- | Constraints on the contract schema, ensuring that the labels of the schema
--   are unique.
type ContractRow s =
  ( AllUniqueLabels (Input s)
  , AllUniqueLabels (Output s)
  )

{- Send a 'PABReq' and return the appropriate 'PABResp'
-}
pabReq ::
  forall w s e a.
  ( AsContractError e
  )
  => PABReq -- ^ The request to send
  -> Prism' PABResp a -- ^ Prism for the response
  -> Contract w s e a
pabReq req prism = Contract $ do
  x <- prompt @PABResp @PABReq req
  case preview prism x of
    Just r -> pure r
    _      ->
        E.throwError @e
            $ review _ResumableContractError
            $ WrongVariantError
            $ "unexpected answer: " <> tshow x

-- | Adjust the unbalanced tx
adjustUnbalancedTx ::
    forall w s e.
    ( AsContractError e
    )
    => UnbalancedTx
    -> Contract w s e UnbalancedTx
adjustUnbalancedTx utx = pabReq (AdjustUnbalancedTxReq utx) E._AdjustUnbalancedTxResp

-- | Wait until the slot
awaitSlot ::
    forall w s e.
    ( AsContractError e
    )
    => Slot
    -> Contract w s e Slot
awaitSlot s = pabReq (AwaitSlotReq s) E._AwaitSlotResp

-- | Wait until the slot
isSlot ::
    forall w s e.
    ( AsContractError e
    )
    => Slot
    -> Promise w s e Slot
isSlot = Promise . awaitSlot

-- | Get the current slot number
{-# DEPRECATED currentSlot "Use currentNodeClientSlot instead" #-}
currentSlot ::
    forall w s e.
    ( AsContractError e
    )
    => Contract w s e Slot
currentSlot = currentPABSlot

{-# DEPRECATED currentPABSlot "Use currentNodeClientSlot instead" #-}
-- | Get the current slot number of PAB
currentPABSlot ::
    forall w s e.
    ( AsContractError e
    )
    => Contract w s e Slot
currentPABSlot = pabReq CurrentNodeClientSlotReq E._CurrentNodeClientSlotResp

-- | Get the current slot number of the node client (the local or remote node) that the application
-- is connected to.
currentNodeClientSlot ::
    forall w s e.
    ( AsContractError e
    )
    => Contract w s e Slot
currentNodeClientSlot = pabReq CurrentNodeClientSlotReq E._CurrentNodeClientSlotResp

-- | Get the current node slot number querying slot number from plutus chain index to be aligned with slot at local running node
currentChainIndexSlot ::
    forall w s e.
    ( AsContractError e
    )
    => Contract w s e Slot
currentChainIndexSlot = pabReq CurrentChainIndexSlotReq E._CurrentChainIndexSlotResp

-- | Wait for a number of slots to pass
waitNSlots ::
  forall w s e.
  ( AsContractError e
  )
  => Natural
  -> Contract w s e Slot
waitNSlots n = do
  c <- currentSlot
  awaitSlot $ c + fromIntegral n

-- | Wait until the slot where the given time falls into and return latest time
-- we know has passed.
--
-- Example: if starting time is 0 and slot length is 3s, then `awaitTime 4`
-- waits until slot 2 and returns the value `POSIXTime 5`.
awaitTime ::
    forall w s e.
    ( AsContractError e
    )
    => POSIXTime
    -> Contract w s e POSIXTime
awaitTime s = pabReq (AwaitTimeReq s) E._AwaitTimeResp

-- | Wait until the slot where the given time falls into and return latest time
-- we know has passed.
isTime ::
    forall w s e.
    ( AsContractError e
    )
    => POSIXTime
    -> Promise w s e POSIXTime
isTime = Promise . awaitTime

{-# DEPRECATED currentTime "Use currentNodeClientTimeRange instead" #-}
-- | Get the latest time of the current slot.
--
-- Example: if slot length is 3s and current slot is 2, then `currentTime`
-- returns the value `POSIXTime 5`
currentTime ::
    forall w s e.
    ( AsContractError e
    )
    => Contract w s e POSIXTime
currentTime = pabReq CurrentTimeReq E._CurrentTimeResp

-- | Get the 'POSIXTime' range of the current slot.
--
-- Example: if slot length is 3s and current slot is 2, then `currentTimeRange`
-- returns the time interval @[3, 5[@.
currentNodeClientTimeRange ::
    forall w s e.
    ( AsContractError e
    )
    => Contract w s e (POSIXTime, POSIXTime)
currentNodeClientTimeRange = pabReq CurrentNodeClientTimeRangeReq E._CurrentNodeClientTimeRangeResp

-- | Wait for a number of milliseconds starting at the ending time of the current
-- slot, and return the latest time we know has passed.
--
-- Example: if starting time is 0, slot length is 3000ms and current slot is 0, then
-- `waitNMilliSeconds 0` returns the value `POSIXTime 2000` and `waitNMilliSeconds 1000`
-- returns the value `POSIXTime 5`.
waitNMilliSeconds ::
  forall w s e.
  ( AsContractError e
  )
  => DiffMilliSeconds
  -> Contract w s e POSIXTime
waitNMilliSeconds n = do
  t <- currentTime
  awaitTime $ t + fromMilliSeconds n

-- | Get the configured parameter set.
getParams ::
    forall w s e.
    ( AsContractError e
    )
    => Contract w s e Params
getParams = pabReq GetParamsReq E._GetParamsResp

datumFromHash ::
    forall w s e.
    ( AsContractError e
    )
    => DatumHash
    -> Contract w s e (Maybe Datum)
datumFromHash h = do
  cir <- pabReq (ChainIndexQueryReq $ E.DatumFromHash h) E._ChainIndexQueryResp
  case cir of
    E.DatumHashResponse r -> pure r
    r                     -> throwError $ review _ChainIndexContractError ("DatumHashResponse", r)



-- | Get all the datums at an address w.r.t. a page query TxOutRef
queryDatumsAt ::
    forall w s e.
    ( AsContractError e
    )
    => CardanoAddress
    -> PageQuery TxOutRef
    -> Contract w s e (QueryResponse [Datum])
queryDatumsAt addr pq = do
  cir <- pabReq (ChainIndexQueryReq $ E.DatumsAtAddress pq addr) E._ChainIndexQueryResp
  case cir of
    E.DatumsAtResponse r -> pure r
    r                    -> throwError $ review _ChainIndexContractError ("DatumsAtResponse", r)


-- | Get the all datums at an address whether or not the corresponding utxo have been consumed or not.
datumsAt ::
    forall w s e.
    ( AsContractError e
    )
    => CardanoAddress
    -> Contract w s e [Datum]
datumsAt addr =
  concat <$> collectQueryResponse (queryDatumsAt addr)


validatorFromHash ::
    forall w s e.
    ( AsContractError e
    )
    => ValidatorHash
    -> Contract w s e (Maybe (Versioned Validator))
validatorFromHash h = do
  cir <- pabReq (ChainIndexQueryReq $ E.ValidatorFromHash h) E._ChainIndexQueryResp
  case cir of
    E.ValidatorHashResponse r -> pure r
    r                         -> throwError $ review _ChainIndexContractError ("ValidatorHashResponse", r)

mintingPolicyFromHash ::
    forall w s e.
    ( AsContractError e
    )
    => MintingPolicyHash
    -> Contract w s e (Maybe (Versioned MintingPolicy))
mintingPolicyFromHash h = do
  cir <- pabReq (ChainIndexQueryReq $ E.MintingPolicyFromHash h) E._ChainIndexQueryResp
  case cir of
    E.MintingPolicyHashResponse r -> pure r
    r                             -> throwError $ review _ChainIndexContractError ("MintingPolicyHashResponse", r)

stakeValidatorFromHash ::
    forall w s e.
    ( AsContractError e
    )
    => StakeValidatorHash
    -> Contract w s e (Maybe (Versioned StakeValidator))
stakeValidatorFromHash h = do
  cir <- pabReq (ChainIndexQueryReq $ E.StakeValidatorFromHash h) E._ChainIndexQueryResp
  case cir of
    E.StakeValidatorHashResponse r -> pure r
    r                              -> throwError $ review _ChainIndexContractError ("StakeValidatorHashResponse", r)

redeemerFromHash ::
    forall w s e.
    ( AsContractError e
    )
    => RedeemerHash
    -> Contract w s e (Maybe Redeemer)
redeemerFromHash h = do
  cir <- pabReq (ChainIndexQueryReq $ E.RedeemerFromHash h) E._ChainIndexQueryResp
  case cir of
    E.RedeemerHashResponse r -> pure r
    r                        -> throwError $ review _ChainIndexContractError ("RedeemerHashResponse", r)

txOutFromRef ::
    forall w s e.
    ( AsContractError e
    )
    => TxOutRef
    -> Contract w s e (Maybe DecoratedTxOut)
txOutFromRef ref = do
  cir <- pabReq (ChainIndexQueryReq $ E.TxOutFromRef ref) E._ChainIndexQueryResp
  case cir of
    E.TxOutRefResponse r -> pure r
    r                    -> throwError $ review _ChainIndexContractError ("TxOutRefResponse", r)

unspentTxOutFromRef ::
    forall w s e.
    ( AsContractError e
    )
    => TxOutRef
    -> Contract w s e (Maybe DecoratedTxOut)
unspentTxOutFromRef ref = do
  cir <- pabReq (ChainIndexQueryReq $ E.UnspentTxOutFromRef ref) E._ChainIndexQueryResp
  case cir of
    E.UnspentTxOutResponse r -> pure r
    r                        -> throwError $ review _ChainIndexContractError ("UnspentTxOutResponse", r)

txFromTxId ::
    forall w s e.
    ( AsContractError e
    )
    => TxId
    -> Contract w s e (Maybe ChainIndexTx)
txFromTxId txid = do
  cir <- pabReq (ChainIndexQueryReq $ E.TxFromTxId txid) E._ChainIndexQueryResp
  case cir of
    E.TxIdResponse r -> pure r
    r                -> throwError $ review _ChainIndexContractError ("TxIdResponse", r)

utxoRefMembership ::
    forall w s e.
    ( AsContractError e
    )
    => TxOutRef
    -> Contract w s e IsUtxoResponse
utxoRefMembership ref = do
  cir <- pabReq (ChainIndexQueryReq $ E.UtxoSetMembership ref) E._ChainIndexQueryResp
  case cir of
    E.UtxoSetMembershipResponse r -> pure r
    r                             -> throwError $ review _ChainIndexContractError ("UtxoSetMembershipResponse", r)

-- | Get the unspent transaction output references at an address.
utxoRefsAt ::
    forall w s e.
    ( AsContractError e
    )
    => PageQuery TxOutRef
    -> CardanoAddress
    -> Contract w s e UtxosResponse
utxoRefsAt pq addr = do
  cir <- pabReq (ChainIndexQueryReq $ E.UtxoSetAtAddress pq addr) E._ChainIndexQueryResp
  case cir of
    E.UtxoSetAtResponse r -> pure r
    r                     -> throwError $ review _ChainIndexContractError ("UtxoSetAtResponse", r)

-- | Get the unspent transaction output references with a specific currrency ('AssetClass').
utxoRefsWithCurrency ::
    forall w s e.
    ( AsContractError e
    )
    => PageQuery TxOutRef
    -> AssetClass
    -> Contract w s e UtxosResponse
utxoRefsWithCurrency pq assetClass = do
  cir <- pabReq (ChainIndexQueryReq $ E.UtxoSetWithCurrency pq assetClass) E._ChainIndexQueryResp
  case cir of
    E.UtxoSetWithCurrencyResponse r -> pure r
    r                               -> throwError $ review _ChainIndexContractError ("UtxoSetWithCurrencyResponse", r)

-- | Get all utxos belonging to the wallet that runs this contract.
ownUtxos :: forall w s e. (AsContractError e) => Contract w s e (Map TxOutRef DecoratedTxOut)
ownUtxos = do
    addrs <- ownAddresses
    fold <$> mapM utxosAt (NonEmpty.toList addrs)

-- | Get an unspent output belonging to the wallet.
getUnspentOutput :: AsContractError e => Contract w s e TxOutRef
getUnspentOutput = do
    utxos <- ownUtxos
    case Map.keys utxos of
        inp : _ -> pure  inp
        []      -> throwError $ review _OtherContractError "Balanced transaction has no inputs"

-- | Get all the unspent transaction output at an address w.r.t. a page query TxOutRef
queryUnspentTxOutsAt ::
    forall w s e.
    ( AsContractError e
    )
    => CardanoAddress
    -> PageQuery TxOutRef
    -> Contract w s e (QueryResponse [(TxOutRef, DecoratedTxOut)])
queryUnspentTxOutsAt addr pq = do
  cir <- pabReq (ChainIndexQueryReq $ E.UnspentTxOutSetAtAddress pq addr) E._ChainIndexQueryResp
  case cir of
    E.UnspentTxOutsAtResponse r -> pure r
    r                           -> throwError $ review _ChainIndexContractError ("UnspentTxOutAtResponse", r)

-- | Find the reference to an utxo containing a reference script
-- by its the script hash, amongst the utxos at a given address
findReferenceValidatorScripByHash ::
    forall w s e.
    ( AsContractError e
    )
    => ValidatorHash
    -> CardanoAddress
    -> Contract w s e TxOutRef
findReferenceValidatorScripByHash hash address = do
    utxos <- utxosAt address
    maybe
      (throwError $ review _ContractError $ OtherContractError "Enable to find the referenc script")
      pure
      $ searchReferenceScript hash utxos
    where
        searchReferenceScript :: ValidatorHash -> Map TxOutRef DecoratedTxOut -> Maybe TxOutRef
        searchReferenceScript (ValidatorHash h) = let
            getReferenceScriptHash = _2 . decoratedTxOutReferenceScript
                . _Just . to (getScriptHash . scriptHash)
                . only h
            in fmap fst
            . find (isJust . preview getReferenceScriptHash)
            . Map.toList

-- | Get the unspent transaction outputs at an address.
utxosAt ::
    forall w s e.
    ( AsContractError e
    )
    => CardanoAddress
    -> Contract w s e (Map TxOutRef DecoratedTxOut)
utxosAt addr =
  Map.fromList . concat <$> collectQueryResponse (queryUnspentTxOutsAt addr)

-- | Get unspent transaction outputs with transaction from address.
utxosTxOutTxAt ::
    forall w s e.
    ( AsContractError e
    )
    => CardanoAddress
    -> Contract w s e (Map TxOutRef (DecoratedTxOut, ChainIndexTx))
utxosTxOutTxAt addr = do
  utxos <- utxosAt addr
  evalStateT (Map.traverseMaybeWithKey go utxos) mempty
  where
    go :: TxOutRef
       -> DecoratedTxOut
       -> StateT (Map TxId ChainIndexTx) (Contract w s e) (Maybe (DecoratedTxOut, ChainIndexTx))
    go ref out = StateT $ \lookupTx -> do
      let txid = txOutRefId ref
      -- Lookup the txid in the lookup table. If it's present, we don't need
      -- to query the chain index again. If it's not, we query the chain
      -- index and store the result in the lookup table.
      case Map.lookup txid lookupTx of
        Just tx ->
          pure (Just (out, tx), lookupTx)
        Nothing -> do
          -- We query the chain index for the tx and store it in the lookup
          -- table if it is found.
          txM <- txFromTxId txid
          case txM of
            Just tx ->
              pure (Just (out, tx), Map.insert txid tx lookupTx)
            Nothing ->
              pure (Nothing, lookupTx)


-- | Get the unspent transaction outputs from a 'ChainIndexTx'.
utxosTxOutTxFromTx ::
    AsContractError e
    => ChainIndexTx
    -> Contract w s e [(TxOutRef, (DecoratedTxOut, ChainIndexTx))]
utxosTxOutTxFromTx tx =
  catMaybes <$> mapM mkOutRef (txOutRefs tx)
  where
    mkOutRef txOutRef = do
      decoratedTxOutM <- unspentTxOutFromRef txOutRef
      pure $ decoratedTxOutM >>= \decoratedTxOut -> pure (txOutRef, (decoratedTxOut, tx))

foldTxoRefsAt ::
    forall w s e a.
    ( AsContractError e
    )
    => (a -> Page TxOutRef -> Contract w s e a)
    -> a
    -> CardanoAddress
    -> Contract w s e a
foldTxoRefsAt f ini addr = go ini (Just def)
  where
    go acc Nothing = pure acc
    go acc (Just pq) = do
      page <- paget <$> txoRefsAt pq addr
      newAcc <- f acc page
      go newAcc (nextPageQuery page)

-- | Get the transactions at an address.
txsAt ::
    forall w s e.
    ( AsContractError e
    )
    => CardanoAddress
    -> Contract w s e [ChainIndexTx]
txsAt addr = do
  foldTxoRefsAt f [] addr
  where
    f acc page = do
      let txoRefs = pageItems page
      let txIds = txOutRefId <$> txoRefs
      txs <- txsFromTxIds txIds
      pure $ acc <> txs

-- | Get the transaction outputs at an address.
txoRefsAt ::
    forall w s e.
    ( AsContractError e
    )
    => PageQuery TxOutRef
    -> CardanoAddress
    -> Contract w s e TxosResponse
txoRefsAt pq addr = do
  cir <- pabReq (ChainIndexQueryReq $ E.TxoSetAtAddress pq addr) E._ChainIndexQueryResp
  case cir of
    E.TxoSetAtResponse r -> pure r
    r                    -> throwError $ review _ChainIndexContractError ("TxoSetAtAddress", r)

-- | Get the transactions for a list of transaction ids.
txsFromTxIds ::
    forall w s e.
    ( AsContractError e
    )
    => [TxId]
    -> Contract w s e [ChainIndexTx]
txsFromTxIds txid = do
  cir <- pabReq (ChainIndexQueryReq $ E.TxsFromTxIds txid) E._ChainIndexQueryResp
  case cir of
    E.TxIdsResponse r -> pure r
    r                 -> throwError $ review _ChainIndexContractError ("TxIdsResponse", r)

getTip ::
    forall w s e.
    ( AsContractError e
    )
    => Contract w s e Tip
getTip = do
  cir <- pabReq (ChainIndexQueryReq E.GetTip) E._ChainIndexQueryResp
  case cir of
    E.GetTipResponse r -> pure r
    r                  -> throwError $ review _ChainIndexContractError ("GetTipResponse", r)

-- | Wait until the target slot and get the unspent transaction outputs at an
-- address.
watchAddressUntilSlot ::
    forall w s e.
    ( AsContractError e
    )
    => CardanoAddress
    -> Slot
    -> Contract w s e (Map TxOutRef DecoratedTxOut)
watchAddressUntilSlot a slot = awaitSlot slot >> utxosAt a

-- | Wait until the target time and get the unspent transaction outputs at an
-- address.
watchAddressUntilTime ::
    forall w s e.
    ( AsContractError e
    )
    => CardanoAddress
    -> POSIXTime
    -> Contract w s e (Map TxOutRef DecoratedTxOut)
watchAddressUntilTime a time = awaitTime time >> utxosAt a

{-| Wait until the UTXO has been spent, returning the transaction that spends it.
-}
awaitUtxoSpent ::
  forall w s e.
  ( AsContractError e
  )
  => TxOutRef
  -> Contract w s e ChainIndexTx
awaitUtxoSpent ref = do
  txIn <- either (throwError . review _ToCardanoConvertContractError) pure $ toCardanoTxIn ref
  pabReq (AwaitUtxoSpentReq txIn) E._AwaitUtxoSpentResp

{-| Wait until the UTXO has been spent, returning the transaction that spends it.
-}
utxoIsSpent ::
  forall w s e.
  ( AsContractError e
  )
  => TxOutRef
  -> Promise w s e ChainIndexTx
utxoIsSpent = Promise . awaitUtxoSpent

{-| Wait until one or more unspent outputs are produced at an address.
-}
awaitUtxoProduced ::
  forall w s e .
  ( AsContractError e
  )
  => CardanoAddress
  -> Contract w s e (NonEmpty ChainIndexTx)
awaitUtxoProduced address =
  pabReq (AwaitUtxoProducedReq address) E._AwaitUtxoProducedResp

{-| Wait until one or more unspent outputs are produced at an address.
-}
utxoIsProduced ::
  forall w s e .
  ( AsContractError e
  )
  => CardanoAddress
  -> Promise w s e (NonEmpty ChainIndexTx)
utxoIsProduced = Promise . awaitUtxoProduced

-- | Watch an address for changes, and return the outputs
--   at that address when the total value at the address
--   has surpassed the given value.
fundsAtAddressGt
    :: forall w s e.
       ( AsContractError e
       )
    => CardanoAddress
    -> C.Value
    -> Contract w s e (Map TxOutRef DecoratedTxOut)
fundsAtAddressGt addr vl =
    fundsAtAddressCondition (\presentVal -> not (presentVal `valueLeq` vl)) addr

fundsAtAddressCondition
    :: forall w s e.
       ( AsContractError e
       )
    => (C.Value -> Bool)
    -> CardanoAddress
    -> Contract w s e (Map TxOutRef DecoratedTxOut)
fundsAtAddressCondition condition addr = loopM go () where
    go () = do
        cur <- utxosAt addr
        let presentVal = foldMap (view decoratedTxOutValue) cur
        if condition presentVal
            then pure (Right cur)
            else awaitUtxoProduced addr >> pure (Left ())

-- | Watch an address for changes, and return the outputs
--   at that address when the total value at the address
--   has reached or surpassed the given value.
fundsAtAddressGeq
    :: forall w s e.
       ( AsContractError e
       )
    => CardanoAddress
    -> C.Value
    -> Contract w s e (Map TxOutRef DecoratedTxOut)
fundsAtAddressGeq addr vl =
    fundsAtAddressCondition (\presentVal -> presentVal `valueGeq` vl) addr

-- | Wait for the status of a transaction to change
awaitTxStatusChange :: forall w s e. AsContractError e => C.TxId -> Contract w s e TxStatus
awaitTxStatusChange i = pabReq (AwaitTxStatusChangeReq i) (E._AwaitTxStatusChangeResp' i)

-- TODO: Configurable level of confirmation (for example, as soon as the tx is
--       included in a block, or only when it can't be rolled back anymore)
-- | Wait until a transaction is confirmed (added to the ledger).
--   If the transaction is never added to the ledger then 'awaitTxConfirmed' never
--   returns
awaitTxConfirmed :: forall w s e. (AsContractError e) => C.TxId -> Contract w s e ()
awaitTxConfirmed i = go where
  go = do
    newStatus <- awaitTxStatusChange i
    case newStatus of
      Unknown -> go
      _       -> pure ()

-- | Wait until a transaction is confirmed (added to the ledger).
isTxConfirmed :: forall w s e. (AsContractError e) => C.TxId -> Promise w s e ()
isTxConfirmed = Promise . awaitTxConfirmed

-- | Wait for the status of a transaction output to change.
awaitTxOutStatusChange :: forall w s e. AsContractError e => TxOutRef -> Contract w s e TxOutStatus
awaitTxOutStatusChange ref = do
  txIn <- either (throwError . review _ToCardanoConvertContractError) pure $ toCardanoTxIn ref
  snd <$> pabReq (AwaitTxOutStatusChangeReq txIn) E._AwaitTxOutStatusChangeResp

-- | Get the 'ContractInstanceId' of this instance.
ownInstanceId :: forall w s e. (AsContractError e) => Contract w s e ContractInstanceId
ownInstanceId = pabReq OwnContractInstanceIdReq E._OwnContractInstanceIdResp

type HasEndpoint l a s =
  ( HasType l (EndpointValue a) (Input s)
  , HasType l ActiveEndpoint (Output s)
  , KnownSymbol l
  , ContractRow s
  )

type Endpoint l a = l .== (EndpointValue a, ActiveEndpoint)

endpointReq :: forall l a s.
    ( HasEndpoint l a s )
    => ActiveEndpoint
endpointReq =
    ActiveEndpoint
        { aeDescription = EndpointDescription $ symbolVal (Proxy @l)
        , aeMetadata    = Nothing
        }

endpointDesc :: forall (l :: Symbol). KnownSymbol l => EndpointDescription
endpointDesc = EndpointDescription $ symbolVal (Proxy @l)

endpointResp :: forall l a s. (HasEndpoint l a s, ToJSON a) => a -> PABResp
endpointResp = ExposeEndpointResp (endpointDesc @l) . EndpointValue . JSON.toJSON

-- | Expose an endpoint, return the data that was entered
endpoint
  :: forall l a w s e b.
     ( HasEndpoint l a s
     , AsContractError e
     , FromJSON a
     )
  => (a -> Contract w s e b) -> Promise w s e b
endpoint f = Promise $ do
    (ed, ev) <- pabReq (ExposeEndpointReq $ endpointReq @l @a @s) E._ExposeEndpointResp
    a <- decode ed ev
    f a

decode
    :: forall a w s e.
       ( FromJSON a
       , AsContractError e
       )
    => EndpointDescription
    -> EndpointValue JSON.Value
    -> Contract w s e a
decode ed ev@EndpointValue{unEndpointValue} =
    either
        (\e -> throwError $ review _EndpointDecodeContractError (ed, ev, Text.pack e))
        pure
        $ JSON.parseEither JSON.parseJSON unEndpointValue

handleEndpoint
  :: forall l a w s e1 e2 b.
     ( HasEndpoint l a s
     , AsContractError e1
     , FromJSON a
     )
  => (Either e1 a -> Contract w s e2 b) -> Promise w s e2 b
handleEndpoint f = Promise $ do
  a <- runError $ do
      (ed, ev) <- pabReq (ExposeEndpointReq $ endpointReq @l @a @s) E._ExposeEndpointResp
      decode ed ev
  f a

-- | Expose an endpoint with some metadata. Return the data that was entered.
endpointWithMeta
  :: forall l a w s e meta b.
     ( HasEndpoint l a s
     , AsContractError e
     , ToJSON meta
     , FromJSON a
     )
  => meta
  -> (a -> Contract w s e b)
  -> Promise w s e b
endpointWithMeta meta f = Promise $ do
    (ed, ev) <- pabReq (ExposeEndpointReq s) E._ExposeEndpointResp
    a <- decode ed ev
    f a
    where
        s = ActiveEndpoint
                { aeDescription = endpointDesc @l
                , aeMetadata    = Just $ JSON.toJSON meta
                }

endpointDescription :: forall l. KnownSymbol l => Proxy l -> EndpointDescription
endpointDescription = EndpointDescription . symbolVal

{-# DEPRECATED ownPaymentPubKeyHash "Use ownFirstPaymentPubKeyHash, ownPaymentPubKeyHashes or ownAddresses instead" #-}
-- | Get the hash of a public key belonging to the wallet that runs this contract.
--   * Any funds paid to this public key hash will be treated as the wallet's own
--     funds
--   * The wallet is able to sign transactions with the private key of this
--     public key, for example, if the public key is added to the
--     'requiredSignatures' field of 'Tx'.
--   * There is a 1-n relationship between wallets and public keys (although in
--     the mockchain n=1)
ownPaymentPubKeyHash :: forall w s e. (AsContractError e) => Contract w s e PaymentPubKeyHash
ownPaymentPubKeyHash = ownFirstPaymentPubKeyHash

-- | Get the addresses belonging to the wallet that runs this contract.
--   * Any funds paid to one of these addresses will be treated as the wallet's own
--     funds
--   * The wallet is able to sign transactions with the private key of one of its
--     public key, for example, if the public key is added to the
--     'requiredSignatures' field of 'Tx'.
--   * There is a 1-n relationship between wallets and addresses (although in
--     the mockchain n=1)
ownAddresses :: forall w s e. (AsContractError e) => Contract w s e (NonEmpty CardanoAddress)
ownAddresses = pabReq OwnAddressesReq E._OwnAddressesResp

-- | Get the first address of the wallet that runs this contract.
ownAddress :: forall w s e. (AsContractError e) => Contract w s e CardanoAddress
ownAddress = NonEmpty.head <$> ownAddresses

ownPaymentPubKeyHashes :: forall w s e. (AsContractError e) => Contract w s e [PaymentPubKeyHash]
ownPaymentPubKeyHashes = do
    addrs <- ownAddresses
    pure $ fmap PaymentPubKeyHash $ mapMaybe cardanoPubKeyHash $ NonEmpty.toList $ addrs

ownFirstPaymentPubKeyHash :: forall w s e. (AsContractError e) => Contract w s e PaymentPubKeyHash
ownFirstPaymentPubKeyHash = do
    pkhs <- ownPaymentPubKeyHashes
    case pkhs of
      []      -> throwError $ review _WalletContractError NoPaymentPubKeyHashError
      (pkh:_) -> pure pkh

-- | Send an unbalanced transaction to be balanced and signed. Returns the ID
--    of the final transaction when the transaction was submitted. Throws an
--    error if balancing or signing failed.
submitUnbalancedTx :: forall w s e. (AsContractError e) => UnbalancedTx -> Contract w s e CardanoTx
submitUnbalancedTx utx = do
  tx <- balanceTx utx
  submitBalancedTx tx

-- | Send an unbalanced transaction to be balanced. Returns the balanced transaction.
--    Throws an error if balancing failed.
balanceTx :: forall w s e. (AsContractError e) => UnbalancedTx -> Contract w s e CardanoTx
balanceTx t =
  let req = pabReq (BalanceTxReq t) E._BalanceTxResp in
  req >>= either (throwError . review _WalletContractError) pure . view E.balanceTxResponse

-- | Send an balanced transaction to be signed. Returns the ID
--    of the final transaction when the transaction was submitted. Throws an
--    error if signing failed.
submitBalancedTx :: forall w s e. (AsContractError e) => CardanoTx -> Contract w s e CardanoTx
submitBalancedTx t =
  let req = pabReq (WriteBalancedTxReq t) E._WriteBalancedTxResp in
  req >>= either (throwError . review _WalletContractError) pure . view E.writeBalancedTxResponse

-- | Build a transaction that satisfies the constraints, then submit it to the
--   network. The constraints do not refer to any typed script inputs or
--   outputs.
submitTx :: forall w s e.
  ( AsContractError e
  )
  => TxConstraints Void Void
  -> Contract w s e CardanoTx
submitTx = submitTxConstraintsWith @Void mempty

-- | Build a transaction that satisfies the constraints, then submit it to the
--   network. Using the current outputs at the contract address and the
--   contract's own public key to solve the constraints.
submitTxConstraints
  :: forall a w s e.
  ( PlutusTx.ToData (RedeemerType a)
  , PlutusTx.FromData (DatumType a)
  , PlutusTx.ToData (DatumType a)
  , AsContractError e
  )
  => TypedValidator a
  -> TxConstraints (RedeemerType a) (DatumType a)
  -> Contract w s e CardanoTx
submitTxConstraints inst = submitTxConstraintsWith (Constraints.typedValidatorLookups inst)

-- | Build a transaction that satisfies the constraints using the UTXO map
--   to resolve any input constraints (see 'Ledger.Constraints.TxConstraints.InputConstraint')
submitTxConstraintsSpending
  :: forall a w s e.
  ( PlutusTx.ToData (RedeemerType a)
  , PlutusTx.FromData (DatumType a)
  , PlutusTx.ToData (DatumType a)
  , AsContractError e
  )
  => TypedValidator a
  -> Map TxOutRef DecoratedTxOut
  -> TxConstraints (RedeemerType a) (DatumType a)
  -> Contract w s e CardanoTx
submitTxConstraintsSpending inst utxo =
  let lookups = Constraints.typedValidatorLookups inst <> Constraints.unspentOutputs utxo
  in submitTxConstraintsWith lookups

-- | Build a transaction that satisfies the constraints
mkTxConstraints :: forall a w s e.
  ( PlutusTx.ToData (RedeemerType a)
  , PlutusTx.FromData (DatumType a)
  , PlutusTx.ToData (DatumType a)
  , AsContractError e
  )
  => ScriptLookups a
  -> TxConstraints (RedeemerType a) (DatumType a)
  -> Contract w s e UnbalancedTx
mkTxConstraints lookups constraints = do
    params <- getParams
    let result = Constraints.mkTx params lookups constraints
        logData = MkTxLog
          { mkTxLogLookups = Constraints.generalise lookups
          , mkTxLogTxConstraints = bimap PlutusTx.toBuiltinData PlutusTx.toBuiltinData constraints
          , mkTxLogResult = result
          }
    logDebug logData
    mapError (review _ConstraintResolutionContractError) $ either throwError pure result

{-| Arguments and result of a call to 'mkTx'
-}
data MkTxLog =
    MkTxLog
        { mkTxLogLookups       :: ScriptLookups Any
        , mkTxLogTxConstraints :: TxConstraints PlutusTx.BuiltinData PlutusTx.BuiltinData
        , mkTxLogResult        :: Either Constraints.MkTxError UnbalancedTx
        }
        deriving stock (Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

-- | Build a transaction that satisfies the constraints, then submit it to the
--   network. Using the given constraints.
submitTxConstraintsWith
  :: forall a w s e.
  ( PlutusTx.ToData (RedeemerType a)
  , PlutusTx.FromData (DatumType a)
  , PlutusTx.ToData (DatumType a)
  , AsContractError e
  )
  => ScriptLookups a
  -> TxConstraints (RedeemerType a) (DatumType a)
  -> Contract w s e CardanoTx
submitTxConstraintsWith sl constraints =
  mkTxConstraints sl constraints >>= submitUnbalancedTx

-- | A version of 'submitTx' that waits until the transaction has been
--   confirmed on the ledger before returning.
submitTxConfirmed :: forall w s e. (AsContractError e) => UnbalancedTx -> Contract w s e ()
submitTxConfirmed t = submitUnbalancedTx t >>= awaitTxConfirmed . getCardanoTxId

-- | Take an 'UnbalancedTx' then balance, sign and submit it to the blockchain
-- without returning any results.
yieldUnbalancedTx
    :: forall w s e. (AsContractError e)
    => UnbalancedTx
    -> Contract w s e ()
yieldUnbalancedTx utx = pabReq (YieldUnbalancedTxReq utx) E._YieldUnbalancedTxResp
