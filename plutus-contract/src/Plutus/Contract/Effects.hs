{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Plutus.Contract.Effects( -- TODO: Move to Requests.Internal
    -- * Plutus application backend request effect types
    PABReq(..),
    _AwaitSlotReq,
    _AwaitTimeReq,
    _AwaitUtxoSpentReq,
    _AwaitUtxoProducedReq,
    _CurrentSlotReq,
    _CurrentTimeReq,
    _AwaitTxStatusChangeReq,
    _AwaitTxOutStatusChangeReq,
    _OwnContractInstanceIdReq,
    _OwnPaymentPublicKeyHashReq,
    _ChainIndexQueryReq,
    _BalanceTxReq,
    _WriteBalancedTxReq,
    _ExposeEndpointReq,
    _PosixTimeRangeToContainedSlotRangeReq,
    _YieldUnbalancedTxReq,
    -- ** Chain index query effect types
    _DatumFromHash,
    _ValidatorFromHash,
    _MintingPolicyFromHash,
    _RedeemerFromHash,
    _TxOutFromRef,
    _TxFromTxId,
    _UtxoSetMembership,
    _UtxoSetAtAddress,
    _UtxoSetWithCurrency,
    _TxsFromTxIds,
    _TxoSetAtAddress,
    _GetTip,
    -- * Plutus application backend response effect types
    PABResp(..),
    _AwaitSlotResp,
    _AwaitTimeResp,
    _AwaitUtxoSpentResp,
    _AwaitUtxoProducedResp,
    _CurrentSlotResp,
    _CurrentTimeResp,
    _AwaitTxStatusChangeResp,
    _AwaitTxStatusChangeResp',
    _AwaitTxOutStatusChangeResp,
    _OwnContractInstanceIdResp,
    _OwnPaymentPublicKeyHashResp,
    _ChainIndexQueryResp,
    _BalanceTxResp,
    _WriteBalancedTxResp,
    _ExposeEndpointResp,
    _PosixTimeRangeToContainedSlotRangeResp,
    _YieldUnbalancedTxResp,
    -- ** Chain index response effect types
    _DatumHashResponse,
    _ValidatorHashResponse,
    _MintingPolicyHashResponse,
    _RedeemerHashResponse,
    _TxOutRefResponse,
    _TxIdResponse,
    _UtxoSetMembershipResponse,
    _UtxoSetAtResponse,
    _UtxoSetWithCurrencyResponse,
    _TxIdsResponse,
    _TxoSetAtResponse,
    _GetTipResponse,
    -- * Etc.
    matches,
    ChainIndexQuery(..),
    ChainIndexResponse(..),
    BalanceTxResponse(..),
    balanceTxResponse,
    WriteBalancedTxResponse(..),
    writeBalancedTxResponse,
    ActiveEndpoint(..),
    ) where

import Control.Lens (Iso', Prism', iso, makePrisms, prism')
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi.Schema qualified as OpenApi
import Data.String (fromString)
import GHC.Generics (Generic)
import Ledger (Address, AssetClass, Datum, DatumHash, MintingPolicy, MintingPolicyHash, PaymentPubKeyHash, Redeemer,
               RedeemerHash, StakeValidator, StakeValidatorHash, TxId, TxOutRef, ValidatorHash)
import Ledger.Constraints.OffChain (UnbalancedTx)
import Ledger.Credential (Credential)
import Ledger.Scripts (Validator)
import Ledger.Slot (Slot, SlotRange)
import Ledger.Time (POSIXTime, POSIXTimeRange)
import Ledger.TimeSlot (SlotConversionError)
import Ledger.Tx (CardanoTx, ChainIndexTxOut, getCardanoTxId, onCardanoTx)
import Plutus.ChainIndex (Page (pageItems), PageQuery)
import Plutus.ChainIndex.Api (IsUtxoResponse (IsUtxoResponse), TxosResponse (TxosResponse),
                              UtxosResponse (UtxosResponse))
import Plutus.ChainIndex.Tx (ChainIndexTx (_citxTxId))
import Plutus.ChainIndex.Types (Tip, TxOutStatus, TxStatus)
import Prettyprinter (Pretty (pretty), hsep, indent, viaShow, vsep, (<+>))
import Wallet.API (WalletAPIError)
import Wallet.Types (ContractInstanceId, EndpointDescription, EndpointValue)

-- | Requests that 'Contract's can make
data PABReq =
    AwaitSlotReq Slot
    | AwaitTimeReq POSIXTime
    | AwaitUtxoSpentReq TxOutRef
    | AwaitUtxoProducedReq Address
    | AwaitTxStatusChangeReq TxId
    | AwaitTxOutStatusChangeReq TxOutRef
    | CurrentSlotReq
    | CurrentTimeReq
    | OwnContractInstanceIdReq
    | OwnPaymentPublicKeyHashReq
    | ChainIndexQueryReq ChainIndexQuery
    | BalanceTxReq UnbalancedTx
    | WriteBalancedTxReq CardanoTx
    | ExposeEndpointReq ActiveEndpoint
    | PosixTimeRangeToContainedSlotRangeReq POSIXTimeRange
    | YieldUnbalancedTxReq UnbalancedTx
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)

instance Pretty PABReq where
  pretty = \case
    AwaitSlotReq s                          -> "Await slot:" <+> pretty s
    AwaitTimeReq s                          -> "Await time:" <+> pretty s
    AwaitUtxoSpentReq utxo                  -> "Await utxo spent:" <+> pretty utxo
    AwaitUtxoProducedReq a                  -> "Await utxo produced:" <+> pretty a
    CurrentSlotReq                          -> "Current slot"
    CurrentTimeReq                          -> "Current time"
    AwaitTxStatusChangeReq txid             -> "Await tx status change:" <+> pretty txid
    AwaitTxOutStatusChangeReq ref           -> "Await txout status change:" <+> pretty ref
    OwnContractInstanceIdReq                -> "Own contract instance ID"
    OwnPaymentPublicKeyHashReq              -> "Own public key"
    ChainIndexQueryReq q                    -> "Chain index query:" <+> pretty q
    BalanceTxReq utx                        -> "Balance tx:" <+> pretty utx
    WriteBalancedTxReq tx                   -> "Write balanced tx:" <+> onCardanoTx pretty (fromString . show) tx
    ExposeEndpointReq ep                    -> "Expose endpoint:" <+> pretty ep
    PosixTimeRangeToContainedSlotRangeReq r -> "Posix time range to contained slot range:" <+> pretty r
    YieldUnbalancedTxReq utx                -> "Yield unbalanced tx:" <+> pretty utx

-- | Responses that 'Contract's receive
data PABResp =
    AwaitSlotResp Slot
    | AwaitTimeResp POSIXTime
    | AwaitUtxoSpentResp ChainIndexTx
    | AwaitUtxoProducedResp (NonEmpty ChainIndexTx)
    | AwaitTxStatusChangeResp TxId TxStatus
    | AwaitTxOutStatusChangeResp TxOutRef TxOutStatus
    | CurrentSlotResp Slot
    | CurrentTimeResp POSIXTime
    | OwnContractInstanceIdResp ContractInstanceId
    | OwnPaymentPublicKeyHashResp PaymentPubKeyHash
    | ChainIndexQueryResp ChainIndexResponse
    | BalanceTxResp BalanceTxResponse
    | WriteBalancedTxResp WriteBalancedTxResponse
    | ExposeEndpointResp EndpointDescription (EndpointValue JSON.Value)
    | PosixTimeRangeToContainedSlotRangeResp (Either SlotConversionError SlotRange)
    | YieldUnbalancedTxResp ()
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty PABResp where
  pretty = \case
    AwaitSlotResp s                          -> "Slot:" <+> pretty s
    AwaitTimeResp s                          -> "Time:" <+> pretty s
    AwaitUtxoSpentResp utxo                  -> "Utxo spent:" <+> pretty utxo
    AwaitUtxoProducedResp addr               -> "Utxo produced:" <+> pretty addr
    CurrentSlotResp s                        -> "Current slot:" <+> pretty s
    CurrentTimeResp s                        -> "Current time:" <+> pretty s
    AwaitTxStatusChangeResp txid status      -> "Status of" <+> pretty txid <+> "changed to" <+> pretty status
    AwaitTxOutStatusChangeResp ref status    -> "Status of" <+> pretty ref <+> "changed to" <+> pretty status
    OwnContractInstanceIdResp i              -> "Own contract instance ID:" <+> pretty i
    OwnPaymentPublicKeyHashResp k            -> "Own public key:" <+> pretty k
    ChainIndexQueryResp rsp                  -> pretty rsp
    BalanceTxResp r                          -> "Balance tx:" <+> pretty r
    WriteBalancedTxResp r                    -> "Write balanced tx:" <+> pretty r
    ExposeEndpointResp desc rsp              -> "Call endpoint" <+> pretty desc <+> "with" <+> pretty rsp
    PosixTimeRangeToContainedSlotRangeResp r -> "Slot range:" <+> pretty r
    YieldUnbalancedTxResp ()                 -> "Yielded unbalanced tx"

matches :: PABReq -> PABResp -> Bool
matches a b = case (a, b) of
  (AwaitSlotReq{}, AwaitSlotResp{})                        -> True
  (AwaitTimeReq{}, AwaitTimeResp{})                        -> True
  (AwaitUtxoSpentReq{}, AwaitUtxoSpentResp{})              -> True
  (AwaitUtxoProducedReq{}, AwaitUtxoProducedResp{})        -> True
  (CurrentSlotReq, CurrentSlotResp{})                      -> True
  (CurrentTimeReq, CurrentTimeResp{})                      -> True
  (AwaitTxStatusChangeReq i, AwaitTxStatusChangeResp i' _) -> i == i'
  (AwaitTxOutStatusChangeReq i, AwaitTxOutStatusChangeResp i' _) -> i == i'
  (OwnContractInstanceIdReq, OwnContractInstanceIdResp{})  -> True
  (OwnPaymentPublicKeyHashReq, OwnPaymentPublicKeyHashResp{})                    -> True
  (ChainIndexQueryReq r, ChainIndexQueryResp r')           -> chainIndexMatches r r'
  (BalanceTxReq{}, BalanceTxResp{})                        -> True
  (WriteBalancedTxReq{}, WriteBalancedTxResp{})            -> True
  (ExposeEndpointReq ActiveEndpoint{aeDescription}, ExposeEndpointResp desc _)
    | aeDescription == desc -> True
  (PosixTimeRangeToContainedSlotRangeReq{}, PosixTimeRangeToContainedSlotRangeResp{}) -> True
  (YieldUnbalancedTxReq{}, YieldUnbalancedTxResp{})        -> True
  _                                                        -> False

chainIndexMatches :: ChainIndexQuery -> ChainIndexResponse -> Bool
chainIndexMatches q r = case (q, r) of
    (DatumFromHash{}, DatumHashResponse{})                   -> True
    (ValidatorFromHash{}, ValidatorHashResponse{})           -> True
    (MintingPolicyFromHash{}, MintingPolicyHashResponse{})   -> True
    (StakeValidatorFromHash{}, StakeValidatorHashResponse{}) -> True
    (RedeemerFromHash{}, RedeemerHashResponse{})             -> True
    (TxOutFromRef{}, TxOutRefResponse{})                     -> True
    (TxFromTxId{}, TxIdResponse{})                           -> True
    (UtxoSetMembership{}, UtxoSetMembershipResponse{})       -> True
    (UtxoSetAtAddress{}, UtxoSetAtResponse{})                -> True
    (UtxoSetWithCurrency{}, UtxoSetWithCurrencyResponse{})   -> True
    (TxsFromTxIds{}, TxIdsResponse{})                        -> True
    (TxoSetAtAddress{}, TxoSetAtResponse{})                  -> True
    (GetTip{}, GetTipResponse{})                             -> True
    _                                                        -> False

-- | Represents all possible chain index queries. Each constructor contains the
-- input(s) needed for the query. These possible queries correspond to the
-- constructors of the data type 'Plutus.ChainIndex.Effects.ChainIndexQueryEffect'.
data ChainIndexQuery =
    DatumFromHash DatumHash
  | ValidatorFromHash ValidatorHash
  | MintingPolicyFromHash MintingPolicyHash
  | StakeValidatorFromHash StakeValidatorHash
  | RedeemerFromHash RedeemerHash
  | TxOutFromRef TxOutRef
  | TxFromTxId TxId
  | UtxoSetMembership TxOutRef
  | UtxoSetAtAddress (PageQuery TxOutRef) Credential
  | UtxoSetWithCurrency (PageQuery TxOutRef) AssetClass
  | TxsFromTxIds [TxId]
  | TxoSetAtAddress (PageQuery TxOutRef) Credential
  | GetTip
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)

instance Pretty ChainIndexQuery where
    pretty = \case
        DatumFromHash h            -> "requesting datum from hash" <+> pretty h
        ValidatorFromHash h        -> "requesting validator from hash" <+> pretty h
        MintingPolicyFromHash h    -> "requesting minting policy from hash" <+> pretty h
        StakeValidatorFromHash h   -> "requesting stake validator from hash" <+> pretty h
        RedeemerFromHash h         -> "requesting redeemer from hash" <+> pretty h
        TxOutFromRef r             -> "requesting utxo from utxo reference" <+> pretty r
        TxFromTxId i               -> "requesting chain index tx from id" <+> pretty i
        UtxoSetMembership txOutRef -> "whether tx output is part of the utxo set" <+> pretty txOutRef
        UtxoSetAtAddress _ c       -> "requesting utxos located at addresses with the credential" <+> pretty c
        UtxoSetWithCurrency _ ac   -> "requesting utxos containing the asset class" <+> pretty ac
        TxsFromTxIds i             -> "requesting chain index txs from ids" <+> pretty i
        TxoSetAtAddress _ c        -> "requesting txos located at addresses with the credential" <+> pretty c
        GetTip                     -> "requesting the tip of the chain index"

-- | Represents all possible responses to chain index queries. Each constructor
-- contain the output resulting for the chain index query. These possible
-- responses come from the data type 'Plutus.ChainIndex.Effects.ChainIndexQueryEffect'.
data ChainIndexResponse =
    DatumHashResponse (Maybe Datum)
  | ValidatorHashResponse (Maybe Validator)
  | MintingPolicyHashResponse (Maybe MintingPolicy)
  | StakeValidatorHashResponse (Maybe StakeValidator)
  | TxOutRefResponse (Maybe ChainIndexTxOut)
  | RedeemerHashResponse (Maybe Redeemer)
  | TxIdResponse (Maybe ChainIndexTx)
  | UtxoSetMembershipResponse IsUtxoResponse
  | UtxoSetAtResponse UtxosResponse
  | UtxoSetWithCurrencyResponse UtxosResponse
  | TxIdsResponse [ChainIndexTx]
  | TxoSetAtResponse TxosResponse
  | GetTipResponse Tip
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty ChainIndexResponse where
    pretty = \case
        DatumHashResponse d -> "Chain index datum from hash response:" <+> pretty d
        ValidatorHashResponse v -> "Chain index validator from hash response:" <+> pretty v
        MintingPolicyHashResponse m -> "Chain index minting policy from hash response:" <+> pretty m
        StakeValidatorHashResponse m -> "Chain index stake validator from hash response:" <+> pretty m
        RedeemerHashResponse r -> "Chain index redeemer from hash response:" <+> pretty r
        TxOutRefResponse t -> "Chain index utxo from utxo ref response:" <+> pretty t
        TxIdResponse t -> "Chain index tx from tx id response:" <+> pretty (_citxTxId <$> t)
        UtxoSetMembershipResponse (IsUtxoResponse tip b) ->
                "Chain index response whether tx output ref is part of the UTxO set:"
            <+> pretty b
            <+> "with tip"
            <+> pretty tip
        UtxoSetAtResponse (UtxosResponse tip txOutRefPage) ->
                "Chain index UTxO set from address response:"
            <+> "Current tip is"
            <+> pretty tip
            <+> "and utxo refs are"
            <+> hsep (fmap pretty $ pageItems txOutRefPage)
        UtxoSetWithCurrencyResponse (UtxosResponse tip txOutRefPage) ->
                "Chain index UTxO with asset class response:"
            <+> "Current tip is"
            <+> pretty tip
            <+> "and utxo refs are"
            <+> hsep (fmap pretty $ pageItems txOutRefPage)
        TxIdsResponse t -> "Chain index txs from tx ids response:" <+> pretty (_citxTxId <$> t)
        TxoSetAtResponse (TxosResponse txOutRefPage) ->
                "Chain index TxO set from address response:"
            <+> "The txo refs are"
            <+> hsep (fmap pretty $ pageItems txOutRefPage)
        GetTipResponse tip -> "Chain index get tip response:" <+> pretty tip

data BalanceTxResponse =
  BalanceTxFailed WalletAPIError
  | BalanceTxSuccess CardanoTx
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty BalanceTxResponse where
  pretty = \case
    BalanceTxFailed e   -> "BalanceTxFailed:" <+> pretty e
    BalanceTxSuccess tx -> "BalanceTxSuccess:" <+> pretty (getCardanoTxId tx)

_AwaitTxStatusChangeResp' :: TxId -> Prism' PABResp TxStatus
_AwaitTxStatusChangeResp' i =
  prism'
    (AwaitTxStatusChangeResp i)
    (\case { AwaitTxStatusChangeResp i' s | i == i' -> Just s; _ -> Nothing })

balanceTxResponse :: Iso' BalanceTxResponse (Either WalletAPIError CardanoTx)
balanceTxResponse = iso f g where
  f = \case { BalanceTxFailed w -> Left w; BalanceTxSuccess t -> Right t }
  g = either BalanceTxFailed BalanceTxSuccess

data WriteBalancedTxResponse =
  WriteBalancedTxFailed WalletAPIError
  | WriteBalancedTxSuccess CardanoTx
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty WriteBalancedTxResponse where
  pretty = \case
    WriteBalancedTxFailed e   -> "WriteBalancedTxFailed:" <+> pretty e
    WriteBalancedTxSuccess tx -> "WriteBalancedTxSuccess:" <+> pretty (getCardanoTxId tx)

writeBalancedTxResponse :: Iso' WriteBalancedTxResponse (Either WalletAPIError CardanoTx)
writeBalancedTxResponse = iso f g where
  f = \case { WriteBalancedTxFailed w -> Left w; WriteBalancedTxSuccess t -> Right t }
  g = either WriteBalancedTxFailed WriteBalancedTxSuccess

data ActiveEndpoint = ActiveEndpoint
  { aeDescription :: EndpointDescription -- ^ The name of the endpoint
  , aeMetadata    :: Maybe JSON.Value -- ^ Data that should be shown to the user
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)

instance Pretty ActiveEndpoint where
  pretty ActiveEndpoint{aeDescription, aeMetadata} =
    indent 2 $ vsep
      [ "Endpoint:" <+> pretty aeDescription
      , "Metadata:" <+> viaShow aeMetadata
      ]

makePrisms ''PABReq

makePrisms ''PABResp

makePrisms ''ChainIndexQuery

makePrisms ''ChainIndexResponse
