{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}
{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:coverage-all #-}
-- | A general-purpose escrow contract in Plutus
module Plutus.Contracts.Escrow(
    -- $escrow
    Escrow
    , EscrowError(..)
    , AsEscrowError(..)
    , EscrowParams(..)
    , EscrowTarget(..)
    , payToScriptTarget
    , payToPaymentPubKeyTarget
    , targetTotal
    , escrowContract
    , payRedeemRefund
    , typedValidator
    -- * Actions
    , pay
    , payEp
    , redeem
    , redeemEp
    , refund
    , refundEp
    , RedeemFailReason(..)
    , RedeemSuccess(..)
    , RefundSuccess(..)
    , EscrowSchema
    -- * Exposed for test endpoints
    , Action(..)
    -- * Coverage
    , covIdx
    ) where

import Control.Lens (makeClassyPrisms, review, view)
import Control.Monad (void)
import Control.Monad.Error.Lens (throwing)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Plutus.V1.Ledger.Api (ScriptContext (..), TxInfo (..))
import PlutusTx qualified
import PlutusTx.Code
import PlutusTx.Coverage
import PlutusTx.Prelude hiding (Applicative (..), Semigroup (..), check, foldMap)

import Ledger (POSIXTime, PaymentPubKeyHash (unPaymentPubKeyHash), TxId, getCardanoTxId, interval, scriptOutputsAt,
               txSignedBy, valuePaidTo)
import Ledger qualified
import Ledger.Constraints (TxConstraints)
import Ledger.Constraints qualified as Constraints
import Ledger.Interval (after, before, from)
import Ledger.Interval qualified as Interval
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts (TypedValidator)
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (Value, geq, lt)
import Plutus.Contract
import Plutus.Contract.Typed.Tx qualified as Typed
import Plutus.Script.Utils.V1.Scripts (datumHash)
import Plutus.V1.Ledger.Scripts (Datum (Datum), DatumHash, ValidatorHash)

import Prelude (Semigroup (..), foldMap)
import Prelude qualified as Haskell

type EscrowSchema =
        Endpoint "pay-escrow" Value
        .\/ Endpoint "redeem-escrow" ()
        .\/ Endpoint "refund-escrow" ()

data RedeemFailReason = DeadlinePassed | NotEnoughFundsAtAddress
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data EscrowError =
    RedeemFailed RedeemFailReason
    | RefundFailed
    | EContractError ContractError
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''EscrowError

instance AsContractError EscrowError where
    _ContractError = _EContractError

-- $escrow
-- The escrow contract implements the exchange of value between multiple
-- parties. It is defined by a list of targets (public keys and script
-- addresses, each associated with a value). It works similar to the
-- crowdfunding contract in that the contributions can be made independently,
-- and the funds can be unlocked only by a transaction that pays the correct
-- amount to each target. A refund is possible if the outputs locked by the
-- contract have not been spent by the deadline. (Compared to the crowdfunding
-- contract, the refund policy is simpler because here because there is no
-- "collection period" during which the outputs may be spent after the deadline
-- has passed. This is because we're assuming that the participants in the
-- escrow contract will make their deposits as quickly as possible after
-- agreeing on a deal)
--
-- The contract supports two modes of operation, manual and automatic. In
-- manual mode, all actions are driven by endpoints that exposed via 'payEp'
-- 'redeemEp' and 'refundEp'. In automatic mode, the 'pay', 'redeem' and
-- 'refund'actions start immediately. This mode is useful when the escrow is
-- called from within another contract, for example during setup (collection of
-- the initial deposits).

-- | Defines where the money should go. Usually we have `d = Datum` (when
--   defining `EscrowTarget` values in off-chain code). Sometimes we have
--   `d = DatumHash` (when checking the hashes in on-chain code)
data EscrowTarget d =
    PaymentPubKeyTarget PaymentPubKeyHash Value
    | ScriptTarget ValidatorHash d Value
    deriving (Haskell.Functor)

PlutusTx.makeLift ''EscrowTarget

-- | An 'EscrowTarget' that pays the value to a public key address.
payToPaymentPubKeyTarget :: PaymentPubKeyHash -> Value -> EscrowTarget d
payToPaymentPubKeyTarget = PaymentPubKeyTarget

-- | An 'EscrowTarget' that pays the value to a script address, with the
--   given data script.
payToScriptTarget :: ValidatorHash -> Datum -> Value -> EscrowTarget Datum
payToScriptTarget = ScriptTarget

-- | Definition of an escrow contract, consisting of a deadline and a list of targets
data EscrowParams d =
    EscrowParams
        { escrowDeadline :: POSIXTime
        -- ^ Latest point at which the outputs may be spent.
        , escrowTargets  :: [EscrowTarget d]
        -- ^ Where the money should go. For each target, the contract checks that
        --   the output 'mkTxOutput' of the target is present in the spending
        --   transaction.
        } deriving (Haskell.Functor)

PlutusTx.makeLift ''EscrowParams

-- | The total 'Value' that must be paid into the escrow contract
--   before it can be unlocked
targetTotal :: EscrowParams d -> Value
targetTotal = foldl (\vl tgt -> vl + targetValue tgt) mempty . escrowTargets

-- | The 'Value' specified by an 'EscrowTarget'
targetValue :: EscrowTarget d -> Value
targetValue = \case
    PaymentPubKeyTarget _ vl -> vl
    ScriptTarget _ _ vl      -> vl

-- | Create a 'Ledger.TxOut' value for the target
mkTx :: EscrowTarget Datum -> TxConstraints Action PaymentPubKeyHash
mkTx = \case
    PaymentPubKeyTarget pkh vl ->
        Constraints.mustPayToPubKey pkh vl
    ScriptTarget vs ds vl ->
        Constraints.mustPayToOtherScript vs ds vl

data Action = Redeem | Refund

data Escrow
instance Scripts.ValidatorTypes Escrow where
    type instance RedeemerType Escrow = Action
    type instance DatumType Escrow = PaymentPubKeyHash

PlutusTx.unstableMakeIsData ''Action
PlutusTx.makeLift ''Action

{-# INLINABLE meetsTarget #-}
-- | @ptx `meetsTarget` tgt@ if @ptx@ pays at least @targetValue tgt@ to the
--   target address.
--
--   The reason why this does not require the target amount to be equal
--   to the actual amount is to enable any excess funds consumed by the
--   spending transaction to be paid to target addresses. This may happen if
--   the target address is also used as a change address for the spending
--   transaction, and allowing the target to be exceed prevents outsiders from
--   poisoning the contract by adding arbitrary outputs to the script address.
meetsTarget :: TxInfo -> EscrowTarget DatumHash -> Bool
meetsTarget ptx = \case
    PaymentPubKeyTarget pkh vl ->
        valuePaidTo ptx (unPaymentPubKeyHash pkh) `geq` vl
    ScriptTarget validatorHash dataValue vl ->
        case scriptOutputsAt validatorHash ptx of
            [(dataValue', vl')] ->
                traceIfFalse "dataValue" (dataValue' == dataValue)
                && traceIfFalse "value" (vl' `geq` vl)
            _ -> False

{-# INLINABLE validate #-}
validate :: EscrowParams DatumHash -> PaymentPubKeyHash -> Action -> ScriptContext -> Bool
validate EscrowParams{escrowDeadline, escrowTargets} contributor action ScriptContext{scriptContextTxInfo} =
    case action of
        Redeem ->
            traceIfFalse "escrowDeadline-after" (escrowDeadline `after` txInfoValidRange scriptContextTxInfo)
            && traceIfFalse "meetsTarget" (all (meetsTarget scriptContextTxInfo) escrowTargets)
        Refund ->
            traceIfFalse "escrowDeadline-before" ((escrowDeadline - 1) `before` txInfoValidRange scriptContextTxInfo)
            && traceIfFalse "txSignedBy" (scriptContextTxInfo `txSignedBy` unPaymentPubKeyHash contributor)

typedValidator :: EscrowParams Datum -> Scripts.TypedValidator Escrow
typedValidator escrow = go (Haskell.fmap datumHash escrow) where
    go = Scripts.mkTypedValidatorParam @Escrow
        $$(PlutusTx.compile [|| validate ||])
        $$(PlutusTx.compile [|| wrap ||])
    wrap = Scripts.mkUntypedValidator

escrowContract
    :: EscrowParams Datum
    -> Contract () EscrowSchema EscrowError ()
escrowContract escrow =
    let inst = typedValidator escrow
        payAndRefund = endpoint @"pay-escrow" $ \vl -> do
            _ <- pay inst escrow vl
            _ <- awaitTime $ escrowDeadline escrow
            refund inst escrow
    in selectList
        [ void payAndRefund
        , void $ redeemEp escrow
        ]

-- | 'pay' with an endpoint that gets the owner's public key and the
--   contribution.
payEp ::
    forall w s e.
    ( HasEndpoint "pay-escrow" Value s
    , AsEscrowError e
    )
    => EscrowParams Datum
    -> Promise w s e TxId
payEp escrow = promiseMap
    (mapError (review _EContractError))
    (endpoint @"pay-escrow" $ pay (typedValidator escrow) escrow)

-- | Pay some money into the escrow contract.
pay ::
    forall w s e.
    ( AsContractError e
    )
    => TypedValidator Escrow
    -- ^ The instance
    -> EscrowParams Datum
    -- ^ The escrow contract
    -> Value
    -- ^ How much money to pay in
    -> Contract w s e TxId
pay inst escrow vl = do
    pk <- ownPaymentPubKeyHash
    let tx = Constraints.mustPayToTheScript pk vl
          <> Constraints.mustValidateIn (Ledger.interval 1 (escrowDeadline escrow))
    utx <- mkTxConstraints (Constraints.typedValidatorLookups inst) tx
    getCardanoTxId <$> submitUnbalancedTx (Constraints.adjustUnbalancedTx utx)

newtype RedeemSuccess = RedeemSuccess TxId
    deriving (Haskell.Eq, Haskell.Show)

-- | 'redeem' with an endpoint.
redeemEp ::
    forall w s e.
    ( HasEndpoint "redeem-escrow" () s
    , AsEscrowError e
    )
    => EscrowParams Datum
    -> Promise w s e RedeemSuccess
redeemEp escrow = promiseMap
    (mapError (review _EscrowError))
    (endpoint @"redeem-escrow" $ \() -> redeem (typedValidator escrow) escrow)

-- | Redeem all outputs at the contract address using a transaction that
--   has all the outputs defined in the contract's list of targets.
redeem ::
    forall w s e.
    ( AsEscrowError e
    )
    => TypedValidator Escrow
    -> EscrowParams Datum
    -> Contract w s e RedeemSuccess
redeem inst escrow = mapError (review _EscrowError) $ do
    let addr = Scripts.validatorAddress inst
    current <- currentTime
    unspentOutputs <- utxosAt addr
    let
        valRange = Interval.to (Haskell.pred $ escrowDeadline escrow)
        tx = Typed.collectFromScript unspentOutputs Redeem
                <> foldMap mkTx (escrowTargets escrow)
                <> Constraints.mustValidateIn valRange
    if current >= escrowDeadline escrow
    then throwing _RedeemFailed DeadlinePassed
    else if foldMap (view Tx.ciTxOutValue) unspentOutputs `lt` targetTotal escrow
         then throwing _RedeemFailed NotEnoughFundsAtAddress
         else do
           utx <- mkTxConstraints ( Constraints.typedValidatorLookups inst
                                 <> Constraints.unspentOutputs unspentOutputs
                                  ) tx
           RedeemSuccess . getCardanoTxId <$> submitUnbalancedTx (Constraints.adjustUnbalancedTx utx)

newtype RefundSuccess = RefundSuccess TxId
    deriving newtype (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | 'refund' with an endpoint.
refundEp ::
    forall w s.
    ( HasEndpoint "refund-escrow" () s
    )
    => EscrowParams Datum
    -> Promise w s EscrowError RefundSuccess
refundEp escrow = endpoint @"refund-escrow" $ \() -> refund (typedValidator escrow) escrow

-- | Claim a refund of the contribution.
refund ::
    forall w s.
    TypedValidator Escrow
    -> EscrowParams Datum
    -> Contract w s EscrowError RefundSuccess
refund inst escrow = do
    pk <- ownPaymentPubKeyHash
    unspentOutputs <- utxosAt (Scripts.validatorAddress inst)
    let flt _ ciTxOut = either id datumHash (Tx._ciTxOutDatum ciTxOut) == datumHash (Datum (PlutusTx.toBuiltinData pk))
        tx' = Typed.collectFromScriptFilter flt unspentOutputs Refund
                <> Constraints.mustValidateIn (from (Haskell.succ $ escrowDeadline escrow))
    if Constraints.modifiesUtxoSet tx'
    then do
        utx <- mkTxConstraints ( Constraints.typedValidatorLookups inst
                              <> Constraints.unspentOutputs unspentOutputs
                               ) tx'
        RefundSuccess . getCardanoTxId <$> submitUnbalancedTx (Constraints.adjustUnbalancedTx utx)
    else throwing _RefundFailed ()

-- | Pay some money into the escrow contract. Then release all funds to their
--   specified targets if enough funds were deposited before the deadline,
--   or reclaim the contribution if the goal has not been met.
payRedeemRefund ::
    forall w s.
    EscrowParams Datum
    -> Value
    -> Contract w s EscrowError (Either RefundSuccess RedeemSuccess)
payRedeemRefund params vl = do
    let inst = typedValidator params
        go = do
            cur <- utxosAt (Scripts.validatorAddress inst)
            let presentVal = foldMap (view Tx.ciTxOutValue) cur
            if presentVal `geq` targetTotal params
                then Right <$> redeem inst params
                else do
                    time <- currentTime
                    if time >= escrowDeadline params
                        then Left <$> refund inst params
                        else waitNSlots 1 >> go
    -- Pay the value 'vl' into the contract
    _ <- pay inst params vl
    go

covIdx :: CoverageIndex
covIdx = getCovIdx $$(PlutusTx.compile [|| validate ||])
