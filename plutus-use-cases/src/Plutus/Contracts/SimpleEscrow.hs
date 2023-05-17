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
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}

-- | This simple escrow contract facilitiates and exchange of currencies.
module Plutus.Contracts.SimpleEscrow
  where

import Cardano.Node.Emulator.Internal.Node.Params qualified as Params
import Control.Lens (makeClassyPrisms)
import Control.Monad (void)
import Control.Monad.Error.Lens (throwing)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Ledger (POSIXTime, PaymentPubKeyHash (unPaymentPubKeyHash), TxId, getCardanoTxId)
import Ledger qualified
import Ledger.Interval (after, before)
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Tx.Constraints.ValidityInterval qualified as Interval
import Ledger.Typed.Scripts (ScriptContextV2)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.Script.Utils.Value (Value, geq)
import Plutus.V2.Ledger.Api (txInfoValidRange)
import Plutus.V2.Ledger.Contexts (txSignedBy, valuePaidTo)
import Plutus.V2.Ledger.Contexts qualified as V2

import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude hiding (Applicative (..), Semigroup (..), check, foldMap)

import Prelude (Semigroup (..), foldMap)
import Prelude qualified as Haskell

data EscrowParams =
  EscrowParams
    { payee     :: PaymentPubKeyHash
    -- ^ The entity that needs to be paid the 'expecting' 'Value'.
    , paying    :: Value
    -- ^ Value to be paid out to the redeemer.
    , expecting :: Value
    -- ^ Value to be received by the payee.
    , deadline  :: POSIXTime
    -- ^ Time after which the contract expires.
    }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

type EscrowSchema =
        Endpoint "lock"   EscrowParams
        .\/ Endpoint "refund" EscrowParams
        .\/ Endpoint "redeem" EscrowParams

data Action
  = Redeem | Refund

data RedeemFailReason = DeadlinePassed
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

newtype RefundSuccess = RefundSuccess TxId
    deriving newtype (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype RedeemSuccess = RedeemSuccess TxId
    deriving (Haskell.Eq, Haskell.Show)

data Escrow
instance Scripts.ValidatorTypes Escrow where
    type instance RedeemerType Escrow = Action
    type instance DatumType    Escrow = EscrowParams

escrowAddress :: Ledger.CardanoAddress
escrowAddress = Scripts.validatorCardanoAddress Params.testnet escrowInstance

escrowInstance :: V2.TypedValidator Escrow
escrowInstance = V2.mkTypedValidator @Escrow
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [|| wrap ||])
      where
        wrap = Scripts.mkUntypedValidator @ScriptContextV2 @EscrowParams @Action

{-# INLINABLE validate #-}
validate :: EscrowParams -> Action -> V2.ScriptContext -> Bool
validate params action V2.ScriptContext{V2.scriptContextTxInfo=txInfo} =
  case action of
    Redeem ->
          -- Can't redeem after the deadline
      let notLapsed = deadline params `after` txInfoValidRange txInfo
          -- Payee has to have been paid
          paid      = valuePaidTo txInfo (unPaymentPubKeyHash $ payee params) `geq` expecting params
       in traceIfFalse "escrow-deadline-lapsed" notLapsed
          && traceIfFalse "escrow-not-paid" paid
    Refund ->
          -- Has to be the person that locked value requesting the refund
      let signed = txInfo `txSignedBy` unPaymentPubKeyHash (payee params)
          -- And we only refund after the deadline has passed
          lapsed = (deadline params - 1) `before` txInfoValidRange txInfo
       in traceIfFalse "escrow-not-signed" signed
          -- && traceIfFalse "refund-too-early" lapsed
          && traceIfFalse "refund-too-early" lapsed

-- | Lock the 'paying' 'Value' in the output of this script, with the
-- requirement that the transaction validates before the 'deadline'.
lockEp :: Promise () EscrowSchema EscrowError ()
lockEp = endpoint @"lock" $ \params -> do
  let valRange = Interval.lessThan (Haskell.pred $ Haskell.pred $ deadline params)
      tx = Constraints.mustPayToTheScriptWithDatumInTx params (paying params)
            <> Constraints.mustValidateInTimeRange valRange
  void $ mkTxConstraints (Constraints.typedValidatorLookups escrowInstance) tx
         >>= adjustUnbalancedTx >>= submitUnbalancedTx

-- | Attempts to redeem the 'Value' locked into this script by paying in from
-- the callers address to the payee.
redeemEp :: Promise () EscrowSchema EscrowError RedeemSuccess
redeemEp = endpoint @"redeem" redeem
  where
    redeem params = do
      time <- snd <$> currentNodeClientTimeRange
      pk <- ownFirstPaymentPubKeyHash
      unspentOutputs <- utxosAt escrowAddress

      let value = foldMap Ledger.decoratedTxOutPlutusValue unspentOutputs
          validityTimeRange = Interval.lessThan (Haskell.pred $ Haskell.pred $ deadline params)
          tx = Constraints.spendUtxosFromTheScript unspentOutputs Redeem
                      <> Constraints.mustValidateInTimeRange validityTimeRange
                      -- Pay me the output of this script
                      <> Constraints.mustPayToPubKey pk value
                      -- Pay the payee their due
                      <> Constraints.mustPayToPubKey (payee params) (expecting params)

      if time >= deadline params
      then throwing _RedeemFailed DeadlinePassed
      else do
        utx <- mkTxConstraints ( Constraints.typedValidatorLookups escrowInstance
                              <> Constraints.unspentOutputs unspentOutputs
                               ) tx >>= adjustUnbalancedTx
        RedeemSuccess . getCardanoTxId <$> submitUnbalancedTx utx

-- | Refunds the locked amount back to the 'payee'.
refundEp :: Promise () EscrowSchema EscrowError RefundSuccess
refundEp = endpoint @"refund" refund
  where
    refund params = do
      unspentOutputs <- utxosAt escrowAddress

      let tx = Constraints.spendUtxosFromTheScript unspentOutputs Refund
                  <> Constraints.mustValidateInTimeRange (Interval.from (deadline params))
                  <> Constraints.mustBeSignedBy (payee params)

      if Constraints.modifiesUtxoSet tx
      then do
        utx <- mkTxConstraints ( Constraints.typedValidatorLookups escrowInstance
                              <> Constraints.unspentOutputs unspentOutputs
                               ) tx >>= adjustUnbalancedTx
        RefundSuccess . getCardanoTxId <$> submitUnbalancedTx utx
      else throwing _RefundFailed ()

PlutusTx.unstableMakeIsData ''EscrowParams
PlutusTx.makeLift ''EscrowParams
PlutusTx.unstableMakeIsData ''Action
PlutusTx.makeLift ''Action
