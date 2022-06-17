{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Spec.Escrow.Endpoints where

import Data.Text (unpack)

import Control.Monad (void)

import Ledger (PaymentPubKeyHash)
import Ledger.Constraints qualified as Constraints
import Ledger.Interval (from)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts (TypedValidator)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.V1.Scripts qualified as Ledger
import Plutus.V1.Ledger.Api (Datum (Datum))

import Plutus.Contract
import Plutus.Contract.Typed.Tx qualified as Typed
import PlutusTx qualified
import PlutusTx.Prelude hiding (Applicative (..), Semigroup (..), check, foldMap)

import Prelude (Semigroup (..))

import Plutus.Contracts.Escrow

type EscrowTestSchema = Endpoint "badrefund-escrow" PaymentPubKeyHash .\/ EscrowSchema

-- | 'badRefund' with an endpoint.
badRefundEp ::
    forall w s.
    ( HasEndpoint "badrefund-escrow" PaymentPubKeyHash s
    )
    => EscrowParams Datum
    -> Promise w s EscrowError ()
badRefundEp escrow = endpoint @"badrefund-escrow" $ \pk -> badRefund (typedValidator escrow) pk

-- Submit a transaction attempting to take the refund belonging to the given pk.
badRefund ::
    forall w s.
    TypedValidator Escrow
    -> PaymentPubKeyHash
    -> Contract w s EscrowError ()
badRefund inst pk = do
    unspentOutputs <- utxosAt (Scripts.validatorAddress inst)
    current <- currentTime
    let flt _ ciTxOut = either id Ledger.datumHash (Tx._ciTxOutDatum ciTxOut) == Ledger.datumHash (Datum (PlutusTx.toBuiltinData pk))
        tx' = Typed.collectFromScriptFilter flt unspentOutputs Refund
           <> Constraints.mustValidateIn (from (current - 1))
    utx <- mkTxConstraints ( Constraints.typedValidatorLookups inst
                          <> Constraints.unspentOutputs unspentOutputs
                           ) tx'
    handleError (\err -> logError $ "Caught error: " ++ unpack err) $
      adjustUnbalancedTx utx >>= void . submitUnbalancedTx

