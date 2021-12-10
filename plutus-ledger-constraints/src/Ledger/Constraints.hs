-- | Constraints for transactions
module Ledger.Constraints(
    -- $constraints
    TC.TxConstraints(..)
    , TC.TxConstraint(..)
    -- * Defining constraints
    , TC.mustPayToTheScript
    , TC.mustPayToPubKey
    , TC.mustPayToPubKeyAddress
    , TC.mustPayWithDatumToPubKey
    , TC.mustPayWithDatumToPubKeyAddress
    , TC.mustMintCurrency
    , TC.mustMintCurrencyWithRedeemer
    , TC.mustMintValue
    , TC.mustMintValueWithRedeemer
    , TC.mustSpendAtLeast
    , TC.mustSpendPubKeyOutput
    , TC.mustSpendScriptOutput
    , TC.mustValidateIn
    , TC.mustBeSignedBy
    , TC.mustProduceAtLeast
    , TC.mustIncludeDatum
    , TC.mustPayToOtherScript
    , TC.mustHashDatum
    , TC.mustSatisfyAnyOf
    -- * Queries
    , TC.modifiesUtxoSet
    , TC.isSatisfiable
    -- * Checking
    , checkScriptContext
    -- * Generating transactions
    , OC.ScriptLookups(..)
    , OC.MkTxError(..)
    , OC.UnbalancedTx
    , OC.typedValidatorLookups
    , OC.unspentOutputs
    , OC.mintingPolicy
    , OC.otherScript
    , OC.otherData
    , OC.ownPaymentPubKeyHash
    , OC.ownStakePubKeyHash
    , OC.mkTx
    , OC.paymentPubKey
    , OC.adjustUnbalancedTx
    -- ** Combining multiple typed scripts into one transaction
    , OC.SomeLookupsAndConstraints(..)
    , OC.mkSomeTx
    ) where

import Ledger.Constraints.OffChain qualified as OC
import Ledger.Constraints.OnChain (checkScriptContext)
import Ledger.Constraints.TxConstraints qualified as TC

-- $constraints
-- This module defines 'Ledger.Constraints.TxConstraints.TxConstraints', a list
-- of constraints on transactions. To construct a value of 'TxConstraints' use
-- the 'mustPayToTheScript', 'mustSpendAtLeast', etc functions. Once we have a
-- 'TxConstraints' value it can be used both to generate a transaction that
-- satisfies the constraints (off-chain, using 'mkTx') and to check whether
-- a given pending transaction meets the constraints (on-chain, using
-- 'checkScriptContext').
