-- | Constraints for transactions
module Ledger.Tx.Constraints(
    -- $constraints
    TC.TxConstraints(..)
    , TC.TxConstraint(..)
    , TC.ScriptInputConstraint(..)
    , TC.ScriptOutputConstraint(..)
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
    , TC.mustSpendOutputFromTheScript
    , TC.mustSpendScriptOutput
    , TC.mustSpendScriptOutputWithMatchingDatumAndValue
    , TC.mustValidateIn
    , TC.mustBeSignedBy
    , TC.mustProduceAtLeast
    , TC.mustIncludeDatum
    , TC.mustPayToOtherScript
    , TC.mustPayToOtherScriptAddress
    , TC.mustHashDatum
    , TC.mustSatisfyAnyOf
    -- * Queries on constraints
    , TC.modifiesUtxoSet
    , TC.isSatisfiable
    -- * On-chain validation
    , checkScriptContext
    -- * Off-chain transaction generation
    , OC.UnbalancedTx(..)
    , OC.MkTxError(..)
    , OC.mkTx
    , OC.adjustUnbalancedTx
    -- ** Combining multiple typed scripts into one transaction
    , OC.SomeLookupsAndConstraints(..)
    , OC.mkSomeTx
    -- ** Lookups
    , OC.ScriptLookups(..)
    , OC.typedValidatorLookups
    , OC.unspentOutputs
    , OC.mintingPolicy
    , OC.otherScript
    , OC.otherData
    , OC.paymentPubKey
    , OC.ownPaymentPubKeyHash
    , OC.ownStakePubKeyHash
    ) where

import Ledger.Constraints.OnChain (checkScriptContext)
import Ledger.Constraints.TxConstraints qualified as TC
import Ledger.Tx.Constraints.OffChain qualified as OC

-- $constraints
-- This module defines 'Ledger.Tx.Constraints.TxConstraints.TxConstraints', a list
-- of constraints on transactions. To construct a value of 'Ledger.Tx.Constraints.TxConstraints.TxConstraints' use
-- the 'Ledger.Tx.Constraints.TxConstraints.mustPayToTheScript',
-- 'Ledger.Tx.Constraints.TxConstraints.mustSpendAtLeast', etc functions. Once we have a
-- 'Ledger.Tx.Constraints.TxConstraints.TxConstraints' value it can be used both to generate a transaction that
-- satisfies the constraints (off-chain, using 'Ledger.Tx.Constraints.TxConstraints.OffChain.mkTx') and to check whether
-- a given pending transaction meets the constraints (on-chain, using
-- 'checkScriptContext').
