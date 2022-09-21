-- | Constraints for transactions
module Ledger.Tx.Constraints(
    -- $constraints
    TC.TxConstraints(..)
    , TC.TxConstraint(..)
    , TC.ScriptInputConstraint(..)
    , TC.ScriptOutputConstraint(..)
    -- * Defining constraints
    , TC.mustPayToTheScript
    , TC.mustPayToTheScriptWithDatumInTx
    , TC.mustPayToTheScriptWithInlineDatum
    , TC.mustPayToPubKey
    , TC.mustPayToPubKeyAddress
    , TC.mustPayWithDatumToPubKey
    , TC.mustPayWithDatumToPubKeyAddress
    , TC.mustPayWithDatumInTxToPubKey
    , TC.mustPayWithDatumInTxToPubKeyAddress
    , TC.mustPayWithInlineDatumToPubKey
    , TC.mustPayWithInlineDatumToPubKeyAddress
    , TC.mustPayToAddressWithReferenceScript
    , TC.mustPayToAddressWithReferenceValidator
    , TC.mustPayToAddressWithReferenceMintingPolicy
    , TC.mustMintCurrency
    , TC.mustMintCurrencyWithRedeemer
    , TC.mustMintValue
    , TC.mustMintValueWithRedeemer
    , TC.mustSpendAtLeast
    , TC.mustSpendPubKeyOutput
    , TC.mustSpendOutputFromTheScript
    , TC.mustSpendScriptOutput
    , TC.mustSpendScriptOutputWithReference
    , TC.mustSpendScriptOutputWithMatchingDatumAndValue
    , TC.mustUseOutputAsCollateral
    , TC.mustReferenceOutput
    , TC.mustValidateIn
    , TC.mustBeSignedBy
    , TC.mustProduceAtLeast
    , TC.mustIncludeDatumInTxWithHash
    , TC.mustIncludeDatumInTx
    , TC.mustPayToOtherScript
    , TC.mustPayToOtherScriptWithDatumInTx
    , TC.mustPayToOtherScriptWithInlineDatum
    , TC.mustPayToOtherScriptAddress
    , TC.mustPayToOtherScriptAddressWithDatumInTx
    , TC.mustPayToOtherScriptAddressWithInlineDatum
    , TC.mustSatisfyAnyOf
    -- * Defining off-chain only constraints
    , TC.collectFromPlutusV1Script
    , TC.collectFromPlutusV1ScriptFilter
    , TC.collectFromTheScriptFilter
    , TC.collectFromTheScript
    , TC.collectFromPlutusV2Script
    , TC.collectFromPlutusV2ScriptFilter
    -- * Queries on constraints
    , TC.modifiesUtxoSet
    , TC.isSatisfiable
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
    , OC.plutusV1MintingPolicy
    , OC.plutusV2MintingPolicy
    , OC.otherScript
    , OC.plutusV1OtherScript
    , OC.plutusV2OtherScript
    , OC.otherData
    , OC.paymentPubKey
    , OC.ownPaymentPubKeyHash
    , OC.ownStakePubKeyHash
    ) where

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
-- 'Ledger.Constraints.OnChain.V1.checkScriptContext', 'Ledger.Constraints.OnChain.V2.checkScriptContext').
