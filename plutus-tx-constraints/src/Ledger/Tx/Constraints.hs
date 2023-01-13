-- | Constraints for transactions
module Ledger.Tx.Constraints(
    -- $constraints
    TC.TxConstraints(..)
    , TC.TxConstraint(..)
    , TC.ScriptInputConstraint(..)
    , TC.ScriptOutputConstraint(..)
    -- * Defining constraints
    , TC.mustPayToTheScriptWithDatumHash
    , TC.mustPayToTheScriptWithDatumInTx
    , TC.mustPayToTheScriptWithInlineDatum
    , TC.mustPayToAddress
    , TC.mustPayToAddressWithDatumHash
    , TC.mustPayToAddressWithDatumInTx
    , TC.mustPayToAddressWithInlineDatum
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
    , TC.mustValidateInSlotRange
    , TC.mustValidateInTimeRange
    , TC.mustBeSignedBy
    , TC.mustProduceAtLeast
    , TC.mustIncludeDatumInTxWithHash
    , TC.mustIncludeDatumInTx
    , TC.mustSatisfyAnyOf
    -- * Must-pay constraints for specific types of addresses
    , TC.mustPayToPubKey
    , TC.mustPayToPubKeyAddress
    , TC.mustPayToPubKeyWithDatumHash
    , TC.mustPayToPubKeyAddressWithDatumHash
    , TC.mustPayToPubKeyWithDatumInTx
    , TC.mustPayToPubKeyAddressWithDatumInTx
    , TC.mustPayToPubKeyWithInlineDatum
    , TC.mustPayToPubKeyAddressWithInlineDatum
    , TC.mustPayToOtherScriptWithDatumHash
    , TC.mustPayToOtherScriptWithDatumInTx
    , TC.mustPayToOtherScriptWithInlineDatum
    , TC.mustPayToOtherScriptAddressWithDatumHash
    , TC.mustPayToOtherScriptAddressWithDatumInTx
    , TC.mustPayToOtherScriptAddressWithInlineDatum
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
    , OC.paymentPubKeyHash
    , OC.ownPaymentPubKeyHash
    , OC.ownStakingCredential
    -- * Deprecated
    , TC.mustPayToTheScript
    , TC.mustPayToAddressWithDatum
    , TC.mustPayWithDatumToPubKey
    , TC.mustPayWithDatumToPubKeyAddress
    , TC.mustPayWithDatumInTxToPubKey
    , TC.mustPayWithDatumInTxToPubKeyAddress
    , TC.mustPayWithInlineDatumToPubKey
    , TC.mustPayWithInlineDatumToPubKeyAddress
    , TC.mustPayToOtherScript
    , TC.mustPayToOtherScriptAddress
    , TC.mustValidateIn
    ) where

import Ledger.Constraints.TxConstraints qualified as TC
import Ledger.Tx.Constraints.OffChain qualified as OC

-- $constraints
-- This module defines 'Ledger.Tx.Constraints.TxConstraints.TxConstraints', a list
-- of constraints on transactions. To construct a value of 'Ledger.Tx.Constraints.TxConstraints.TxConstraints' use
-- the 'Ledger.Tx.Constraints.TxConstraints.mustPayToTheScriptWithDatumHash',
-- 'Ledger.Tx.Constraints.TxConstraints.mustSpendAtLeast', etc functions. Once we have a
-- 'Ledger.Tx.Constraints.TxConstraints.TxConstraints' value it can be used both to generate a transaction that
-- satisfies the constraints (off-chain, using 'Ledger.Tx.Constraints.TxConstraints.OffChain.mkTx') and to check whether
-- a given pending transaction meets the constraints (on-chain, using
-- 'Ledger.Constraints.OnChain.V1.checkScriptContext', 'Ledger.Constraints.OnChain.V2.checkScriptContext').
