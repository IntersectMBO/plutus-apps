{-# LANGUAGE TemplateHaskell #-}
-- | This module provides a few helper to ease the inspection of contract and tx validation errors
module Spec.Contract.Error where

import Control.Lens (anyOf, has, makeClassyPrisms)
import Data.Text qualified as Text
import Ledger qualified
import Ledger.Tx.Constraints.OffChain qualified as Constraints
import Plutus.Contract qualified as Contract
import Plutus.Contract qualified as Ledger
import Wallet qualified

makeClassyPrisms ''Ledger.ScriptError
makeClassyPrisms ''Wallet.WalletAPIError

cardanoLedgerErrorContaining :: Text.Text -> Ledger.ValidationError -> Bool
cardanoLedgerErrorContaining msg = anyOf Ledger._CardanoLedgerValidationError (Text.isInfixOf msg)

txOutRefNotFound :: Ledger.TxOutRef -> Contract.ContractError  -> Bool
txOutRefNotFound expectedRef =
  anyOf
    (Ledger._ConstraintResolutionContractError . Constraints._TxOutRefNotFound)
    (== expectedRef)

declaredInputMismatchError :: Contract.ContractError  -> Bool
declaredInputMismatchError =
  has $ Ledger._ConstraintResolutionContractError . Constraints._DeclaredInputMismatch

insufficientFundsError :: Contract.ContractError -> Bool
insufficientFundsError = has $ Contract._WalletContractError . _InsufficientFunds
