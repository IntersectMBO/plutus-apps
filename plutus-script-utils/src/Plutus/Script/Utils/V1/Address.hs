module Plutus.Script.Utils.V1.Address
    ( mkValidatorAddress
    ) where

import Plutus.Script.Utils.V1.Scripts qualified as PV1
import Plutus.V1.Ledger.Api (Address (Address), Credential (ScriptCredential), Validator)

{-# INLINABLE mkValidatorAddress #-}
-- | The address that should be used by a transaction output locked by the given
-- Plutus V1 validator script.
mkValidatorAddress :: Validator -> Address
mkValidatorAddress validator = Address (ScriptCredential (PV1.validatorHash validator)) Nothing
