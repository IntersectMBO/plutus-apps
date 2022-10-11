module Plutus.Script.Utils.V2.Address
    ( mkValidatorAddress
    ) where

import Plutus.Script.Utils.V2.Scripts qualified as PV2
import Plutus.V2.Ledger.Api (Address (Address), Credential (ScriptCredential), Validator)

{-# INLINABLE mkValidatorAddress #-}
-- | The address that should be used by a transaction output locked by the given
-- Plutus V2 validator script.
mkValidatorAddress :: Validator -> Address
mkValidatorAddress validator = Address (ScriptCredential (PV2.validatorHash validator)) Nothing
