{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Plutus.Script.Utils.V2.Scripts.Validators
    ( UntypedValidator
    , mkUntypedValidator
    ) where

import Plutus.Script.Utils.V1.Scripts.Validators (UntypedValidator)
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx (UnsafeFromData (unsafeFromBuiltinData))
import PlutusTx.Prelude (check)

{-# INLINABLE mkUntypedValidator #-}
-- | Converts a custom datum and redeemer from a validator function to an
-- untyped validator function. See Note [Scripts returning Bool].
--
-- Here's an example of how this function can be used:
--
-- @
--   import PlutusTx qualified
--   import Plutus.V2.Ledger.Scripts qualified as Plutus
--   import Plutus.Script.Utils.V2.Scripts (mkUntypedValidator)
--
--   newtype MyCustomDatum = MyCustomDatum Integer
--   PlutusTx.unstableMakeIsData ''MyCustomDatum
--   newtype MyCustomRedeemer = MyCustomRedeemer Integer
--   PlutusTx.unstableMakeIsData ''MyCustomRedeemer
--
--   mkValidator :: MyCustomDatum -> MyCustomRedeemer -> Plutus.ScriptContext -> Bool
--   mkValidator _ _ _ = True
--
--   validator :: Plutus.Validator
--   validator = Plutus.mkValidatorScript
--       $$(PlutusTx.compile [|| wrap ||])
--    where
--       wrap = mkUntypedValidator mkValidator
-- @
mkUntypedValidator
    :: forall d r
    . (UnsafeFromData d, UnsafeFromData r)
    => (d -> r -> PV2.ScriptContext -> Bool)
    -> UntypedValidator
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
mkUntypedValidator f d r p =
    check $ f (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)
