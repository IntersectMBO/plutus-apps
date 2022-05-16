{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
module BasicValidators where

import PlutusCore.Default qualified as PLC
import PlutusTx qualified
import PlutusTx.Prelude (Bool (False, True), BuiltinData, Integer, Maybe (Just), Ord ((<=)), check, error, fold, ($))

import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Typed.Scripts qualified as Typed
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api qualified as Ledger

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL

import Codec.Serialise (serialise)
import Flat qualified

import Prelude (IO, print, show)
import Prelude qualified as Haskell

myKeyHash :: Ledger.PubKeyHash
myKeyHash = Haskell.undefined

-- BLOCK1
-- | A specific date.
newtype Date = Date Integer
-- | Either a specific end date, or "never".
data EndDate = Fixed Integer | Never

-- 'unstableMakeIsData' is a TemplateHaskell function that takes a type name and
-- generates an 'IsData' instance definition for it. It should work for most
-- types, including newtypes and sum types. For production usage use 'makeIsDataIndexed'
-- which ensures that the output is stable across time.
PlutusTx.unstableMakeIsData ''Date
PlutusTx.unstableMakeIsData ''EndDate

-- BLOCK2
alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> ()
alwaysSucceeds _ _ _ = ()

alwaysFails :: BuiltinData -> BuiltinData -> BuiltinData -> ()
alwaysFails _ _ _ = error ()

-- We can use 'compile' to turn a validator function into a compiled Plutus Core program.
-- Here's a reminder of how to do it.
alwaysSucceedsCompiled :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
alwaysSucceedsCompiled = $$(PlutusTx.compile [|| alwaysSucceeds ||])
-- BLOCK3
-- | Checks if a date is before the given end date.
beforeEnd :: Date -> EndDate -> Bool
beforeEnd (Date d) (Fixed e) = d <= e
beforeEnd (Date _) Never     = True

-- | Check that the date in the redeemer is before the limit in the datum.
validateDate :: BuiltinData -> BuiltinData -> BuiltinData -> ()
-- The 'check' function takes a 'Bool' and fails if it is false.
-- This is handy since it's more natural to talk about booleans.
validateDate datum redeemer _ = check $ case (PlutusTx.fromBuiltinData datum, PlutusTx.fromBuiltinData redeemer) of
    -- We can decode both the arguments at the same time: 'Just' means that
    -- decoding succeeded.
    (Just endDate, Just date) -> beforeEnd date endDate
    -- One or the other failed to decode.
    _                         -> False
-- BLOCK4
validatePayment :: BuiltinData -> BuiltinData -> BuiltinData -> ()
validatePayment _ _ ctx = check $ case PlutusTx.fromBuiltinData ctx of
    Just valCtx ->
        -- The 'TxInfo' in the validation context is the representation of the
        -- transaction being validated
        let txinfo = Ledger.scriptContextTxInfo valCtx
        -- 'pubKeyOutputsAt' collects the 'Value' at all outputs which pay to
        -- the given public key hash
            values = Ledger.pubKeyOutputsAt myKeyHash txinfo
        -- 'fold' sums up all the values, we assert that there must be more
        -- than 1 Ada (more stuff is fine!)
        in fold values `Value.geq` Ada.adaValueOf 1
    _ -> False
-- BLOCK5
data DateValidator
instance Typed.ValidatorTypes DateValidator where
    type instance RedeemerType DateValidator = Date
    type instance DatumType DateValidator = EndDate
-- BLOCK6
validateDateTyped :: EndDate -> Date -> Ledger.ScriptContext -> Bool
validateDateTyped endDate date _ = beforeEnd date endDate

validateDateWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
validateDateWrapped = Typed.mkUntypedValidator validateDateTyped
-- BLOCK7
dateInstance :: Typed.TypedValidator DateValidator
dateInstance = Typed.mkTypedValidator @DateValidator
    -- The first argument is the compiled validator.
    $$(PlutusTx.compile [|| validateDateTyped ||])
    -- The second argument is a compiled wrapper.
    -- Unfortunately we can't just inline mkUntypedValidator here for technical reasons.
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Typed.mkUntypedValidator

dateValidatorHash :: Ledger.ValidatorHash
dateValidatorHash = Typed.validatorHash dateInstance

dateValidator :: Typed.Validator
dateValidator = Typed.validatorScript dateInstance
-- BLOCK8
-- We can serialize a 'Validator's, 'Datum's, and 'Redeemer's directly to CBOR
serializedDateValidator :: BSL.ByteString
serializedDateValidator = serialise dateValidator
serializedDate :: Date -> BSL.ByteString
serializedDate d = serialise (Ledger.Datum $ PlutusTx.toBuiltinData d)
serializedEndDate :: EndDate -> BSL.ByteString
serializedEndDate d = serialise (Ledger.Redeemer $ PlutusTx.toBuiltinData d)

-- The serialized forms can be written or read using normal Haskell IO functionality.
showSerialised :: IO ()
showSerialised = do
  print serializedDateValidator
  print $ serializedDate (Date 0)
  print $ serializedEndDate Never
-- BLOCK9
-- We can serialize 'CompiledCode' also
serializedCompiledCode :: BS.ByteString
serializedCompiledCode = Flat.flat $$(PlutusTx.compile [|| validateDateTyped ||])

-- The 'loadFromFile' function is a drop-in replacement for 'compile', but
-- takes the file path instead of the code to compile.
validatorCodeFromFile :: PlutusTx.CompiledCode (() -> () -> Ledger.ScriptContext -> Bool)
validatorCodeFromFile = $$(PlutusTx.loadFromFile "plutus/howtos/myscript.uplc")
-- BLOCK10
