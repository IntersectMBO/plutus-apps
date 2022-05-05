{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
module BasicValidators where

import PlutusCore.Default qualified as PLC
import PlutusTx
import PlutusTx.Lift
import PlutusTx.Prelude

import Ledger
import Ledger.Ada
import Ledger.Typed.Scripts
import Ledger.Value

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL

import Codec.Serialise
import Flat qualified as Flat

import Prelude (IO, print, show)
import Prelude qualified as Haskell

myKeyHash :: PubKeyHash
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
unstableMakeIsData ''Date
unstableMakeIsData ''EndDate

-- BLOCK2
alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> ()
alwaysSucceeds _ _ _ = ()

alwaysFails :: BuiltinData -> BuiltinData -> BuiltinData -> ()
alwaysFails _ _ _ = error ()

-- We can use 'compile' to turn a validator function into a compiled Plutus Core program.
-- Here's a reminder of how to do it.
alwaysSucceedsCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
alwaysSucceedsCompiled = $$(compile [|| alwaysSucceeds ||])
-- BLOCK3
-- | Checks if a date is before the given end date.
beforeEnd :: Date -> EndDate -> Bool
beforeEnd (Date d) (Fixed e) = d <= e
beforeEnd (Date _) Never     = True

-- | Check that the date in the redeemer is before the limit in the datum.
validateDate :: BuiltinData -> BuiltinData -> BuiltinData -> ()
-- The 'check' function takes a 'Bool' and fails if it is false.
-- This is handy since it's more natural to talk about booleans.
validateDate datum redeemer _ = check $ case (fromBuiltinData datum, fromBuiltinData redeemer) of
    -- We can decode both the arguments at the same time: 'Just' means that
    -- decoding succeeded.
    (Just endDate, Just date) -> beforeEnd date endDate
    -- One or the other failed to decode.
    _                         -> False
-- BLOCK4
validatePayment :: BuiltinData -> BuiltinData -> BuiltinData -> ()
validatePayment _ _ ctx = check $ case fromBuiltinData ctx of
    Just valCtx ->
        -- The 'TxInfo' in the validation context is the representation of the
        -- transaction being validated
        let txinfo = scriptContextTxInfo valCtx
        -- 'pubKeyOutputsAt' collects the 'Value' at all outputs which pay to
        -- the given public key hash
            values = pubKeyOutputsAt myKeyHash txinfo
        -- 'fold' sums up all the values, we assert that there must be more
        -- than 1 Ada (more stuff is fine!)
        in fold values `geq` adaValueOf 1
    _ -> False
-- BLOCK5
data DateValidator
instance ValidatorTypes DateValidator where
    type instance RedeemerType DateValidator = Date
    type instance DatumType DateValidator = EndDate
-- BLOCK6
validateDateTyped :: EndDate -> Date -> ScriptContext -> Bool
validateDateTyped endDate date _ = beforeEnd date endDate

validateDateWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
validateDateWrapped = wrapValidator validateDateTyped
-- BLOCK7
dateInstance :: TypedValidator DateValidator
dateInstance = mkTypedValidator @DateValidator
    -- The first argument is the compiled validator.
    $$(compile [|| validateDateTyped ||])
    -- The second argument is a compiled wrapper.
    -- Unfortunately we can't just inline wrapValidator here for technical reasons.
    $$(compile [|| wrap ||])
    where
        wrap = wrapValidator

dateValidatorHash :: ValidatorHash
dateValidatorHash = validatorHash dateInstance

dateValidator :: Validator
dateValidator = validatorScript dateInstance
-- BLOCK8
-- We can serialize a 'Validator's, 'Datum's, and 'Redeemer's directly to CBOR
serializedDateValidator :: BSL.ByteString
serializedDateValidator = serialise dateValidator
serializedDate :: Date -> BSL.ByteString
serializedDate d = serialise (Datum $ toBuiltinData d)
serializedEndDate :: EndDate -> BSL.ByteString
serializedEndDate d = serialise (Redeemer $ toBuiltinData d)

-- The serialized forms can be written or read using normal Haskell IO functionality.
showSerialised :: IO ()
showSerialised = do
  print serializedDateValidator
  print $ serializedDate (Date 0)
  print $ serializedEndDate Never
-- BLOCK9
-- We can serialize 'CompiledCode' also
serializedCompiledCode :: BS.ByteString
serializedCompiledCode = Flat.flat $ $$(compile [|| validateDateTyped ||])

-- The 'loadFromFile' function is a drop-in replacement for 'compile', but
-- takes the file path instead of the code to compile.
validatorCodeFromFile :: CompiledCode (() -> () -> ScriptContext -> Bool)
validatorCodeFromFile = $$(loadFromFile "plutus/howtos/myscript.uplc")
-- BLOCK10
