-- BLOCK1
-- Necessary language extensions for the Plutus Tx compiler to work.
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module BasicPlutusTx where

import PlutusCore.Default qualified as PLC
-- Main Plutus Tx module.
import PlutusTx qualified
-- Additional support for lifting.
import PlutusTx.Lift (liftCode, makeLift)
-- Builtin functions.
import PlutusTx.Builtins qualified as Builtins
-- The Plutus Tx Prelude, discussed further below.
import PlutusTx.Prelude qualified as PlutusTx

-- Setup for doctest examples.

-- $setup
-- >>> import Tutorial.PlutusTx
-- >>> import PlutusTx
-- >>> import PlutusCore
-- >>> import PlutusCore.Evaluation.Machine.Ck
-- >>> import Prettyprinter

-- BLOCK2
integerOne :: PlutusTx.CompiledCode Builtins.Integer
{- 'compile' turns the 'TExpQ Integer' into a
  'TExpQ (CompiledCode Integer)' and the splice
  inserts it into the program. -}
integerOne = $$(PlutusTx.compile
    {- The quote has type 'TExpQ Integer'.
      We always use unbounded integers in Plutus Core, so we have to pin
      down this numeric literal to an ``Integer`` rather than an ``Int``. -}
    [|| (1 :: Builtins.Integer) ||])

{- |
>>> pretty $ getPlc integerOne
(program 1.0.0
  (con 1)
)
-}
-- BLOCK3
integerIdentity :: PlutusTx.CompiledCode (Builtins.Integer -> Builtins.Integer)
integerIdentity = $$(PlutusTx.compile [|| \(x:: Builtins.Integer) -> x ||])

{- |
>>> pretty $ getPlc integerIdentity
(program 1.0.0
  (lam ds (con integer) ds)
)
-}
-- BLOCK4
{- Functions which will be used in Plutus Tx programs should be marked
  with GHC’s 'INLINABLE' pragma. This is usually necessary for
  non-local functions to be usable in Plutus Tx blocks, as it instructs
  GHC to keep the information that the Plutus Tx compiler needs. While
  you may be able to get away with omitting it, it is good practice to
  always include it. -}
{-# INLINABLE plusOne #-}
plusOne :: Builtins.Integer -> Builtins.Integer
{- 'addInteger' comes from 'PlutusTx.Builtins', and is
  mapped to the builtin integer addition function in Plutus Core. -}
plusOne x = x `Builtins.addInteger` 1

{-# INLINABLE myProgram #-}
myProgram :: Builtins.Integer
myProgram =
    let
        -- Local functions do not need to be marked as 'INLINABLE'.
        plusOneLocal :: Builtins.Integer -> Builtins.Integer
        plusOneLocal x = x `Builtins.addInteger` 1

        localTwo = plusOneLocal 1
        externalTwo = plusOne 1
    in localTwo `Builtins.addInteger` externalTwo

functions :: PlutusTx.CompiledCode Builtins.Integer
functions = $$(PlutusTx.compile [|| myProgram ||])

{- We’ve used the CK evaluator for Plutus Core to evaluate the program
  and check that the result was what we expected. -}
{- |
>>> pretty $ unsafeEvaluateCk $ toTerm $ getPlc functions
(con 4)
-}
-- BLOCK5
matchMaybe :: PlutusTx.CompiledCode (PlutusTx.Maybe Builtins.Integer -> Builtins.Integer)
matchMaybe = $$(PlutusTx.compile [|| \(x:: PlutusTx.Maybe Builtins.Integer) -> case x of
    PlutusTx.Just n  -> n
    PlutusTx.Nothing -> 0
  ||])
-- BLOCK6
-- | Either a specific end date, or "never".
data EndDate = Fixed Builtins.Integer | Never

-- | Check whether a given time is past the end date.
pastEnd :: PlutusTx.CompiledCode (EndDate -> Builtins.Integer -> PlutusTx.Bool)
pastEnd = $$(PlutusTx.compile [|| \(end::EndDate) (current::Builtins.Integer) -> case end of
    Fixed n -> n `Builtins.lessThanEqualsInteger` current
    Never   -> PlutusTx.False
  ||])
-- BLOCK7
-- | Check whether a given time is past the end date.
pastEnd' :: PlutusTx.CompiledCode (EndDate -> Builtins.Integer -> PlutusTx.Bool)
pastEnd' = $$(PlutusTx.compile [|| \(end::EndDate) (current::Builtins.Integer) -> case end of
    Fixed n -> n PlutusTx.< current
    Never   -> PlutusTx.False
  ||])
-- BLOCK8
addOne :: PlutusTx.CompiledCode (Builtins.Integer -> Builtins.Integer)
addOne = $$(PlutusTx.compile [|| \(x:: Builtins.Integer) -> x `Builtins.addInteger` 1 ||])
-- BLOCK9
addOneToN :: Builtins.Integer -> PlutusTx.CompiledCode Builtins.Integer
addOneToN n =
    addOne
    -- 'applyCode' applies one 'CompiledCode' to another.
    `PlutusTx.applyCode`
    -- 'liftCode' lifts the argument 'n' into a
    -- 'CompiledCode Integer'.
    liftCode n

{- |
>>> pretty $ getPlc addOne
(program 1.0.0
  [
    (lam
      addInteger
      (fun (con integer) (fun (con integer) (con integer)))
      (lam ds (con integer) [ [ addInteger ds ] (con 1) ])
    )
    (lam
      arg
      (con integer)
      (lam arg (con integer) [ [ (builtin addInteger) arg ] arg ])
    )
  ]
)
>>> let program = getPlc $ addOneToN 4
>>> pretty program
(program 1.0.0
  [
    [
      (lam
        addInteger
        (fun (con integer) (fun (con integer) (con integer)))
        (lam ds (con integer) [ [ addInteger ds ] (con 1) ])
      )
      (lam
        arg
        (con integer)
        (lam arg (con integer) [ [ (builtin addInteger) arg ] arg ])
      )
    ]
    (con 4)
  ]
)
>>> pretty $ unsafeEvaluateCk $ toTerm program
(con 5)
-}
-- BLOCK10
-- 'makeLift' generates instances of 'Lift' automatically.
makeLift ''EndDate

pastEndAt :: EndDate -> Builtins.Integer -> PlutusTx.CompiledCode PlutusTx.Bool
pastEndAt end current =
    pastEnd
    `PlutusTx.applyCode`
    liftCode end
    `PlutusTx.applyCode`
    liftCode current

{- |
>>> let program = getPlc $ pastEndAt Never 5
>>> pretty $ unsafeEvaluateCk $ toTerm program
(abs
  out_Bool (type) (lam case_True out_Bool (lam case_False out_Bool case_False))
)
-}
-- BLOCK11
