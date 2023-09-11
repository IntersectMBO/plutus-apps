{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Ledger.Tx.Constraints.ValidityInterval
  ( ValidityInterval(..)
  , interval
  , from
  , lessThan
  , fromPlutusInterval
  , toPlutusInterval
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Interval (Extended (Finite, NegInf, PosInf), Interval (Interval), LowerBound (LowerBound),
                                    UpperBound (UpperBound))
import PlutusTx.Prelude (Bool (False, True), Enum (succ), Functor (fmap), Maybe (Just, Nothing))
import Prelude qualified as Haskell

{- Note [About ValidityInterval]

During the transition from `plutus-apps` ledger validation rules to
`cardano-ledger` validation rules we have found that the `cardano-ledger` has a
problem with the translation from the transaction's upper bound slot to the
'TxInfo' validity range upper bound time. By definition, as given by the ledger
specification, the upper bound should be open (exclusive) but they convert it
as a closed bound.

We encountered this issue by getting a Phase 2 validation error when doing:

@
 txValidityTimeRange `contains` txInfoValidRange scriptContextTxInfo
@

in a Plutus validation script if 'txValidityTimeRange' does not define a lower
bound (using 'NegInf').

We have introduced 'ValidityInterval' type to provide a correct by construction way
for dealing with validity intervals. More details are available in ADR-13 doc/adr/0013-tx-validity-time-range-fix.rst.

For more info on the bug in ledger, see https://github.com/input-output-hk/cardano-ledger/issues/3043.
Note that this bug will be fixed in a future HF (next HF after Vasil -> PlutusV3 or later).
-}

-- | 'ValidityInterval' is a half open interval. Closed (inclusive) on the bottom, open
-- (exclusive) on the top. A 'Nothing' on the bottom is negative infinity, and a 'Nothing'
-- on the top is positive infinity.
data ValidityInterval a = ValidityInterval
  { invalidBefore    :: !(Maybe a) -- ^ Inclusive lower bound or negative infinity
  , invalidHereafter :: !(Maybe a) -- ^ Exclusive upper bound or positive infinity
  }
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (ToJSON, FromJSON)

instance Functor ValidityInterval where
  fmap f (ValidityInterval from' to') = ValidityInterval (fmap f from') (fmap f to')

{-# INLINABLE interval #-}
-- | @interval a b@ includes all values that are greater than or equal to @a@
-- and smaller than @b@. In math. notation: [a,b)
interval :: a -> a -> ValidityInterval a
interval s s' = ValidityInterval (Just s) (Just s')

{-# INLINABLE from #-}
-- | @from a@ is an 'ValidityInterval' that includes all values that are
--  greater than or equal to @a@. In math. notation: [a,+∞]
from :: a -> ValidityInterval a
from s = ValidityInterval (Just s) Nothing

{-# INLINABLE lessThan #-}
-- | @lessThan a@ is an 'ValidityInterval' that includes all values that are
--  smaller than @a@. In math. notation: [-∞,a)
lessThan :: a -> ValidityInterval a
lessThan s = ValidityInterval Nothing (Just s)

{-# INLINABLE fromLowerBound #-}
fromLowerBound :: Enum a => LowerBound a -> Maybe a
fromLowerBound (LowerBound (Finite v) closed) = if closed then Just v else Just (succ v)
fromLowerBound _                              = Nothing

{-# INLINABLE fromUpperBound #-}
fromUpperBound :: Enum a => UpperBound a -> Maybe a
fromUpperBound (UpperBound (Finite v) closed) = if closed then Just (succ v) else Just v
fromUpperBound _                              = Nothing

{-# INLINABLE fromPlutusInterval #-}
fromPlutusInterval :: Enum a => Interval a -> ValidityInterval a
fromPlutusInterval (Interval from' to') = ValidityInterval (fromLowerBound from') (fromUpperBound to')

{-# INLINABLE toLowerBound #-}
toLowerBound :: Maybe a -> LowerBound a
toLowerBound (Just v) = LowerBound (Finite v) True
toLowerBound _        = LowerBound NegInf True

{-# INLINABLE toUpperBound #-}
toUpperBound :: Maybe a -> UpperBound a
toUpperBound (Just v) = UpperBound (Finite v) False
toUpperBound _        = UpperBound PosInf True

{-# INLINABLE toPlutusInterval #-}
toPlutusInterval :: ValidityInterval a -> Interval a
toPlutusInterval (ValidityInterval from' to') = Interval (toLowerBound from') (toUpperBound to')
