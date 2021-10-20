{-# LANGUAGE EmptyDataDeriving #-}
-- | As we translate types and not type constructors, we have to pass dummy types
--   to any type constructor.
--
--   'buildBridge' will translate all parameter types which
--   come from a module TypeParameters (e.g. this one) to lower case.
--
--   For translating something like Maybe:
--
--   @
--     data Maybe' a = Nothing' | Just' a
--   @
--
--   you would use:
--
--   @
--     import "Language.PureScript.Bridge"
--     import "Language.PureScript.Bridge.TypeParameters"
--
--     st = mkSumType @(Maybe' A) -- Note that we use "Maybe' A" instead of just Maybe - which would not work.
--   @
module Language.PureScript.Bridge.TypeParameters where

data A deriving (Eq, Ord)

data B deriving (Eq, Ord)

data C deriving (Eq, Ord)

data D deriving (Eq, Ord)

data E deriving (Eq, Ord)

data F deriving (Eq, Ord)

data G deriving (Eq, Ord)

data H deriving (Eq, Ord)

data I deriving (Eq, Ord)

data J deriving (Eq, Ord)

data K deriving (Eq, Ord)

data L deriving (Eq, Ord)

data M deriving (Eq, Ord)

data N deriving (Eq, Ord)

data O deriving (Eq, Ord)

data P deriving (Eq, Ord)

data Q deriving (Eq, Ord)

data R deriving (Eq, Ord)

data S deriving (Eq, Ord)

data T deriving (Eq, Ord)

data U deriving (Eq, Ord)

data V deriving (Eq, Ord)

data W deriving (Eq, Ord)

data X deriving (Eq, Ord)

data Y deriving (Eq, Ord)

data Z deriving (Eq, Ord)

-- | You can use those if your type parameters are actually type constructors as well:
--   @
--   st = mkSumType @('ReaderT' R M1 A)
--   @
data A1 a

data B1 a

data C1 a

data D1 a

data E1 a

data F1 a

data G1 a

data H1 a

data I1 a

data J1 a

data K1 a

data L1 a

data M1 a

data N1 a

data O1 a

data P1 a

data Q1 a

data R1 a

data S1 a

data T1 a

data U1 a

data V1 a

data W1 a

data X1 a

data Y1 a

data Z1 a
