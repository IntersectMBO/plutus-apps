-- | As we translate types and not type constructors, we have to pass dummy types
--   to any type constructor. doBridge will translate all parameter types which
--   come from a module TypeParameters (e.g. this one) to lower case.
--   E.g. for translating something like Maybe:
--   @
--     data Maybe' a = Nothing' | Just' a
--   @
--   you would use:
--   @
--     import 'Language.PureScript.Bridge'
--     import 'Language.PureScript.Bridge.TypeParameters'
--     mkSumType (Proxy :: Proxy (Maybe A)) -- Note the capital A, which comes from the TypeParameters module.
--   @

module Language.PureScript.Bridge.TypeParameters where


data A
data B
data C
data D
data E
data F
data G
data H
data I
data J
data K
data L
data M
data N
data O
data P
data Q
data R
data S
data T
data U
data V
data W
data X
data Y
data Z

-- | You can use those if your type parameters are actually type constructors as well:
--   @
--   mkSumType (Proxy :: Proxy ('ReaderT' R M1 A))
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
