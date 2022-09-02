module RewindableIndex.Index
  ( QueryValidity(..)
  , Storable(..)
  , IxQuery
  , IxResult
  , IxPoint
  , IxMonad
  ) where


data family IxQuery  e
data family IxResult e
data family IxPoint  e
type family IxMonad  c :: * -> *

data QueryValidity s =
    Interval s s
  | AlwaysValid

class Storable c e where
  store
    :: Foldable f
    => f (IxPoint e, e)
    -> c
    -> IxMonad c c

  truncate
    :: IxPoint e
    -> c
    -> IxMonad c (Maybe c)

  resume
    :: c
    -> IxMonad c (Maybe (IxPoint e))

-- class Index a e where
--   insert
--     :: e
--     -> a
--     -> IxMonad a a

--   rewind
--     :: IxPoint e
--     -> a
--     -> IxMonad a a

--   -- * Primitive
--   query
--     :: a
--     -> IxQuery e
--     -> QueryValidity (IxPoint e)
--     -> IxMonad a (IxResult e)
