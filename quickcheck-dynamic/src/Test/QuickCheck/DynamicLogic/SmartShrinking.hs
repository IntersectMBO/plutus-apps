module Test.QuickCheck.DynamicLogic.SmartShrinking( shrinkSmart ) where

import Test.QuickCheck

-- This combinator captures the 'smart shrinking' implemented for the
-- Smart type wrapper in Test.QuickCheck.Modifiers.

shrinkSmart :: (a->[a]) -> Smart a -> [Smart a]
shrinkSmart shr (Smart i x) = take i' ys `ilv` drop i' ys
  where
    ys = [Smart j y | (j,y) <- [0..] `zip` shr x ]
    i' = 0 `max`  (i-2)
    []     `ilv` bs     = bs
    as     `ilv` []     = as
    (a:as) `ilv` (b:bs) = a : b : (as `ilv` bs)
