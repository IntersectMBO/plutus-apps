-- |
-- Module    : Numeric.MathFunctions.Comparison
-- Copyright : (c) 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions for approximate comparison of floating point numbers.
--
-- Approximate floating point comparison, based on Bruce Dawson's
-- \"Comparing floating point numbers\":
-- <http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm>
module Numeric.MathFunctions.Comparison
    ( within
    ) where

import Basement.Floating (doubleToWord)
import Data.Word (Word64)

-- |
-- Measure distance between two @Double@s in ULPs (units of least
-- precision). Note that it's different from @abs (ulpDelta a b)@
-- since it returns correct result even when 'ulpDelta' overflows.
ulpDistance :: Double
            -> Double
            -> Word64
ulpDistance a b =
  -- IEEE754 floats use most significant bit as sign bit (not
  -- 2-complement) and we need to rearrange representations of float
  -- number so that they could be compared lexicographically as
  -- Word64.
  let big     = 0x8000000000000000
      order i | i < big   = i + big
              | otherwise = maxBound - i
      ai = order ai0
      bi = order bi0
      d  | ai > bi   = ai - bi
         | otherwise = bi - ai
   in d
  where
    ai0 = doubleToWord a
    bi0 = doubleToWord b


-- | Compare two 'Double' values for approximate equality, using
-- Dawson's method.
--
-- The required accuracy is specified in ULPs (units of least
-- precision).  If the two numbers differ by the given number of ULPs
-- or less, this function returns @True@.
within :: Int                   -- ^ Number of ULPs of accuracy desired.
       -> Double -> Double -> Bool
within ulps a b
  | ulps < 0  = False
  | otherwise = ulpDistance a b <= fromIntegral ulps
