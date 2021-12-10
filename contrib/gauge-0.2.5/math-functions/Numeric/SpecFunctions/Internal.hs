{-# LANGUAGE CPP, BangPatterns, ScopedTypeVariables, ForeignFunctionInterface #-}
-- |
-- Module    : Numeric.SpecFunctions.Internal
-- Copyright : (c) 2009, 2011, 2012 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Internal module with implementation of special functions.
module Numeric.SpecFunctions.Internal
    ( erf
    , erfc
    , invErf
    , invErfc
    , log2
    ) where

import Data.Bits       ((.&.), (.|.), shiftR)
import Data.Word       (Word64)
import qualified Data.Vector.Unboxed as U

import Numeric.MathFunctions.Constants

----------------------------------------------------------------
-- Error function
----------------------------------------------------------------

-- | Error function.
--
-- \[
-- \operatorname{erf}(x) = \frac{2}{\sqrt{\pi}} \int_{0}^{x} \exp(-t^2) dt
-- \]
--
-- Function limits are:
--
-- \[
-- \begin{aligned}
--  &\operatorname{erf}(-\infty) &=& -1 \\
--  &\operatorname{erf}(0)       &=& \phantom{-}\,0 \\
--  &\operatorname{erf}(+\infty) &=& \phantom{-}\,1 \\
-- \end{aligned}
-- \]
erf :: Double -> Double
{-# INLINE erf #-}
erf = c_erf

-- | Complementary error function.
--
-- \[
-- \operatorname{erfc}(x) = 1 - \operatorname{erf}(x)
-- \]
--
-- Function limits are:
--
-- \[
-- \begin{aligned}
--  &\operatorname{erf}(-\infty) &=&\, 2 \\
--  &\operatorname{erf}(0)       &=&\, 1 \\
--  &\operatorname{erf}(+\infty) &=&\, 0 \\
-- \end{aligned}
-- \]
erfc :: Double -> Double
{-# INLINE erfc #-}
erfc = c_erfc

foreign import ccall "erf"  c_erf  :: Double -> Double
foreign import ccall "erfc" c_erfc :: Double -> Double


-- | Inverse of 'erf'.
invErf :: Double -- ^ /p/ ∈ [-1,1]
       -> Double
invErf p = invErfc (1 - p)

-- | Inverse of 'erfc'.
invErfc :: Double -- ^ /p/ ∈ [0,2]
        -> Double
invErfc p
  | p == 2        = m_neg_inf
  | p == 0        = m_pos_inf
  | p >0 && p < 2 = if p <= 1 then r else -r
  | otherwise     = modErr $ "invErfc: p must be in [0,2] got " ++ show p
  where
    pp = if p <= 1 then p else 2 - p
    t  = sqrt $ -2 * log( 0.5 * pp)
    -- Initial guess
    x0 = -0.70711 * ((2.30753 + t * 0.27061) / (1 + t * (0.99229 + t * 0.04481)) - t)
    r  = loop 0 x0
    --
    loop :: Int -> Double -> Double
    loop !j !x
      | j >= 2    = x
      | otherwise = let err = erfc x - pp
                        x'  = x + err / (1.12837916709551257 * exp(-x * x) - x * err) -- // Halley
                    in loop (j+1) x'

-- | /O(log n)/ Compute the logarithm in base 2 of the given value.
log2 :: Int -> Int
log2 v0
    | v0 <= 0   = modErr $ "log2: nonpositive input, got " ++ show v0
    | otherwise = go 5 0 v0
  where
    go !i !r !v | i == -1        = r
                | v .&. b i /= 0 = let si = U.unsafeIndex sv i
                                   in go (i-1) (r .|. si) (v `shiftR` si)
                | otherwise      = go (i-1) r v
    b = U.unsafeIndex bv
    !bv = U.fromList [ 0x02, 0x0c, 0xf0, 0xff00
                     , fromIntegral (0xffff0000 :: Word64)
                     , fromIntegral (0xffffffff00000000 :: Word64)]
    !sv = U.fromList [1,2,4,8,16,32]

modErr :: String -> a
modErr msg = error $ "Numeric.SpecFunctions." ++ msg
