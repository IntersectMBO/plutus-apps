-- |
-- Module    : Numeric.MathFunctions.Constants
-- Copyright : (c) 2009, 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Constant values common to much numeric code.

module Numeric.MathFunctions.Constants
    (
      -- * IEE754 constants
      m_epsilon
    , m_huge
    , m_tiny
    , m_max_exp
    , m_pos_inf
    , m_neg_inf
    , m_NaN
    , m_max_log
    , m_min_log
      -- * Mathematical constants
    , m_1_sqrt_2
    , m_2_sqrt_pi
    , m_ln_sqrt_2_pi
    , m_sqrt_2
    , m_sqrt_2_pi
    , m_eulerMascheroni
    ) where

----------------------------------------------------------------
-- IEE754 constants
----------------------------------------------------------------

-- | Largest representable finite value.
m_huge :: Double
m_huge = 1.7976931348623157e308
{-# INLINE m_huge #-}

-- | The smallest representable positive normalized value.
m_tiny :: Double
m_tiny = 2.2250738585072014e-308
{-# INLINE m_tiny #-}

-- | The largest 'Int' /x/ such that 2**(/x/-1) is approximately
-- representable as a 'Double'.
m_max_exp :: Int
m_max_exp = 1024

-- | Positive infinity.
m_pos_inf :: Double
m_pos_inf = 1/0
{-# INLINE m_pos_inf #-}

-- | Negative infinity.
m_neg_inf :: Double
m_neg_inf = -1/0
{-# INLINE m_neg_inf #-}

-- | Not a number.
m_NaN :: Double
m_NaN = 0/0
{-# INLINE m_NaN #-}

-- | Maximum possible finite value of @log x@
m_max_log :: Double
m_max_log = 709.782712893384
{-# INLINE m_max_log #-}

-- | Logarithm of smallest normalized double ('m_tiny')
m_min_log :: Double
m_min_log = -708.3964185322641
{-# INLINE m_min_log #-}


----------------------------------------------------------------
-- Mathematical constants
----------------------------------------------------------------

-- | @sqrt 2@
m_sqrt_2 :: Double
m_sqrt_2 = 1.4142135623730950488016887242096980785696718753769480731766
{-# INLINE m_sqrt_2 #-}

-- | @sqrt (2 * pi)@
m_sqrt_2_pi :: Double
m_sqrt_2_pi = 2.5066282746310005024157652848110452530069867406099383166299
{-# INLINE m_sqrt_2_pi #-}

-- | @2 / sqrt pi@
m_2_sqrt_pi :: Double
m_2_sqrt_pi = 1.1283791670955125738961589031215451716881012586579977136881
{-# INLINE m_2_sqrt_pi #-}

-- | @1 / sqrt 2@
m_1_sqrt_2 :: Double
m_1_sqrt_2 = 0.7071067811865475244008443621048490392848359376884740365883
{-# INLINE m_1_sqrt_2 #-}

-- | The smallest 'Double' &#949; such that 1 + &#949; &#8800; 1.
m_epsilon :: Double
m_epsilon = encodeFloat (signif+1) expo - 1.0
    where (signif,expo) = decodeFloat (1.0::Double)

-- | @log(sqrt((2*pi))@
m_ln_sqrt_2_pi :: Double
m_ln_sqrt_2_pi = 0.9189385332046727417803297364056176398613974736377834128171
{-# INLINE m_ln_sqrt_2_pi #-}

-- | Euler–Mascheroni constant (γ = 0.57721...)
m_eulerMascheroni :: Double
m_eulerMascheroni = 0.5772156649015328606065121
{-# INLINE m_eulerMascheroni #-}
