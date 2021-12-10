{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleContexts,
    MultiParamTypeClasses, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- |
-- Module    : Numeric.Sum
-- Copyright : (c) 2014 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions for summing floating point numbers more accurately than
-- the naive 'Prelude.sum' function and its counterparts in the
-- @vector@ package and elsewhere.
--
-- When used with floating point numbers, in the worst case, the
-- 'Prelude.sum' function accumulates numeric error at a rate
-- proportional to the number of values being summed. The algorithms
-- in this module implement different methods of /compensated
-- summation/, which reduce the accumulation of numeric error so that
-- it either grows much more slowly than the number of inputs
-- (e.g. logarithmically), or remains constant.
module Numeric.Sum (
    -- * Summation type class
      Summation(..)
    , sumVector
    , kbn
    ) where

import Control.DeepSeq (NFData(..))
import Control.Monad
import Data.Data (Typeable, Data)
import Data.Vector.Generic (Vector(..), foldl')
import qualified Data.Vector.Generic.Mutable as M

import qualified Data.Foldable as F
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

-- | A class for summation of floating point numbers.
class Summation s where
    -- | The identity for summation.
    zero :: s

    -- | Add a value to a sum.
    add  :: s -> Double -> s

    -- | Sum a collection of values.
    --
    -- Example:
    -- @foo = 'sum' 'kbn' [1,2,3]@
    sum  :: (F.Foldable f) => (s -> Double) -> f Double -> Double
    sum  f = f . F.foldl' add zero
    {-# INLINE sum #-}

instance Summation Double where
    zero = 0
    add = (+)

-- | Kahan-Babuška-Neumaier summation. This is a little more
-- computationally costly than plain Kahan summation, but is /always/
-- at least as accurate.
data KBNSum = KBNSum {-# UNPACK #-} !Double {-# UNPACK #-} !Double
            deriving (Eq, Show, Typeable, Data)

newtype instance U.MVector s KBNSum = MV_KBNSum (U.MVector s (Double,Double))
newtype instance U.Vector    KBNSum = V_KBNSum  (U.Vector    (Double,Double))

instance M.MVector U.MVector KBNSum where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    {-# INLINE basicClear #-}
    {-# INLINE basicSet #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeGrow #-}
    basicLength (MV_KBNSum v) = M.basicLength v
    basicUnsafeSlice i n (MV_KBNSum v) = MV_KBNSum $ M.basicUnsafeSlice i n v
    basicOverlaps (MV_KBNSum v1) (MV_KBNSum v2) = M.basicOverlaps v1 v2
    basicUnsafeNew n = MV_KBNSum `liftM` M.basicUnsafeNew n
    basicUnsafeReplicate n (KBNSum a b) = MV_KBNSum `liftM` M.basicUnsafeReplicate n (a,b)
    basicUnsafeRead (MV_KBNSum v) i = uncurry KBNSum `liftM` M.basicUnsafeRead v i
    basicUnsafeWrite (MV_KBNSum v) i (KBNSum a b) = M.basicUnsafeWrite v i (a,b)
    basicClear (MV_KBNSum v) = M.basicClear v
    basicSet (MV_KBNSum v) (KBNSum a b) = M.basicSet v (a,b)
    basicUnsafeCopy (MV_KBNSum v1) (MV_KBNSum v2) = M.basicUnsafeCopy v1 v2
    basicUnsafeMove (MV_KBNSum v1) (MV_KBNSum v2) = M.basicUnsafeMove v1 v2
    basicUnsafeGrow (MV_KBNSum v) n = MV_KBNSum `liftM` M.basicUnsafeGrow v n
#if MIN_VERSION_vector(0,11,0)
    {-# INLINE basicInitialize #-}
    basicInitialize (MV_KBNSum v) = M.basicInitialize v
#endif

instance G.Vector U.Vector KBNSum where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (MV_KBNSum v) = V_KBNSum `liftM` G.basicUnsafeFreeze v
    basicUnsafeThaw (V_KBNSum v) = MV_KBNSum `liftM` G.basicUnsafeThaw v
    basicLength (V_KBNSum v) = G.basicLength v
    basicUnsafeSlice i n (V_KBNSum v) = V_KBNSum $ G.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_KBNSum v) i = uncurry KBNSum `liftM` G.basicUnsafeIndexM v i
    basicUnsafeCopy (MV_KBNSum mv) (V_KBNSum v) = G.basicUnsafeCopy mv v
    elemseq _ = seq


instance U.Unbox KBNSum

instance Summation KBNSum where
    zero = KBNSum 0 0
    add  = kbnAdd

instance NFData KBNSum where
    rnf !_ = ()

kbnAdd :: KBNSum -> Double -> KBNSum
kbnAdd (KBNSum sum c) x = KBNSum sum' c'
  where c' | abs sum >= abs x = c + ((sum - sum') + x)
           | otherwise        = c + ((x - sum') + sum)
        sum'                  = sum + x

-- | Return the result of a Kahan-Babuška-Neumaier sum.
kbn :: KBNSum -> Double
kbn (KBNSum sum c) = sum + c

-- | /O(n)/ Sum a vector of values.
sumVector :: (Vector v Double, Summation s) =>
             (s -> Double) -> v Double -> Double
sumVector f = f . foldl' add zero
{-# INLINE sumVector #-}

-- $usage
--
-- Most of these summation algorithms are intended to be used via the
-- 'Summation' typeclass interface. Explicit type annotations should
-- not be necessary, as the use of a function such as 'kbn' or 'kb2'
-- to extract the final sum out of a 'Summation' instance gives the
-- compiler enough information to determine the precise type of
-- summation algorithm to use.
--
-- As an example, here is a (somewhat silly) function that manually
-- computes the sum of elements in a list.
--
-- @
-- sillySumList :: [Double] -> Double
-- sillySumList = loop 'zero'
--   where loop s []     = 'kbn' s
--         loop s (x:xs) = 'seq' s' loop s' xs
--           where s'    = 'add' s x
-- @
--
-- In most instances, you can simply use the much more general 'sum'
-- function instead of writing a summation function by hand.
--
-- @
-- -- Avoid ambiguity around which sum function we are using.
-- import Prelude hiding (sum)
-- --
-- betterSumList :: [Double] -> Double
-- betterSumList xs = 'sum' 'kbn' xs
-- @

-- Note well the use of 'seq' in the example above to force the
-- evaluation of intermediate values.  If you must write a summation
-- function by hand, and you forget to evaluate the intermediate
-- values, you are likely to incur a space leak.
--
-- Here is an example of how to compute a prefix sum in which the
-- intermediate values are as accurate as possible.
--
-- @
-- prefixSum :: [Double] -> [Double]
-- prefixSum xs = map 'kbn' . 'scanl' 'add' 'zero' $ xs
-- @

-- $references
--
-- * Kahan, W. (1965), Further remarks on reducing truncation
--   errors. /Communications of the ACM/ 8(1):40.
--
-- * Neumaier, A. (1974), Rundungsfehleranalyse einiger Verfahren zur
--   Summation endlicher Summen.
--   /Zeitschrift für Angewandte Mathematik und Mechanik/ 54:39–51.
--
-- * Klein, A. (2006), A Generalized
--   Kahan-Babuška-Summation-Algorithm. /Computing/ 76(3):279-293.
--
-- * Higham, N.J. (1993), The accuracy of floating point
--   summation. /SIAM Journal on Scientific Computing/ 14(4):783–799.
