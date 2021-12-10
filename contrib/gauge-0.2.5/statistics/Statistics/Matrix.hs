{-# LANGUAGE PatternGuards #-}
-- |
-- Module    : Statistics.Matrix
-- Copyright : 2011 Aleksey Khudyakov, 2014 Bryan O'Sullivan
-- License   : BSD3
--
-- Basic matrix operations.
--
-- There isn't a widely used matrix package for Haskell yet, so
-- we implement the necessary minimum here.

module Statistics.Matrix
    ( -- * Data types
      Matrix(..)
    , Vector
      -- * Conversion from/to lists/vectors
    , fromVector
    , dimension
    -- , center
    , multiplyV
    , transpose
    , norm
    , column
    -- , row
    , for
    , unsafeIndex
    ) where

import Prelude hiding (exponent, map, sum)
import qualified Data.Vector.Unboxed as U

import Statistics.Function (for, square)
import Statistics.Matrix.Types
import Statistics.Sample.Internal (sum)


----------------------------------------------------------------
-- Conversion to/from vectors/lists
----------------------------------------------------------------

-- | Convert from a row-major vector.
fromVector :: Int               -- ^ Number of rows.
           -> Int               -- ^ Number of columns.
           -> U.Vector Double   -- ^ Flat list of values, in row-major order.
           -> Matrix
fromVector r c v
  | r*c /= len = error "input size mismatch"
  | otherwise  = Matrix r c 0 v
  where len    = U.length v

----------------------------------------------------------------
-- Other
----------------------------------------------------------------

-- | Return the dimensions of this matrix, as a (row,column) pair.
dimension :: Matrix -> (Int, Int)
dimension (Matrix r c _ _) = (r, c)

-- | Matrix-vector multiplication.
multiplyV :: Matrix -> Vector -> Vector
multiplyV m v
  | cols m == c = U.generate (rows m) (sum . U.zipWith (*) v . row m)
  | otherwise   = error $ "matrix/vector unconformable " ++ show (cols m,c)
  where c = U.length v

-- | Calculate the Euclidean norm of a vector.
norm :: Vector -> Double
norm = sqrt . sum . U.map square

-- | Return the given column.
column :: Matrix -> Int -> Vector
column (Matrix r c _ v) i = U.backpermute v $ U.enumFromStepN i c r
{-# INLINE column #-}

-- | Return the given row.
row :: Matrix -> Int -> Vector
row (Matrix _ c _ v) i = U.slice (c*i) c v

unsafeIndex :: Matrix
            -> Int              -- ^ Row.
            -> Int              -- ^ Column.
            -> Double
unsafeIndex = unsafeBounds U.unsafeIndex

-- | Given row and column numbers, calculate the offset into the flat
-- row-major vector, without checking.
unsafeBounds :: (Vector -> Int -> r) -> Matrix -> Int -> Int -> r
unsafeBounds k (Matrix _ cs _ v) r c = k v $! r * cs + c
{-# INLINE unsafeBounds #-}


transpose :: Matrix -> Matrix
transpose m@(Matrix r0 c0 e _) = Matrix c0 r0 e . U.generate (r0*c0) $ \i ->
  let (r,c) = i `quotRem` r0
  in unsafeIndex m c r
