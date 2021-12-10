{-# LANGUAGE BangPatterns, FlexibleContexts #-}
-- |
-- Module    : Statistics.Transform
-- Copyright : (c) 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Fourier-related transformations of mathematical functions.
--
-- These functions are written for simplicity and correctness, not
-- speed.  If you need a fast FFT implementation for your application,
-- you should strongly consider using a library of FFTW bindings
-- instead.

module Statistics.Transform
    (
    -- * Type synonyms
      CD
    -- * Discrete cosine transform
    , dct
    , idct
    ) where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Bits (shiftL, shiftR)
import Data.Complex (Complex(..), conjugate, realPart)
import Numeric.SpecFunctions (log2)
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector                 as V

type CD = Complex Double

-- | Discrete cosine transform (DCT-II).
dct :: (G.Vector v CD, G.Vector v Double, G.Vector v Int) => v Double -> v Double
dct = dctWorker . G.map (:+0)
{-# INLINABLE  dct #-}
{-# SPECIAlIZE dct :: U.Vector Double -> U.Vector Double #-}
{-# SPECIAlIZE dct :: V.Vector Double -> V.Vector Double #-}

dctWorker :: (G.Vector v CD, G.Vector v Double, G.Vector v Int) => v CD -> v Double
{-# INLINE dctWorker #-}
dctWorker xs
  -- length 1 is special cased because shuffle algorithms fail for it.
  | G.length xs == 1 = G.map ((2*) . realPart) xs
  | vectorOK xs      = G.map realPart $ G.zipWith (*) weights (fft interleaved)
  | otherwise        = error "Statistics.Transform.dct: bad vector length"
  where
    interleaved = G.backpermute xs $ G.enumFromThenTo 0 2 (len-2) G.++
                                     G.enumFromThenTo (len-1) (len-3) 1
    weights = G.cons 2 . G.generate (len-1) $ \x ->
              2 * exp ((0:+(-1))*fi (x+1)*pi/(2*n))
      where n = fi len
    len = G.length xs



-- | Inverse discrete cosine transform (DCT-III). It's inverse of
-- 'dct' only up to scale parameter:
--
-- > (idct . dct) x = (* length x)
idct :: (G.Vector v CD, G.Vector v Double) => v Double -> v Double
idct = idctWorker . G.map (:+0)
{-# INLINABLE  idct #-}
{-# SPECIAlIZE idct :: U.Vector Double -> U.Vector Double #-}
{-# SPECIAlIZE idct :: V.Vector Double -> V.Vector Double #-}

idctWorker :: (G.Vector v CD, G.Vector v Double) => v CD -> v Double
{-# INLINE idctWorker #-}
idctWorker xs
  | vectorOK xs = G.generate len interleave
  | otherwise   = error "Statistics.Transform.dct: bad vector length"
  where
    interleave z | even z    = vals `G.unsafeIndex` halve z
                 | otherwise = vals `G.unsafeIndex` (len - halve z - 1)
    vals = G.map realPart . ifft $ G.zipWith (*) weights xs
    weights
      = G.cons n
      $ G.generate (len - 1) $ \x -> 2 * n * exp ((0:+1) * fi (x+1) * pi/(2*n))
      where n = fi len
    len = G.length xs



-- | Inverse fast Fourier transform.
ifft :: G.Vector v CD => v CD -> v CD
ifft xs
  | vectorOK xs = G.map ((/fi (G.length xs)) . conjugate) . fft . G.map conjugate $ xs
  | otherwise   = error "Statistics.Transform.ifft: bad vector length"
{-# INLINABLE  ifft #-}
{-# SPECIAlIZE ifft :: U.Vector CD -> U.Vector CD #-}
{-# SPECIAlIZE ifft :: V.Vector CD -> V.Vector CD #-}

-- | Radix-2 decimation-in-time fast Fourier transform.
fft :: G.Vector v CD => v CD -> v CD
fft v | vectorOK v  = G.create $ do mv <- G.thaw v
                                    mfft mv
                                    return mv
      | otherwise   = error "Statistics.Transform.fft: bad vector length"
{-# INLINABLE  fft #-}
{-# SPECIAlIZE fft :: U.Vector CD -> U.Vector CD #-}
{-# SPECIAlIZE fft :: V.Vector CD -> V.Vector CD #-}

-- Vector length must be power of two. It's not checked
mfft :: (M.MVector v CD) => v s CD -> ST s ()
{-# INLINE mfft #-}
mfft vec = bitReverse 0 0
 where
  bitReverse i j | i == len-1 = stage 0 1
                 | otherwise  = do
    when (i < j) $ M.swap vec i j
    let inner k l | k <= l    = inner (k `shiftR` 1) (l-k)
                  | otherwise = bitReverse (i+1) (l+k)
    inner (len `shiftR` 1) j
  stage l !l1 | l == m    = return ()
              | otherwise = do
    let !l2 = l1 `shiftL` 1
        !e  = -6.283185307179586/fromIntegral l2
        flight j !a | j == l1   = stage (l+1) l2
                    | otherwise = do
          let butterfly i | i >= len  = flight (j+1) (a+e)
                          | otherwise = do
                let i1 = i + l1
                xi1 :+ yi1 <- M.read vec i1
                let !c = cos a
                    !s = sin a
                    d  = (c*xi1 - s*yi1) :+ (s*xi1 + c*yi1)
                ci <- M.read vec i
                M.write vec i1 (ci - d)
                M.write vec i (ci + d)
                butterfly (i+l2)
          butterfly j
    flight 0 0
  len = M.length vec
  m   = log2 len


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

fi :: Int -> CD
fi = fromIntegral

halve :: Int -> Int
halve = (`shiftR` 1)

vectorOK :: G.Vector v a => v a -> Bool
{-# INLINE vectorOK #-}
vectorOK v = (1 `shiftL` log2 n) == n where n = G.length v
