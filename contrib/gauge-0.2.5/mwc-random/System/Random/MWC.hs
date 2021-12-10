{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleContexts,
    MagicHash, Rank2Types, ScopedTypeVariables, TypeFamilies,
    ForeignFunctionInterface #-}
-- |
-- Module    : System.Random.MWC
-- Copyright : (c) 2009-2012 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Pseudo-random number generation.  This module contains code for
-- generating high quality random numbers that follow a uniform
-- distribution.
--
-- For non-uniform distributions, see the
-- 'System.Random.MWC.Distributions' module.
--
-- The uniform PRNG uses Marsaglia's MWC256 (also known as MWC8222)
-- multiply-with-carry generator, which has a period of 2^8222 and
-- fares well in tests of randomness.  It is also extremely fast,
-- between 2 and 3 times faster than the Mersenne Twister.
--
-- The generator state is stored in the 'Gen' data type. It can be
-- created in several ways:
--
--   1. Using the 'withSystemRandom' call, which creates a random state.
--
--   2. Supply your own seed to 'initialize' function.
--
--   3. Finally, 'create' makes a generator from a fixed seed.
--      Generators created in this way aren't really random.
--
-- For repeatability, the state of the generator can be snapshotted
-- and replayed using the 'save' and 'restore' functions.
--
-- The simplest use is to generate a vector of uniformly distributed values:
--
-- @
--   vs \<- 'withSystemRandom' . 'asGenST' $ \\gen -> 'uniformVector' gen 100
-- @
--
-- These values can be of any type which is an instance of the class
-- 'Variate'.
--
-- To generate random values on demand, first 'create' a random number
-- generator.
--
-- @
--   gen <- 'create'
-- @
--
-- Hold onto this generator and use it wherever random values are
-- required (creating a new generator is expensive compared to
-- generating a random number, so you don't want to throw them
-- away). Get a random value using 'uniform' or 'uniformR':
--
-- @
--   v <- 'uniform' gen
-- @
--
-- @
--   v <- 'uniformR' (1, 52) gen
-- @
module System.Random.MWC
    (
    -- * Gen: Pseudo-Random Number Generators
      Gen
    , initialize
    , createSystemRandom
    , GenIO
    , splitGen

    -- * Variates: uniformly distributed values
    , Variate(..)
    , uniformVector

    ) where

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

import Control.Monad           (liftM, replicateM)
import Data.Bits               ((.|.), shiftL, shiftR)
import Data.Int                (Int8, Int16, Int32, Int64)
import Data.Vector.Generic     (Vector)
import Data.Word               (Word8, Word16, Word32, Word64)
#if !MIN_VERSION_base(4,8,0)
import Data.Word               (Word)
#endif
import Foreign.Marshal.Alloc   (allocaBytes)
import Foreign.Marshal.Array   (peekArray)
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Unboxed         as I
import qualified Data.Vector.Unboxed.Mutable as M
#if defined(mingw32_HOST_OS)
import Foreign.Ptr
import Foreign.C.Types
#else
import System.IO        (IOMode(..), hGetBuf, withBinaryFile)
#endif

import Basement.Monad (PrimMonad(..))


-- | The class of types for which we can generate uniformly
-- distributed random variates.
--
-- The uniform PRNG uses Marsaglia's MWC256 (also known as MWC8222)
-- multiply-with-carry generator, which has a period of 2^8222 and
-- fares well in tests of randomness.  It is also extremely fast,
-- between 2 and 3 times faster than the Mersenne Twister.
--
-- /Note/: Marsaglia's PRNG is not known to be cryptographically
-- secure, so you should not use it for cryptographic operations.
class Variate a where
    -- | Generate a single uniformly distributed random variate.  The
    -- range of values produced varies by type:
    --
    -- * For fixed-width integral types, the type's entire range is
    --   used.
    --
    -- * For floating point numbers, the range (0,1] is used. Zero is
    --   explicitly excluded, to allow variates to be used in
    --   statistical calculations that require non-zero values
    --   (e.g. uses of the 'log' function).
    --
    -- To generate a 'Float' variate with a range of [0,1), subtract
    -- 2**(-33).  To do the same with 'Double' variates, subtract
    -- 2**(-53).
    uniform :: Gen -> IO a
    -- | Generate single uniformly distributed random variable in a
    -- given range.
    --
    -- * For integral types inclusive range is used.
    --
    -- * For floating point numbers range (a,b] is used if one ignores
    --   rounding errors.
    uniformR :: (a,a) -> Gen -> IO a

instance Variate Word32 where
    uniform  = uniform1 fromIntegral
    uniformR a b = uniformRange a b
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

instance Variate Word64 where
    uniform  = uniform2 wordsTo64Bit
    uniformR a b = uniformRange a b
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

instance Variate Int where
#if WORD_SIZE_IN_BITS < 64
    uniform = uniform1 fromIntegral
#else
    uniform = uniform2 wordsTo64Bit
#endif
    uniformR a b = uniformRange a b
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

instance Variate Word where
#if WORD_SIZE_IN_BITS < 64
    uniform = uniform1 fromIntegral
#else
    uniform = uniform2 wordsTo64Bit
#endif
    uniformR a b = uniformRange a b
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

wordsTo64Bit :: (Integral a) => Word32 -> Word32 -> a
wordsTo64Bit x y =
    fromIntegral ((fromIntegral x `shiftL` 32) .|. fromIntegral y :: Word64)
{-# INLINE wordsTo64Bit #-}

-- | State of the pseudo-random number generator. It uses mutable
-- state so same generator shouldn't be used from the different
-- threads simultaneously.
newtype Gen = Gen (M.MVector (PrimState IO) Word32)

-- | A shorter name for PRNG state in the 'IO' monad.
type GenIO = Gen

ioff, coff :: Int
ioff = 256
coff = 257

-- | Create a generator for variates using the given seed of 256 elements
--
-- @gen' <- 'initialize' . 'fromSeed' =<< 'save'@
initialize :: I.Vector Word32 -> IO Gen
initialize seed
    | fini /= 256 = error "mwc seed invalid size"
    | otherwise   = do
        q <- M.unsafeNew 258
        fill q
        M.unsafeWrite q ioff 255
        M.unsafeWrite q coff 362436
        return (Gen q)
  where
    fini = G.length seed
    fill q = go 0 where
          go i | i == 256  = return ()
               | otherwise = M.unsafeWrite q i (G.unsafeIndex seed i) >> go (i+1)
{-# INLINE initialize #-}

-- | Acquire seed from the system entropy source. On Unix machines,
-- this will attempt to use @/dev/urandom@. On Windows, it will internally
-- use @RtlGenRandom@.
acquireSeedSystem :: IO [Word32]
acquireSeedSystem = do
#if !defined(mingw32_HOST_OS)
  -- Read 256 random Word32s from /dev/urandom
  let nbytes = 1024
      random = "/dev/urandom"
  allocaBytes nbytes $ \buf -> do
    nread <- withBinaryFile random ReadMode $
               \h -> hGetBuf h buf nbytes
    peekArray (nread `div` 4) buf
#else
  let nbytes = 1024
  -- Generate 256 random Word32s from RtlGenRandom
  allocaBytes nbytes $ \buf -> do
    ok <- c_RtlGenRandom buf (fromIntegral nbytes)
    if ok then return () else fail "Couldn't use RtlGenRandom"
    peekArray (nbytes `div` 4) buf

-- Note: on 64-bit Windows, the 'stdcall' calling convention
-- isn't supported, so we use 'ccall' instead.
#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 architecture!
#endif

-- Note: On Windows, the typical convention would be to use
-- the CryptoGenRandom API in order to generate random data.
-- However, here we use 'SystemFunction036', AKA RtlGenRandom.
--
-- This is a commonly used API for this purpose; one bonus is
-- that it avoids having to bring in the CryptoAPI library,
-- and completely sidesteps the initialization cost of CryptoAPI.
--
-- While this function is technically "subject to change" that is
-- extremely unlikely in practice: rand_s in the Microsoft CRT uses
-- this, and they can't change it easily without also breaking
-- backwards compatibility with e.g. statically linked applications.
--
-- The name 'SystemFunction036' is the actual link-time name; the
-- display name is just for giggles, I guess.
--
-- See also:
--   - http://blogs.msdn.com/b/michael_howard/archive/2005/01/14/353379.aspx
--   - https://bugzilla.mozilla.org/show_bug.cgi?id=504270
--
foreign import WINDOWS_CCONV unsafe "SystemFunction036"
  c_RtlGenRandom :: Ptr a -> CULong -> IO Bool
#endif

-- | Seed a PRNG with data from the system's fast source of pseudo-random
-- numbers.
createSystemRandom :: IO GenIO
createSystemRandom = do
  seed <- acquireSeedSystem
  initialize (I.fromList seed)

-- | Compute the next index into the state pool.  This is simply
-- addition modulo 256.
nextIndex :: Integral a => a -> Int
nextIndex i = fromIntegral j
    where j = fromIntegral (i+1) :: Word8
{-# INLINE nextIndex #-}

aa :: Word64
aa = 1540315826
{-# INLINE aa #-}

data DoubleWord32 = DoubleWord32 {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32

uniformWord32 :: Gen -> IO Word32
uniformWord32 (Gen q) = do
  i  <- nextIndex `liftM` M.unsafeRead q ioff
  c  <- fromIntegral `liftM` M.unsafeRead q coff
  qi <- fromIntegral `liftM` M.unsafeRead q i
  let t  = aa * qi + c
      c' = fromIntegral (t `shiftR` 32)
      x  = fromIntegral t + c'
      (DoubleWord32 x' c'')  | x < c'    = DoubleWord32 (x + 1) (c' + 1)
                             | otherwise = DoubleWord32 x c'
  M.unsafeWrite q i x'
  M.unsafeWrite q ioff (fromIntegral i)
  M.unsafeWrite q coff (fromIntegral c'')
  return x'
{-# INLINE uniformWord32 #-}

uniform1 :: (Word32 -> a) -> Gen -> IO a
uniform1 f gen = do
  i <- uniformWord32 gen
  return $! f i
{-# INLINE uniform1 #-}

uniform2 :: (Word32 -> Word32 -> a) -> Gen -> IO a
uniform2 f (Gen q) = do
  i  <- nextIndex `liftM` M.unsafeRead q ioff
  let j = nextIndex i
  c  <- fromIntegral `liftM` M.unsafeRead q coff
  qi <- fromIntegral `liftM` M.unsafeRead q i
  qj <- fromIntegral `liftM` M.unsafeRead q j
  let t   = aa * qi + c
      c'  = fromIntegral (t `shiftR` 32)
      x   = fromIntegral t + c'
      DoubleWord32 x' c'' | x < c'    = DoubleWord32 (x + 1) (c' + 1)
                          | otherwise = DoubleWord32 x c'
      u   = aa * qj + fromIntegral c''
      d'  = fromIntegral (u `shiftR` 32)
      y   = fromIntegral u + d'
      DoubleWord32 y' d'' | y < d'    = DoubleWord32 (y + 1) (d' + 1)
                          | otherwise = DoubleWord32 y d'
  M.unsafeWrite q i x'
  M.unsafeWrite q j y'
  M.unsafeWrite q ioff (fromIntegral j)
  M.unsafeWrite q coff (fromIntegral d'')
  return $! f x' y'
{-# INLINE uniform2 #-}

-- Type family for fixed size integrals. For signed data types it's
-- its unsigned couterpart with same size and for unsigned data types
-- it's same type
type family Unsigned a :: *

type instance Unsigned Int8  = Word8
type instance Unsigned Int16 = Word16
type instance Unsigned Int32 = Word32
type instance Unsigned Int64 = Word64

type instance Unsigned Word8  = Word8
type instance Unsigned Word16 = Word16
type instance Unsigned Word32 = Word32
type instance Unsigned Word64 = Word64

-- This is workaround for bug #25.
--
-- GHC-7.6 has a bug (#8072) which results in calculation of wrong
-- number of buckets in function `uniformRange'. Consequently uniformR
-- generates values in wrong range.
--
-- Bug only affects 32-bit systems and Int/Word data types. Word32
-- works just fine. So we set Word32 as unsigned counterpart for Int
-- and Word on 32-bit systems. It's done only for GHC-7.6 because
-- other versions are unaffected by the bug and we expect that GHC may
-- optimise code which uses Word better.
#if (WORD_SIZE_IN_BITS < 64) && (__GLASGOW_HASKELL__ == 706)
type instance Unsigned Int   = Word32
type instance Unsigned Word  = Word32
#else
type instance Unsigned Int   = Word
type instance Unsigned Word  = Word
#endif


-- Subtract two numbers under assumption that x>=y and store result in
-- unsigned data type of same size
sub :: (Integral a, Integral (Unsigned a)) => a -> a -> Unsigned a
sub x y = fromIntegral x - fromIntegral y
{-# INLINE sub #-}

add :: (Integral a, Integral (Unsigned a)) => a -> Unsigned a -> a
add m x = m + fromIntegral x
{-# INLINE add #-}

-- Generate uniformly distributed value in inclusive range.
--
-- NOTE: This function must be fully applied. Otherwise it won't be
--       inlined, which will cause a severe performance loss.
--
-- > uniformR     = uniformRange      -- won't be inlined
-- > uniformR a b = uniformRange a b  -- will be inlined
uniformRange :: ( Integral a, Bounded a, Variate a
                , Integral (Unsigned a), Bounded (Unsigned a), Variate (Unsigned a))
             => (a,a) -> Gen -> IO a
uniformRange (x1,x2) g
  | n == 0    = uniform g   -- Abuse overflow in unsigned types
  | otherwise = loop
  where
    -- Allow ranges where x2<x1
    (i, j) | x1 < x2   = (x1, x2)
           | otherwise = (x2, x1)
    n       = 1 + sub j i
    buckets = maxBound `div` n
    maxN    = buckets * n
    loop    = do x <- uniform g
                 if x < maxN then return $! add i (x `div` buckets)
                             else loop
{-# INLINE uniformRange #-}

-- | Generate a vector of pseudo-random variates.  This is not
-- necessarily faster than invoking 'uniform' repeatedly in a loop,
-- but it may be more convenient to use in some situations.
uniformVector :: (Variate a, Vector v a)
             => Gen -> Int -> IO (v a)
uniformVector gen n = G.replicateM n (uniform gen)
{-# INLINE uniformVector #-}

-- | Split a generator into several that can run independently.
splitGen :: Int -> GenIO -> IO [GenIO]
splitGen n gen
  | n <= 0    = return []
  | otherwise =
  fmap (gen:) . replicateM (n-1) $
  initialize =<< uniformVector gen 256

-- $references
--
-- * Marsaglia, G. (2003) Seeds for random number generators.
--   /Communications of the ACM/ 46(5):90&#8211;93.
--   <http://doi.acm.org/10.1145/769800.769827>
--
-- * Doornik, J.A. (2007) Conversion of high-period random numbers to
--   floating point.
--   /ACM Transactions on Modeling and Computer Simulation/ 17(1).
--   <http://www.doornik.com/research/randomdouble.pdf>
