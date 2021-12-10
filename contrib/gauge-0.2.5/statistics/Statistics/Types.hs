{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-- |
-- Module    : Statistics.Types
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Data types common used in statistics
module Statistics.Types
    ( -- * Confidence level
      CL
      -- ** Accessors
    , confidenceLevel
    , significanceLevel
      -- ** Constructors
    , mkCL
      -- ** Constants and conversion to nσ
    , cl95
      -- * Estimates and upper/lower limits
    , Estimate(..)
    -- , NormalErr(..)
    , ConfInt(..)
      -- ** Constructors
    -- , estimateNormErr
    , estimateFromInterval
    , estimateFromErr
      -- ** Accessors
    , confidenceInterval
    , Scale(..)
      -- * Other
    , Sample
    ) where

import Control.DeepSeq              (NFData(..))
import Data.Data                    (Data,Typeable)
import Data.Maybe                   (fromMaybe)
import GHC.Generics                 (Generic)

#if __GLASGOW_HASKELL__ == 704
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
#endif

import Statistics.Internal
import Statistics.Types.Internal


----------------------------------------------------------------
-- Data type for confidence level
----------------------------------------------------------------

-- |
-- Confidence level. In context of confidence intervals it's
-- probability of said interval covering true value of measured
-- value. In context of statistical tests it's @1-α@ where α is
-- significance of test.
--
-- Since confidence level are usually close to 1 they are stored as
-- @1-CL@ internally. There are two smart constructors for @CL@:
-- 'mkCL' and 'mkCLFromSignificance' (and corresponding variant
-- returning @Maybe@). First creates @CL@ from confidence level and
-- second from @1 - CL@ or significance level.
--
-- >>> cl95
-- mkCLFromSignificance 0.05
--
-- Prior to 0.14 confidence levels were passed to function as plain
-- @Doubles@. Use 'mkCL' to convert them to @CL@.
newtype CL a = CL a
               deriving (Eq, Typeable, Data, Generic)

instance Show a => Show (CL a) where
  showsPrec n (CL p) = defaultShow1 "mkCLFromSignificance" p n
instance (Num a, Ord a, Read a) => Read (CL a) where
  readPrec = defaultReadPrecM1 "mkCLFromSignificance" mkCLFromSignificanceE

instance NFData   a => NFData   (CL a) where
  rnf (CL a) = rnf a

-- |
-- >>> cl95 > cl90
-- True
instance Ord a => Ord (CL a) where
  CL a <  CL b = a >  b
  CL a <= CL b = a >= b
  CL a >  CL b = a <  b
  CL a >= CL b = a <= b
  max (CL a) (CL b) = CL (min a b)
  min (CL a) (CL b) = CL (max a b)


-- | Create confidence level from probability β or probability
--   confidence interval contain true value of estimate. Will throw
--   exception if parameter is out of [0,1] range
--
-- >>> mkCL 0.95    -- same as cl95
-- mkCLFromSignificance 0.05
mkCL :: (Ord a, Num a) => a -> CL a
mkCL
  = fromMaybe (error "Statistics.Types.mkCL: probability is out if [0,1] range")
  . mkCLE

-- | Same as 'mkCL' but returns @Nothing@ instead of error if
--   parameter is out of [0,1] range
--
-- >>> mkCLE 0.95    -- same as cl95
-- Just (mkCLFromSignificance 0.05)
mkCLE :: (Ord a, Num a) => a -> Maybe (CL a)
mkCLE p
  | p >= 0 && p <= 1 = Just $ CL (1 - p)
  | otherwise        = Nothing

-- | Same as 'mkCLFromSignificance' but returns @Nothing@ instead of error if
--   parameter is out of [0,1] range
--
-- >>> mkCLFromSignificanceE 0.05    -- same as cl95
-- Just (mkCLFromSignificance 0.05)
mkCLFromSignificanceE :: (Ord a, Num a) => a -> Maybe (CL a)
mkCLFromSignificanceE p
  | p >= 0 && p <= 1 = Just $ CL p
  | otherwise        = Nothing

-- | Get confidence level. This function is subject to rounding
--   errors. If @1 - CL@ is needed use 'significanceLevel' instead
confidenceLevel :: (Num a) => CL a -> a
confidenceLevel (CL p) = 1 - p

-- | Get significance level.
significanceLevel :: CL a -> a
significanceLevel (CL p) = p



-- | 95% confidence level
cl95 :: Fractional a => CL a
cl95 = CL 0.05

----------------------------------------------------------------
-- Data type for p-value
----------------------------------------------------------------

-- | Newtype wrapper for p-value.
newtype PValue a = PValue a
               deriving (Eq,Ord, Typeable, Data, Generic)

instance Show a => Show (PValue a) where
  showsPrec n (PValue p) = defaultShow1 "mkPValue" p n
instance (Num a, Ord a, Read a) => Read (PValue a) where
  readPrec = defaultReadPrecM1 "mkPValue" mkPValueE

instance NFData a => NFData (PValue a) where
  rnf (PValue a) = rnf a


-- | Construct PValue. Returns @Nothing@ if argument is out of [0,1] range.
mkPValueE :: (Ord a, Num a) => a -> Maybe (PValue a)
mkPValueE p
  | p >= 0 && p <= 1 = Just $ PValue p
  | otherwise        = Nothing

----------------------------------------------------------------
-- Point estimates
----------------------------------------------------------------

-- |
-- A point estimate and its confidence interval. It's parametrized by
-- both error type @e@ and value type @a@. This module provides two
-- types of error: 'NormalErr' for normally distributed errors and
-- 'ConfInt' for error with normal distribution. See their
-- documentation for more details.
--
-- For example @144 ± 5@ (assuming normality) could be expressed as
--
-- > Estimate { estPoint = 144
-- >          , estError = NormalErr 5
-- >          }
--
-- Or if we want to express @144 + 6 - 4@ at CL95 we could write:
--
-- > Estimate { estPoint = 144
-- >          , estError = ConfInt
-- >                       { confIntLDX = 4
-- >                       , confIntUDX = 6
-- >                       , confIntCL  = cl95
-- >                       }
--
-- Prior to statistics 0.14 @Estimate@ data type used following definition:
--
-- > data Estimate = Estimate {
-- >      estPoint           :: {-# UNPACK #-} !Double
-- >    , estLowerBound      :: {-# UNPACK #-} !Double
-- >    , estUpperBound      :: {-# UNPACK #-} !Double
-- >    , estConfidenceLevel :: {-# UNPACK #-} !Double
-- >    }
--
-- Now type @Estimate ConfInt Double@ should be used instead. Function
-- 'estimateFromInterval' allow to easily construct estimate from same inputs.
data Estimate e a = Estimate
    { estPoint           :: !a
      -- ^ Point estimate.
    , estError           :: !(e a)
      -- ^ Confidence interval for estimate.
    } deriving (Eq, Read, Show, Generic
#if __GLASGOW_HASKELL__ >= 708
               , Typeable, Data
#endif
               )

instance (NFData   (e a), NFData   a) => NFData   (Estimate e a) where
    rnf (Estimate x dx) = rnf x `seq` rnf dx


-- | Confidence interval. It assumes that confidence interval forms
--   single interval and isn't set of disjoint intervals.
data ConfInt a = ConfInt
  { confIntLDX :: !a
    -- ^ Lower error estimate, or distance between point estimate and
    --   lower bound of confidence interval.
  , confIntUDX :: !a
    -- ^ Upper error estimate, or distance between point estimate and
    --   upper bound of confidence interval.
  , confIntCL  :: !(CL Double)
    -- ^ Confidence level corresponding to given confidence interval.
  }
  deriving (Read,Show,Eq,Typeable,Data,Generic)

instance NFData   a => NFData   (ConfInt a) where
    rnf (ConfInt x y _) = rnf x `seq` rnf y



----------------------------------------
-- Constructors

-- | Create estimate with asymmetric error.
estimateFromErr
  :: a                     -- ^ Central estimate
  -> (a,a)                 -- ^ Lower and upper errors. Both should be
                           --   positive but it's not checked.
  -> CL Double             -- ^ Confidence level for interval
  -> Estimate ConfInt a
estimateFromErr x (ldx,udx) cl = Estimate x (ConfInt ldx udx cl)

-- | Create estimate with asymmetric error.
estimateFromInterval
  :: Num a
  => a                     -- ^ Point estimate. Should lie within
                           --   interval but it's not checked.
  -> (a,a)                 -- ^ Lower and upper bounds of interval
  -> CL Double             -- ^ Confidence level for interval
  -> Estimate ConfInt a
estimateFromInterval x (lx,ux) cl
  = Estimate x (ConfInt (x-lx) (ux-x) cl)


----------------------------------------
-- Accessors

-- | Get confidence interval
confidenceInterval :: Num a => Estimate ConfInt a -> (a,a)
confidenceInterval (Estimate x (ConfInt ldx udx _))
  = (x - ldx, x + udx)


-- | Data types which could be multiplied by constant.
class Scale e where
  scale :: (Ord a, Num a) => a -> e a -> e a

instance Scale ConfInt where
  scale a (ConfInt l u cl) | a >= 0    = ConfInt  (a*l)  (a*u) cl
                           | otherwise = ConfInt (-a*u) (-a*l) cl

instance Scale e => Scale (Estimate e) where
  scale a (Estimate x dx) = Estimate (a*x) (scale a dx)

