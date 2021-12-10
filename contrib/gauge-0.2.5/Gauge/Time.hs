{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Gauge.Time
    ( MicroSeconds(..)
    , MilliSeconds(..)
    , NanoSeconds(..)
    , PicoSeconds100(..)
    -- * Convertion functions
    , microSecondsToDouble
    , milliSecondsToDouble
    , nanoSecondsToDouble
    , picosecondsToNanoSeconds
    , doubleToNanoSeconds
    , doubleToPicoseconds100
    ) where

import           Data.Typeable
import           Data.Data
import           Data.Word
import           Control.DeepSeq
import           GHC.Generics
import           Gauge.Optional (OptionalTag)

-- | Represent a number of milliseconds.
newtype MilliSeconds = MilliSeconds Word64
    deriving (Eq, Read, Show, Typeable, Data, Generic, NFData, Enum, Bounded, Num, OptionalTag)

-- | Represent a number of microseconds
newtype MicroSeconds = MicroSeconds Word64
    deriving (Eq, Read, Show, Typeable, Data, Generic, NFData, Enum, Bounded, Num, OptionalTag)

-- | Represent a number of nanoseconds
newtype NanoSeconds = NanoSeconds Word64
    deriving (Eq, Read, Show, Typeable, Data, Generic, NFData, Enum, Bounded, Num, OptionalTag)

-- | Represent a number of hundreds of picoseconds
newtype PicoSeconds100 = PicoSeconds100 Word64
    deriving (Eq, Read, Show, Typeable, Data, Generic, NFData, Enum, Bounded, Num, OptionalTag)

ref_picoseconds100 :: Num a => a
ref_picoseconds100 = 10000000000

ref_nanoseconds :: Num a => a
ref_nanoseconds = 1000000000

ref_microseconds :: Num a => a
ref_microseconds = 1000000

ref_milliseconds :: Num a => a
ref_milliseconds = 1000

microSecondsToDouble :: MicroSeconds -> Double
microSecondsToDouble (MicroSeconds w) = fromIntegral w / ref_microseconds

milliSecondsToDouble :: MilliSeconds -> Double
milliSecondsToDouble (MilliSeconds w) = fromIntegral w / ref_milliseconds

nanoSecondsToDouble :: NanoSeconds -> Double
nanoSecondsToDouble (NanoSeconds w) = fromIntegral w / ref_nanoseconds

doubleToNanoSeconds :: Double -> NanoSeconds
doubleToNanoSeconds w = NanoSeconds $ truncate (w * ref_nanoseconds)

-- | Return the number of integral nanoseconds followed by the number of hundred of picoseconds (1 digit)
picosecondsToNanoSeconds :: PicoSeconds100 -> (NanoSeconds, Word)
picosecondsToNanoSeconds (PicoSeconds100 p) = (NanoSeconds ns, fromIntegral fragment)
  where (ns, fragment) = p `divMod` 10

doubleToPicoseconds100 :: Double -> PicoSeconds100
doubleToPicoseconds100 w = PicoSeconds100 $ round (w * ref_picoseconds100)
