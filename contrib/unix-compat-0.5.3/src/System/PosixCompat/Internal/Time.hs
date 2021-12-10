{-# LANGUAGE CPP #-}
{-
Compatibility wrapper to help manage the transition from
old-time to time packages. Only used at all on win32.
-}
module System.PosixCompat.Internal.Time (
      ClockTime
    , getClockTime
    , clockTimeToEpochTime
    ) where

import System.Posix.Types (EpochTime)

#ifdef OLD_TIME

import System.Time (ClockTime(TOD), getClockTime)

clockTimeToEpochTime :: ClockTime -> EpochTime
clockTimeToEpochTime (TOD s _) = fromInteger s

#else

import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)

type ClockTime = POSIXTime

getClockTime :: IO ClockTime
getClockTime = getPOSIXTime

clockTimeToEpochTime :: ClockTime -> EpochTime
clockTimeToEpochTime = fromInteger . floor

#endif
