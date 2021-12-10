{-# LANGUAGE CPP #-}

{-|
This module makes the operations exported by @System.Posix.Time@
available on all platforms. On POSIX systems it re-exports operations from
@System.Posix.Time@, on other platforms it emulates the operations as far
as possible.
-}
module System.PosixCompat.Time (
      epochTime
    ) where

#ifndef mingw32_HOST_OS

import System.Posix.Time

#else

import Control.Monad (liftM)
import System.Posix.Types (EpochTime)

import System.PosixCompat.Internal.Time (
      getClockTime, clockTimeToEpochTime
    )

-- | The portable version of @epochTime@ calls 'getClockTime' to obtain the
--   number of seconds that have elapsed since the epoch (Jan 01 00:00:00 GMT
--   1970).
epochTime :: IO EpochTime
epochTime = liftM clockTimeToEpochTime getClockTime

#endif

