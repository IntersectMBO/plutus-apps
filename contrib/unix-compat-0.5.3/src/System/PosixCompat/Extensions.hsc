{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | This module provides some functions not present in the unix package.
module System.PosixCompat.Extensions (
    -- * Device IDs.
      CMajor
    , CMinor
    , deviceMajor
    , deviceMinor
    , makeDeviceID
    ) where


#ifndef mingw32_HOST_OS
#include "HsUnixCompat.h"
#endif

import Foreign.C.Types
import System.PosixCompat.Types


type CMajor = CUInt
type CMinor = CUInt

-- | Gets the major number from a 'DeviceID' for a device file.
--
-- The portable implementation always returns @0@.
deviceMajor :: DeviceID -> CMajor
#ifdef mingw32_HOST_OS
deviceMajor _ = 0
#else
deviceMajor dev = unix_major dev

foreign import ccall unsafe "unix_major" unix_major :: CDev -> CUInt
#endif

-- | Gets the minor number from a 'DeviceID' for a device file.
--
-- The portable implementation always returns @0@.
deviceMinor :: DeviceID -> CMinor
#ifdef mingw32_HOST_OS
deviceMinor _ = 0
#else
deviceMinor dev = unix_minor dev

foreign import ccall unsafe "unix_minor" unix_minor :: CDev -> CUInt
#endif

-- | Creates a 'DeviceID' for a device file given a major and minor number.
makeDeviceID :: CMajor -> CMinor -> DeviceID
#ifdef mingw32_HOST_OS
makeDeviceID _ _ = 0
#else
makeDeviceID ma mi = unix_makedev ma mi

foreign import ccall unsafe "unix_makedev" unix_makedev :: CUInt -> CUInt -> CDev
#endif
