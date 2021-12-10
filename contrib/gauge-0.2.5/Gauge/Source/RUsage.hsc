-- |
-- Module      : Gauge.Source.RUsage
-- Copyright   : (c) 2017 Vincent Hanquez
--
-- A bindings to POSIX getrusage()
--

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Gauge.Source.RUsage
    ( Who
    , pattern Self
    , pattern Children
    , RUsage(..)
    , TimeVal(..)
    , get
    , with
    , supported
    ) where

#ifndef mingw32_HOST_OS
#define SUPPORT_RUSAGE
#endif

#ifdef SUPPORT_RUSAGE

import Control.Applicative
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc

#include <sys/time.h>
#include <sys/resource.h>

#else

#endif

import Gauge.Time (MicroSeconds(..))
import Foreign.C.Types
import Data.Word
import Prelude -- Silence redundant import warnings

{- struct rusage :
struct timeval ru_utime; /* user CPU time used */
struct timeval ru_stime; /* system CPU time used */
long   ru_maxrss;        /* maximum resident set size */
long   ru_ixrss;         /* integral shared memory size */
long   ru_idrss;         /* integral unshared data size */
long   ru_isrss;         /* integral unshared stack size */
long   ru_minflt;        /* page reclaims (soft page faults) */
long   ru_majflt;        /* page faults (hard page faults) */
long   ru_nswap;         /* swaps */
long   ru_inblock;       /* block input operations */
long   ru_oublock;       /* block output operations */
long   ru_msgsnd;        /* IPC messages sent */
long   ru_msgrcv;        /* IPC messages received */
long   ru_nsignals;      /* signals received */
long   ru_nvcsw;         /* voluntary context switches */
long   ru_nivcsw;        /* involuntary context switches */
-}

data RUsage = RUsage
    { userCpuTime               :: {-# UNPACK #-} !TimeVal
    , systemCpuTime             :: {-# UNPACK #-} !TimeVal
    , maxResidentSetSize        :: {-# UNPACK #-} !Word64
    , iSharedMemorySize         :: {-# UNPACK #-} !Word64
    , iUnsharedDataSize         :: {-# UNPACK #-} !Word64
    , iUnsharedStackSize        :: {-# UNPACK #-} !Word64
    , minorFault                :: {-# UNPACK #-} !Word64
    , majorFault                :: {-# UNPACK #-} !Word64
    , nSwap                     :: {-# UNPACK #-} !Word64
    , inBlock                   :: {-# UNPACK #-} !Word64
    , outBlock                  :: {-# UNPACK #-} !Word64
    , msgSend                   :: {-# UNPACK #-} !Word64
    , msgRecv                   :: {-# UNPACK #-} !Word64
    , nSignals                  :: {-# UNPACK #-} !Word64
    , nVoluntaryContextSwitch   :: {-# UNPACK #-} !Word64
    , nInvoluntaryContextSwitch :: {-# UNPACK #-} !Word64
    } deriving (Show, Eq)

newtype TimeVal = TimeVal MicroSeconds
    deriving (Show,Eq)

#ifdef SUPPORT_RUSAGE

instance Storable RUsage where
    alignment _ = 8
    sizeOf _ = sizeRUsage
    peek p = RUsage <$> (#peek struct rusage, ru_utime) p
                    <*> (#peek struct rusage, ru_stime) p
                    <*> (clongToW64 <$> ( (#peek struct rusage, ru_maxrss  ) p) )
                    <*> (clongToW64 <$> ( (#peek struct rusage, ru_ixrss   ) p) )
                    <*> (clongToW64 <$> ( (#peek struct rusage, ru_idrss   ) p) )
                    <*> (clongToW64 <$> ( (#peek struct rusage, ru_isrss   ) p) )
                    <*> (clongToW64 <$> ( (#peek struct rusage, ru_minflt  ) p) )
                    <*> (clongToW64 <$> ( (#peek struct rusage, ru_majflt  ) p) )
                    <*> (clongToW64 <$> ( (#peek struct rusage, ru_nswap   ) p) )
                    <*> (clongToW64 <$> ( (#peek struct rusage, ru_inblock ) p) )
                    <*> (clongToW64 <$> ( (#peek struct rusage, ru_oublock ) p) )
                    <*> (clongToW64 <$> ( (#peek struct rusage, ru_msgsnd  ) p) )
                    <*> (clongToW64 <$> ( (#peek struct rusage, ru_msgrcv  ) p) )
                    <*> (clongToW64 <$> ( (#peek struct rusage, ru_nsignals) p) )
                    <*> (clongToW64 <$> ( (#peek struct rusage, ru_nvcsw   ) p) )
                    <*> (clongToW64 <$> ( (#peek struct rusage, ru_nivcsw  ) p) )
      where

    poke p (RUsage utime stime maxrss ixrss idrss isrss minflt majflt nswap
            inblock oublock msgsnd msgrcv nsignals nvcsw nivcsw) = do
        (#poke struct rusage, ru_utime)    p utime
        (#poke struct rusage, ru_stime)    p stime
        (#poke struct rusage, ru_maxrss)   p (w64ToCLong maxrss)
        (#poke struct rusage, ru_ixrss)    p (w64ToCLong ixrss)
        (#poke struct rusage, ru_idrss)    p (w64ToCLong idrss)
        (#poke struct rusage, ru_isrss)    p (w64ToCLong isrss)
        (#poke struct rusage, ru_minflt)   p (w64ToCLong minflt)
        (#poke struct rusage, ru_majflt)   p (w64ToCLong majflt)
        (#poke struct rusage, ru_nswap)    p (w64ToCLong nswap)
        (#poke struct rusage, ru_inblock)  p (w64ToCLong inblock)
        (#poke struct rusage, ru_oublock)  p (w64ToCLong oublock)
        (#poke struct rusage, ru_msgsnd)   p (w64ToCLong msgsnd)
        (#poke struct rusage, ru_msgrcv)   p (w64ToCLong msgrcv)
        (#poke struct rusage, ru_nsignals) p (w64ToCLong nsignals)
        (#poke struct rusage, ru_nvcsw)    p (w64ToCLong nvcsw)
        (#poke struct rusage, ru_nivcsw)   p (w64ToCLong nivcsw)

instance Storable TimeVal where
    alignment _ = 8
    sizeOf _ = #const sizeof(struct timeval)
    peek p = toTimeVal <$> (#peek struct timeval, tv_sec) p
                       <*> (#peek struct timeval, tv_usec) p
      where toTimeVal !s !us = TimeVal $! MicroSeconds $! (clongToW64 s * secondsToMicroScale) + clongToW64 us
    poke p (TimeVal (MicroSeconds cus)) = do
        (#poke struct timeval, tv_sec) p (w64ToCLong s)
        (#poke struct timeval, tv_usec) p (w64ToCLong us)
      where (s, us) = cus `divMod` secondsToMicroScale

secondsToMicroScale :: Word64
secondsToMicroScale = 1000000

w64ToCLong :: Word64 -> CLong
w64ToCLong = fromIntegral

clongToW64 :: CLong -> Word64
clongToW64 = fromIntegral

sizeRUsage :: Int
sizeRUsage = #const sizeof(struct rusage)

#if __GLASGOW_HASKELL__ >= 710
pattern Self :: Who
#endif
pattern Self = (#const RUSAGE_SELF) :: Who

#if __GLASGOW_HASKELL__ >= 710
pattern Children :: Who
#endif
pattern Children = (#const RUSAGE_CHILDREN) :: Who

type Who = CInt

-- | Gather RUsage
get :: Who -> IO RUsage
get who = alloca $ \ptr -> do
    throwErrnoIfMinus1_ "getrusage" (binding_getrusage who ptr)
    peek ptr

-- | call a function `f` gathering RUSage before and after the call.
with :: Who -> IO a -> IO (a, RUsage, RUsage)
with who f = allocaBytes (sizeRUsage * 2) $ \ptr -> do
    let ptr2 = ptr `plusPtr` sizeRUsage
    throwErrnoIfMinus1_ "getrusage" (binding_getrusage who ptr)
    a <- f
    throwErrnoIfMinus1_ "getrusage" (binding_getrusage who ptr2)
    (,,) <$> pure a <*> peek ptr <*> peek ptr2

-- binding for: int getrusage(int who, struct rusage *usage);
foreign import ccall unsafe "getrusage"
    binding_getrusage :: Who -> Ptr RUsage -> IO CInt

-- | On operating system not supporting getrusage this will be False, otherwise True.
supported :: Bool
supported = True

#else

#if __GLASGOW_HASKELL__ >= 710
pattern Self :: Who
#endif
pattern Self = 1 :: Who

#if __GLASGOW_HASKELL__ >= 710
pattern Children :: Who
#endif
pattern Children = 2 :: Who

type Who = CInt

get :: Who -> IO RUsage
get _ = pure rusageEmpty

with :: Who -> IO a -> IO (a, RUsage, RUsage)
with _ f = (,,) <$> f <*> pure rusageEmpty <*> pure rusageEmpty

rusageEmpty :: RUsage
rusageEmpty = RUsage ms0 ms0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  where ms0 = TimeVal $ MicroSeconds 0

supported :: Bool
supported = False

#endif
