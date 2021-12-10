{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{-|
This module makes the operations exported by @System.Posix.Temp@
available on all platforms. On POSIX systems it re-exports operations from
@System.Posix.Temp@, on other platforms it emulates the operations as far
as possible.
-}
module System.PosixCompat.Temp (
      mkstemp
    ) where

#ifndef mingw32_HOST_OS
-- Re-export unix package

import System.Posix.Temp

#elif defined(__GLASGOW_HASKELL__)
-- Windows w/ GHC, we have fdToHandle so we
-- can use our own implementation of mkstemp.

import System.IO (Handle)
import Foreign.C (CInt(..), CString, withCString, peekCString, throwErrnoIfMinus1)
import GHC.IO.Handle.FD (fdToHandle)

-- | 'mkstemp' - make a unique filename and open it for
-- reading\/writing.
-- The returned 'FilePath' is the (possibly relative) path of
-- the created file, which is padded with 6 random characters.
mkstemp :: String -> IO (FilePath, Handle)
mkstemp template = do
  withCString template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemp" (c_mkstemp ptr)
    name <- peekCString ptr
    h <- fdToHandle (fromIntegral fd)
    return (name, h)

foreign import ccall unsafe "unixcompat_mkstemp"
    c_mkstemp :: CString -> IO CInt

#else
-- Windows w/o GHC, we don't have fdToHandle :(

import System.IO (Handle)
import System.IO.Error (mkIOError, illegalOperationErrorType)

mkstemp :: String -> IO (FilePath, Handle)
mkstemp _ = ioError $ mkIOError illegalOperationErrorType x Nothing Nothing
  where
    x = "System.PosixCompat.Temp.mkstemp: not supported"

#endif
