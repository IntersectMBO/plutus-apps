{-# LANGUAGE CPP #-}

{-|
This module makes the operations exported by @System.Posix.Files@
available on all platforms. On POSIX systems it re-exports operations from
@System.Posix.Files@. On other platforms it emulates the operations as far
as possible.

/NOTE: the portable implementations are not well tested, in some cases
functions are only stubs./
-}
module System.PosixCompat.Files (
    -- * File modes
    -- FileMode exported by System.Posix.Types
      unionFileModes
    , intersectFileModes
    , nullFileMode
    , ownerReadMode
    , ownerWriteMode
    , ownerExecuteMode
    , ownerModes
    , groupReadMode
    , groupWriteMode
    , groupExecuteMode
    , groupModes
    , otherReadMode
    , otherWriteMode
    , otherExecuteMode
    , otherModes
    , setUserIDMode
    , setGroupIDMode
    , stdFileMode
    , accessModes

    -- ** Setting file modes
    , setFileMode
    , setFdMode
    , setFileCreationMask

    -- ** Checking file existence and permissions
    , fileAccess
    , fileExist

    -- * File status
    , FileStatus
    -- ** Obtaining file status
    , getFileStatus
    , getFdStatus
    , getSymbolicLinkStatus
    -- ** Querying file status
    , deviceID
    , fileID
    , fileMode
    , linkCount
    , fileOwner
    , fileGroup
    , specialDeviceID
    , fileSize
    , accessTime
    , modificationTime
    , statusChangeTime
    , accessTimeHiRes
    , modificationTimeHiRes
    , statusChangeTimeHiRes
    , isBlockDevice
    , isCharacterDevice
    , isNamedPipe
    , isRegularFile
    , isDirectory
    , isSymbolicLink
    , isSocket

    -- * Creation
    , createNamedPipe
    , createDevice

    -- * Hard links
    , createLink
    , removeLink

    -- * Symbolic links
    , createSymbolicLink
    , readSymbolicLink

    -- * Renaming files
    , rename

    -- * Changing file ownership
    , setOwnerAndGroup
    , setFdOwnerAndGroup
    , setSymbolicLinkOwnerAndGroup

    -- * Changing file timestamps
    , setFileTimes
    , touchFile

    -- * Setting file sizes
    , setFileSize
    , setFdSize

    -- * Find system-specific limits for a file
    , PathVar(..)
    , getPathVar
    , getFdPathVar
    ) where

#ifndef mingw32_HOST_OS

#include "HsUnixCompat.h"

import System.Posix.Files

#if NEED_setSymbolicLinkOwnerAndGroup
import System.PosixCompat.Types

setSymbolicLinkOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setSymbolicLinkOwnerAndGroup _ _ _ = return ()
#endif

#else /* Portable implementation */

import Control.Exception (bracket)
import Control.Monad (liftM, liftM2)
import Data.Bits ((.|.), (.&.))
import Data.Int (Int64)
import Data.Time.Clock.POSIX (POSIXTime)
import Foreign.C.Types (CTime(..))
import Prelude hiding (read)
import System.Directory (Permissions, emptyPermissions)
import System.Directory (getPermissions, setPermissions)
import System.Directory (readable, setOwnerReadable)
import System.Directory (writable, setOwnerWritable)
import System.Directory (executable, setOwnerExecutable)
import System.Directory (searchable, setOwnerSearchable)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.IO (IOMode(..), openFile, hSetFileSize, hClose)
import System.IO.Error
import System.PosixCompat.Types
import System.Win32.File hiding (getFileType)
import System.Win32.HardLink (createHardLink)
import System.Win32.Time (FILETIME(..), getFileTime, setFileTime)

import System.PosixCompat.Internal.Time (
      getClockTime, clockTimeToEpochTime
    )

#ifdef __GLASGOW_HASKELL__
import GHC.IO.Handle.FD (fdToHandle)
#endif


unsupported :: String -> IO a
unsupported f = ioError $ mkIOError illegalOperationErrorType x Nothing Nothing
  where
    x = "System.PosixCompat.Files." ++ f ++ ": not supported"

-- -----------------------------------------------------------------------------
-- POSIX file modes

nullFileMode     :: FileMode
nullFileMode     = 0o000000

ownerReadMode    :: FileMode
ownerWriteMode   :: FileMode
ownerExecuteMode :: FileMode
groupReadMode    :: FileMode
groupWriteMode   :: FileMode
groupExecuteMode :: FileMode
otherReadMode    :: FileMode
otherWriteMode   :: FileMode
otherExecuteMode :: FileMode
setUserIDMode    :: FileMode
setGroupIDMode   :: FileMode

ownerReadMode    = 0o000400
ownerWriteMode   = 0o000200
ownerExecuteMode = 0o000100
groupReadMode    = 0o000040
groupWriteMode   = 0o000020
groupExecuteMode = 0o000010
otherReadMode    = 0o000004
otherWriteMode   = 0o000002
otherExecuteMode = 0o000001
setUserIDMode    = 0o004000
setGroupIDMode   = 0o002000

stdFileMode      :: FileMode
ownerModes       :: FileMode
groupModes       :: FileMode
otherModes       :: FileMode
accessModes      :: FileMode

stdFileMode = ownerReadMode  .|. ownerWriteMode .|.
              groupReadMode  .|. groupWriteMode .|.
              otherReadMode  .|. otherWriteMode
ownerModes  = ownerReadMode  .|. ownerWriteMode .|. ownerExecuteMode
groupModes  = groupReadMode  .|. groupWriteMode .|. groupExecuteMode
otherModes  = otherReadMode  .|. otherWriteMode .|. otherExecuteMode
accessModes = ownerModes .|. groupModes .|. otherModes

unionFileModes :: FileMode -> FileMode -> FileMode
unionFileModes m1 m2 = m1 .|. m2

intersectFileModes :: FileMode -> FileMode -> FileMode
intersectFileModes m1 m2 = m1 .&. m2

fileTypeModes :: FileMode
fileTypeModes = 0o0170000

blockSpecialMode     :: FileMode
characterSpecialMode :: FileMode
namedPipeMode        :: FileMode
regularFileMode      :: FileMode
directoryMode        :: FileMode
symbolicLinkMode     :: FileMode
socketMode           :: FileMode

blockSpecialMode     = 0o0060000
characterSpecialMode = 0o0020000
namedPipeMode        = 0o0010000
regularFileMode      = 0o0100000
directoryMode        = 0o0040000
symbolicLinkMode     = 0o0120000
socketMode           = 0o0140000


setFileMode :: FilePath -> FileMode -> IO ()
setFileMode name m = setPermissions name $ modeToPerms m


setFdMode :: Fd -> FileMode -> IO ()
setFdMode _ _ = unsupported "setFdMode"

-- | The portable implementation does nothing and returns 'nullFileMode'.
setFileCreationMask :: FileMode -> IO FileMode
setFileCreationMask _ = return nullFileMode

modeToPerms :: FileMode -> Permissions

#ifdef DIRECTORY_1_0
modeToPerms m = Permissions
    { readable   = m .&. ownerReadMode    /= 0
    , writable   = m .&. ownerWriteMode   /= 0
    , executable = m .&. ownerExecuteMode /= 0
    , searchable = m .&. ownerExecuteMode /= 0 }
#else
modeToPerms m =
    setOwnerReadable   (m .&. ownerReadMode    /= 0) $
    setOwnerWritable   (m .&. ownerWriteMode   /= 0) $
    setOwnerExecutable (m .&. ownerExecuteMode /= 0) $
    setOwnerSearchable (m .&. ownerExecuteMode /= 0) $
    emptyPermissions
#endif

-- -----------------------------------------------------------------------------
-- access()

fileAccess :: FilePath -> Bool -> Bool -> Bool -> IO Bool
fileAccess name read write exec =
    do perm <- getPermissions name
       return $ (not read  || readable perm)
             && (not write || writable perm)
             && (not exec  || executable perm || searchable perm)

fileExist :: FilePath -> IO Bool
fileExist name = liftM2 (||) (doesFileExist name) (doesDirectoryExist name)

-- -----------------------------------------------------------------------------
-- stat() support

data FileStatus = FileStatus
    { deviceID              :: DeviceID
    , fileID                :: FileID
    , fileMode              :: FileMode
    , linkCount             :: LinkCount
    , fileOwner             :: UserID
    , fileGroup             :: GroupID
    , specialDeviceID       :: DeviceID
    , fileSize              :: FileOffset
    , accessTime            :: EpochTime
    , modificationTime      :: EpochTime
    , statusChangeTime      :: EpochTime
    , accessTimeHiRes       :: POSIXTime
    , modificationTimeHiRes :: POSIXTime
    , statusChangeTimeHiRes :: POSIXTime
    }

isBlockDevice :: FileStatus -> Bool
isBlockDevice stat =
    (fileMode stat `intersectFileModes` fileTypeModes) == blockSpecialMode

isCharacterDevice :: FileStatus -> Bool
isCharacterDevice stat =
    (fileMode stat `intersectFileModes` fileTypeModes) == characterSpecialMode

isNamedPipe :: FileStatus -> Bool
isNamedPipe stat =
    (fileMode stat `intersectFileModes` fileTypeModes) == namedPipeMode

isRegularFile :: FileStatus -> Bool
isRegularFile stat =
    (fileMode stat `intersectFileModes` fileTypeModes) == regularFileMode

isDirectory :: FileStatus -> Bool
isDirectory stat =
    (fileMode stat `intersectFileModes` fileTypeModes) == directoryMode

isSymbolicLink :: FileStatus -> Bool
isSymbolicLink stat =
    (fileMode stat `intersectFileModes` fileTypeModes) == symbolicLinkMode

isSocket :: FileStatus -> Bool
isSocket stat =
    (fileMode stat `intersectFileModes` fileTypeModes) == socketMode

getFileStatus :: FilePath -> IO FileStatus
getFileStatus path = do
    perm  <- liftM permsToMode (getPermissions path)
    typ   <- getFileType path
    info  <- bracket openPath closeHandle getFileInformationByHandle
    let atime = windowsToPosixTime (bhfiLastAccessTime info)
        mtime = windowsToPosixTime (bhfiLastWriteTime info)
        ctime = windowsToPosixTime (bhfiCreationTime info)
    return $ FileStatus
             { deviceID         = fromIntegral (bhfiVolumeSerialNumber info)
             , fileID           = fromIntegral (bhfiFileIndex info)
             , fileMode         = typ .|. perm
             , linkCount        = fromIntegral (bhfiNumberOfLinks info)
             , fileOwner        = 0
             , fileGroup        = 0
             , specialDeviceID  = 0
             , fileSize         = fromIntegral (bhfiSize info)
             , accessTime       = posixTimeToEpochTime atime
             , modificationTime = posixTimeToEpochTime mtime
             , statusChangeTime = posixTimeToEpochTime mtime
             , accessTimeHiRes       = atime
             , modificationTimeHiRes = mtime
             , statusChangeTimeHiRes = ctime
             }
  where
    openPath = createFile path
                 gENERIC_READ
                 (fILE_SHARE_READ .|. fILE_SHARE_WRITE .|. fILE_SHARE_DELETE)
                 Nothing
                 oPEN_EXISTING
                 (sECURITY_ANONYMOUS .|. fILE_FLAG_BACKUP_SEMANTICS)
                 Nothing

-- | Convert a 'POSIXTime' (synomym for 'Data.Time.Clock.NominalDiffTime')
-- into an 'EpochTime' (integral number of seconds since epoch). This merely
-- throws away the fractional part.
posixTimeToEpochTime :: POSIXTime -> EpochTime
posixTimeToEpochTime = fromInteger . floor

-- three function stolen from System.Directory.Internals.Windows:

-- | Difference between the Windows and POSIX epochs in units of 100ns.
windowsPosixEpochDifference :: Num a => a
windowsPosixEpochDifference = 116444736000000000

-- | Convert from Windows time to POSIX time.
windowsToPosixTime :: FILETIME -> POSIXTime
windowsToPosixTime (FILETIME t) =
  (fromIntegral t - windowsPosixEpochDifference) / 10000000

{- will be needed to /set/ high res timestamps, not yet supported

-- | Convert from POSIX time to Windows time.  This is lossy as Windows time
--   has a resolution of only 100ns.
posixToWindowsTime :: POSIXTime -> FILETIME
posixToWindowsTime t = FILETIME $
  truncate (t * 10000000 + windowsPosixEpochDifference)
-}

permsToMode :: Permissions -> FileMode
permsToMode perms = r .|. w .|. x
  where
    r = f (readable perms) (ownerReadMode .|. groupReadMode .|. otherReadMode)
    w = f (writable perms) (ownerWriteMode .|. groupWriteMode .|. otherWriteMode)
    x = f (executable perms || searchable perms)
          (ownerExecuteMode .|. groupExecuteMode .|. otherExecuteMode)
    f True m  = m
    f False _ = nullFileMode

getFileType :: FilePath -> IO FileMode
getFileType path =
    do f <- doesFileExist path
       if f then return regularFileMode
            else do d <- doesDirectoryExist path
                    if d then return directoryMode
                         else unsupported "Unknown file type."

getFdStatus :: Fd -> IO FileStatus
getFdStatus _ = unsupported "getFdStatus"

getSymbolicLinkStatus :: FilePath -> IO FileStatus
getSymbolicLinkStatus path = getFileStatus path

createNamedPipe :: FilePath -> FileMode -> IO ()
createNamedPipe _ _ = unsupported "createNamedPipe"

createDevice :: FilePath -> FileMode -> DeviceID -> IO ()
createDevice _ _ _ = unsupported "createDevice"

-- -----------------------------------------------------------------------------
-- Hard links

createLink :: FilePath -> FilePath -> IO ()
createLink = createHardLink

removeLink :: FilePath -> IO ()
removeLink _ = unsupported "removeLink"

-- -----------------------------------------------------------------------------
-- Symbolic Links

createSymbolicLink :: FilePath -> FilePath -> IO ()
createSymbolicLink _ _ = unsupported "createSymbolicLink"

readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink _ = unsupported "readSymbolicLink"

-- -----------------------------------------------------------------------------
-- Renaming

rename :: FilePath -> FilePath -> IO ()
#if MIN_VERSION_Win32(2, 6, 0)
rename name1 name2 = moveFileEx name1 (Just name2) mOVEFILE_REPLACE_EXISTING
#else
rename name1 name2 = moveFileEx name1 name2 mOVEFILE_REPLACE_EXISTING
#endif

-- -----------------------------------------------------------------------------
-- chown()

-- | The portable implementation does nothing.
setOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setOwnerAndGroup _ _ _ = return ()

-- | The portable implementation does nothing.
setFdOwnerAndGroup :: Fd -> UserID -> GroupID -> IO ()
setFdOwnerAndGroup _ _ _ = return ()

-- | The portable implementation does nothing.
setSymbolicLinkOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setSymbolicLinkOwnerAndGroup _ _ _ = return ()

-- -----------------------------------------------------------------------------
-- utime()

setFileTimes :: FilePath -> EpochTime -> EpochTime -> IO ()
setFileTimes file atime mtime =
  bracket openFileHandle closeHandle $ \handle -> do
    (creationTime, _, _) <- getFileTime handle
    setFileTime
      handle
      creationTime
      (epochTimeToFileTime atime)
      (epochTimeToFileTime mtime)
  where
    openFileHandle = createFile file
                       gENERIC_WRITE
                       fILE_SHARE_NONE
                       Nothing
                       oPEN_EXISTING
                       fILE_ATTRIBUTE_NORMAL
                       Nothing

    -- based on https://support.microsoft.com/en-us/kb/167296
    epochTimeToFileTime (CTime t) = FILETIME (fromIntegral ll)
      where
        ll :: Int64
        ll = fromIntegral t * 10000000 + 116444736000000000

touchFile :: FilePath -> IO ()
touchFile name =
    do t <- liftM clockTimeToEpochTime getClockTime
       setFileTimes name t t

-- -----------------------------------------------------------------------------
-- Setting file sizes

setFileSize :: FilePath -> FileOffset -> IO ()
setFileSize file off =
    bracket (openFile file WriteMode) (hClose)
            (\h -> hSetFileSize h (fromIntegral off))

setFdSize :: Fd -> FileOffset -> IO ()
#ifdef __GLASGOW_HASKELL__
setFdSize (Fd fd) off =
    do h <- fdToHandle (fromIntegral fd)
       hSetFileSize h (fromIntegral off)
#else
setFdSize fd off = unsupported "setFdSize"
#endif

-- -----------------------------------------------------------------------------
-- pathconf()/fpathconf() support

data PathVar
  = FileSizeBits                  -- _PC_FILESIZEBITS
  | LinkLimit                     -- _PC_LINK_MAX
  | InputLineLimit                -- _PC_MAX_CANON
  | InputQueueLimit               -- _PC_MAX_INPUT
  | FileNameLimit                 -- _PC_NAME_MAX
  | PathNameLimit                 -- _PC_PATH_MAX
  | PipeBufferLimit               -- _PC_PIPE_BUF

  -- These are described as optional in POSIX:
                                  -- _PC_ALLOC_SIZE_MIN
                                  -- _PC_REC_INCR_XFER_SIZE
                                  -- _PC_REC_MAX_XFER_SIZE
                                  -- _PC_REC_MIN_XFER_SIZE
                                  -- _PC_REC_XFER_ALIGN
  | SymbolicLinkLimit             -- _PC_SYMLINK_MAX
  | SetOwnerAndGroupIsRestricted  -- _PC_CHOWN_RESTRICTED
  | FileNamesAreNotTruncated      -- _PC_NO_TRUNC
  | VDisableChar                  -- _PC_VDISABLE
  | AsyncIOAvailable              -- _PC_ASYNC_IO
  | PrioIOAvailable               -- _PC_PRIO_IO
  | SyncIOAvailable               -- _PC_SYNC_IO

getPathVar :: FilePath -> PathVar -> IO Limit
getPathVar _ _ = unsupported "getPathVar"

getFdPathVar :: Fd -> PathVar -> IO Limit
getFdPathVar _ _ = unsupported "getFdPathVar"

#endif
