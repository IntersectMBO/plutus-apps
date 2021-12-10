{-# LANGUAGE CPP #-}

{-|
This module makes the operations exported by @System.Posix.User@
available on all platforms. On POSIX systems it re-exports operations from
@System.Posix.User@. On other platforms it provides dummy implementations.
-}
module System.PosixCompat.User (
    -- * User environment
    -- ** Querying the user environment
      getRealUserID
    , getRealGroupID
    , getEffectiveUserID
    , getEffectiveGroupID
    , getGroups
    , getLoginName
    , getEffectiveUserName

    -- *** The group database
    , GroupEntry(..)
    , getGroupEntryForID
    , getGroupEntryForName
    , getAllGroupEntries

    -- *** The user database
    , UserEntry(..)
    , getUserEntryForID
    , getUserEntryForName
    , getAllUserEntries

    -- ** Modifying the user environment
    , setUserID
    , setGroupID
    ) where

#ifndef mingw32_HOST_OS

#include "HsUnixCompat.h"

import System.Posix.User

#if __GLASGOW_HASKELL__<605
getAllGroupEntries :: IO [GroupEntry]
getAllGroupEntries = return []

getAllUserEntries :: IO [UserEntry]
getAllUserEntries = return []
#endif

#else /* Portable implementation */

import System.IO.Error
import System.PosixCompat.Types

unsupported :: String -> IO a
unsupported f = ioError $ mkIOError illegalOperationErrorType x Nothing Nothing
    where x = "System.PosixCompat.User." ++ f ++ ": not supported"

-- -----------------------------------------------------------------------------
-- User environment

getRealUserID :: IO UserID
getRealUserID = unsupported "getRealUserID"

getRealGroupID :: IO GroupID
getRealGroupID = unsupported "getRealGroupID"

getEffectiveUserID :: IO UserID
getEffectiveUserID = unsupported "getEffectiveUserID"

getEffectiveGroupID :: IO GroupID
getEffectiveGroupID = unsupported "getEffectiveGroupID"

getGroups :: IO [GroupID]
getGroups = return []

getLoginName :: IO String
getLoginName = unsupported "getLoginName"

setUserID :: UserID -> IO ()
setUserID _ = return ()

setGroupID :: GroupID -> IO ()
setGroupID _ = return ()

-- -----------------------------------------------------------------------------
-- User names

getEffectiveUserName :: IO String
getEffectiveUserName = unsupported "getEffectiveUserName"

-- -----------------------------------------------------------------------------
-- The group database

data GroupEntry = GroupEntry
    { groupName     :: String
    , groupPassword :: String
    , groupID       :: GroupID
    , groupMembers  :: [String]
    } deriving (Show, Read, Eq)

getGroupEntryForID :: GroupID -> IO GroupEntry
getGroupEntryForID _ = unsupported "getGroupEntryForID"

getGroupEntryForName :: String -> IO GroupEntry
getGroupEntryForName _ = unsupported "getGroupEntryForName"

getAllGroupEntries :: IO [GroupEntry]
getAllGroupEntries = return []

-- -----------------------------------------------------------------------------
-- The user database (pwd.h)

data UserEntry = UserEntry
    { userName      :: String
    , userPassword  :: String
    , userID        :: UserID
    , userGroupID   :: GroupID
    , userGecos     :: String
    , homeDirectory :: String
    , userShell     :: String
    } deriving (Show, Read, Eq)

getUserEntryForID :: UserID -> IO UserEntry
getUserEntryForID _ = unsupported "getUserEntryForID"

getUserEntryForName :: String -> IO UserEntry
getUserEntryForName _ = unsupported "getUserEntryForName"

getAllUserEntries :: IO [UserEntry]
getAllUserEntries = return []

#endif
