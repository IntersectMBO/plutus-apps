{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
This module re-exports the types from @System.Posix.Types@ on all platforms.

On Windows 'UserID', 'GroupID' and 'LinkCount' are missing, so they are
redefined by this module.
-}
module System.PosixCompat.Types (
      module System.Posix.Types
#ifdef mingw32_HOST_OS
    , UserID
    , GroupID
    , LinkCount
#endif
    ) where

#ifdef mingw32_HOST_OS
-- Since CIno (FileID's underlying type) reflects <sys/type.h> ino_t,
-- which mingw defines as short int (int16), it must be overriden to
-- match the size of windows fileIndex (word64).
import System.Posix.Types

import Data.Word (Word32)

newtype UserID = UserID Word32
  deriving (Eq, Ord, Enum, Bounded, Integral, Num, Real)
instance Show UserID where show (UserID x) = show x
instance Read UserID where readsPrec i s = [ (UserID x, s')
                                           | (x,s') <- readsPrec i s]

newtype GroupID = GroupID Word32
  deriving (Eq, Ord, Enum, Bounded, Integral, Num, Real)
instance Show GroupID where show (GroupID x) = show x
instance Read GroupID where readsPrec i s = [ (GroupID x, s')
                                            | (x,s') <- readsPrec i s]

newtype LinkCount = LinkCount Word32
  deriving (Eq, Ord, Enum, Bounded, Integral, Num, Real)
instance Show LinkCount where show (LinkCount x) = show x
instance Read LinkCount where readsPrec i s = [ (LinkCount x, s')
                                              | (x,s') <- readsPrec i s]

#else
import System.Posix.Types
#endif
