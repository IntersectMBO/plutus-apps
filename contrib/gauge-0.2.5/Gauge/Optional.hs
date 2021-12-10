{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- |
-- Module      : Gauge.Optional
-- Copyright   : (c) 2017-2018 Vincent Hanquez
--
-- A sum-type free Maybe where the value Nothing is
-- represented by a special value in the original
-- domain supported
--
-- The OptionalTag class is where the special value
-- is defined
--
{-# LANGUAGE DeriveGeneric #-}
module Gauge.Optional
    ( Optional
    , toOptional
    , unOptional
    , OptionalTag(..)
    , isOmitted
    , omitted
    , toMaybe
    , fromMaybe
    , map
    , both
    ) where

import Prelude hiding (map)
import Data.Int
import Data.Word
import Data.Data
import GHC.Generics
import Basement.Compat.CallStack

-- | A type representing a sum-type free Maybe a
-- where a specific tag represent Nothing
newtype Optional a = Optional { unOptional :: a }
    deriving (Eq, Show, Read, Typeable, Data, Generic)

class OptionalTag a where
    optionalTag :: a
    isOptionalTag :: a -> Bool

instance OptionalTag Int64 where
    optionalTag = minBound
    isOptionalTag = (==) optionalTag
instance OptionalTag Word64 where
    optionalTag = maxBound
    isOptionalTag = (==) optionalTag
instance OptionalTag Double where
    optionalTag = -1/0
    isOptionalTag d = isInfinite d || isNaN d

-- | Create an optional value from a 
toOptional :: (HasCallStack, OptionalTag a) => String -> a -> Optional a
toOptional ty v
    | isOptionalTag v = error ("Creating an optional valid value for " ++ ty ++ " using the optional tag")
    | otherwise       = Optional v
{-# INLINE toOptional #-}

omitted :: OptionalTag a => Optional a
omitted = Optional optionalTag
{-# INLINE omitted #-}

isOmitted :: OptionalTag a => Optional a -> Bool
isOmitted (Optional v)
    | isOptionalTag v = True
    | otherwise       = False
    
toMaybe :: OptionalTag a => Optional a -> Maybe a
toMaybe (Optional v) | isOptionalTag v = Nothing
                     | otherwise       = Just v
{-# INLINE toMaybe #-}

fromMaybe :: (HasCallStack, OptionalTag a) => Maybe a -> Optional a
fromMaybe Nothing  = Optional optionalTag
fromMaybe (Just v)
    | isOptionalTag v = error "fromMaybe: creating an optional value using the optional tag"
    | otherwise       = Optional v
{-# INLINE fromMaybe #-}

map :: OptionalTag a => (a -> a) -> Optional a -> Optional a
map f o@(Optional v) | isOptionalTag v = o
                     | otherwise       = Optional (f v) 
{-# INLINE map #-}

both :: (HasCallStack, OptionalTag a) => (a -> a -> a) -> Optional a -> Optional a -> Optional a
both f o1 o2
    | isOmitted o1    = o2
    | isOmitted o2    = o1
    | isOptionalTag r = error "both: creating an optional value using the optional tag"
    | otherwise       = Optional r
  where r = f (unOptional o1) (unOptional o2)
