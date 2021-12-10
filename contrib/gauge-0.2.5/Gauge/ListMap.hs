-- This is an extremely cheap (code-wise) implementation of Map.
-- it's not meant to be efficient, but just provide
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Gauge.ListMap
    ( Map
    , fromList
    , toList
    , lookup
    ) where

import           Data.Typeable
import           GHC.Generics
import           Prelude hiding (lookup)
import qualified Prelude as P
import           Control.DeepSeq (NFData)
import           Data.List hiding (lookup)
import           Data.Function (on)

newtype Map k v = Map [(k,v)]
    deriving (Show,Eq,Typeable,Generic, NFData)

fromList :: Ord k => [(k,v)] -> Map k v
fromList = Map . map head . groupBy ((==) `on` fst) . sortBy (compare `on` fst)

toList :: Map k v -> [(k,v)]
toList (Map l) = l

lookup :: Eq k => k -> Map k v -> Maybe v
lookup k (Map l) = P.lookup k l
