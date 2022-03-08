{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Index.Split
  ( -- * API
    SplitIndex(..)
  , new
  , insert
  , insertL
  , size
  , rewind
  -- * Observations
  , view
  , getHistory
  , getEvents
  ) where

import           Data.Foldable (foldlM)
import           Data.List     (scanl')

import           Index         (IndexView (..))

data SplitIndex m a e = SplitIndex
  { siStoredIx :: m a
    -- ^ Combined view of `[e]` and `m a`
  , siEvents   :: [e]
  , siDepth    :: Int
  , siStore    :: a -> m a
  , siIndex    :: a -> [e] -> a
    -- ^ Not sure how reasonble this is for a SQL db, but will leave it as-is for now
  }

storeEventsThreshold :: Int
storeEventsThreshold = 3

new
  :: Monad m
  => (a -> [e] -> a)
  -> (a -> m a)
  -> Int
  -> m a
  -> Maybe (SplitIndex m a e)
new findex fstore depth ix
  | depth <= 0   = Nothing
  | otherwise    = Just $ SplitIndex
    { siStoredIx = ix
    , siEvents   = []
    , siDepth    = depth
    , siStore    = fstore
    , siIndex    = findex
    }

insert :: Monad m => e -> SplitIndex m a e -> m (SplitIndex m a e)
insert e ix@SplitIndex{ siEvents, siDepth } = do
  ix' <- if length siEvents > siDepth * storeEventsThreshold
         then mergeEvents ix
         else pure        ix
  pure ix' { siEvents = e : siEvents }

mergeEvents :: Monad m => SplitIndex m a e -> m (SplitIndex m a e)
mergeEvents ix@SplitIndex { siEvents, siDepth, siStore, siIndex, siStoredIx } = do
  let liveEs   = take siDepth siEvents
      storedEs = drop siDepth siEvents
  six       <- siStoredIx
  let six'   = siIndex six storedEs
  nextStore <- siStore six'
  pure $ ix { siStoredIx = pure nextStore
            , siEvents = liveEs
            }

insertL :: Monad m => [e] -> SplitIndex m a e -> m (SplitIndex m a e)
insertL es ix = foldlM (flip insert) ix es

size :: SplitIndex m a e -> Int
size SplitIndex { siDepth, siEvents } =
  min siDepth (length siEvents + 1)

rewind :: Int -> SplitIndex m a e -> Maybe (SplitIndex m a e)
rewind n ix@SplitIndex { siEvents }
  | length siEvents > n = Just $ ix { siEvents = drop n siEvents }
  | otherwise           = Nothing

view :: Monad m => SplitIndex m a e -> m (IndexView a)
view SplitIndex{siStoredIx, siDepth, siEvents, siIndex} = do
  v <- siStoredIx
  let d = siDepth
      s = length siEvents + 1
  pure $ IndexView { ixDepth = d
                   , ixView  = siIndex v siEvents
                   , ixSize  = s
                   }

getHistory :: Monad m => SplitIndex m a e -> m [a]
getHistory SplitIndex{siStoredIx, siIndex, siEvents} = do
  storedIx <- siStoredIx
  pure $ scanl' (\a e -> siIndex a [e]) storedIx siEvents

getEvents :: SplitIndex m a e -> [e]
getEvents SplitIndex{siEvents} = siEvents
