{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Index.Stored
  ( StoredIndex
  , new
  , insert
  , insertL
  , view
  , historyLength
  , rewind
  ) where

import Data.Foldable (foldlM)

data StoredIndex m a e = StoredIndex
  { siHandle :: m a
  , siEvents :: [e]
  , siDepth  :: Int
  , siStore  :: a -> [e] -> m a
  }

storeEventsThreshold :: Int
storeEventsThreshold = 3

new :: Monad m => (a -> [e] -> m a) -> Int -> m a -> Maybe (StoredIndex m a e)
new store depth acc
  | depth <= 0 = Nothing
  | otherwise  = Just $ StoredIndex
    { siHandle = acc
    , siEvents = []
    , siDepth  = depth
    , siStore  = store
    }

insert :: Monad m => e -> StoredIndex m a e -> m (StoredIndex m a e)
insert e ix@StoredIndex{ siEvents, siDepth } = do
  ix' <- if length siEvents > siDepth * storeEventsThreshold
         then mergeEvents ix
         else pure        ix
  pure ix' { siEvents = e : siEvents }

mergeEvents :: Monad m => StoredIndex m a e -> m (StoredIndex m a e)
mergeEvents ix@StoredIndex { siEvents, siDepth, siStore, siHandle } = do
  let liveEs   = take siDepth siEvents
      storedEs = drop siDepth siEvents
  h         <- siHandle
  nextStore <- siStore h storedEs
  pure $ ix { siHandle = pure nextStore
            , siEvents = liveEs
            }

insertL :: Monad m => [e] -> StoredIndex m a e -> m (StoredIndex m a e)
insertL es ix = foldlM (flip insert) ix es

view :: StoredIndex m a e -> m a
view = siHandle

historyLength :: StoredIndex m a e -> Int
historyLength StoredIndex { siDepth, siEvents } =
  min siDepth (length siEvents)

rewind :: Int -> StoredIndex m a e -> StoredIndex m a e
rewind n ix@StoredIndex { siEvents } =
  ix { siEvents = drop n siEvents }
