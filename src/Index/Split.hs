{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Index.Split
  ( SplitIndex
  , new
  , insert
  , insertL
  , view
  , historyLength
  , rewind
  ) where

import           Data.Foldable (foldlM)

data SplitIndex m a e = SplitIndex
  { siHandle :: m a
  , siEvents :: [e]
  , siDepth  :: Int
  , siStore  :: a -> [e] -> m a
  , siView  :: forall b. (SplitIndex m a e -> m b) -> SplitIndex m a e -> m b
  }

type SplitIndexView m a e =
  forall b. (SplitIndex m a e -> m b) -> SplitIndex m a e -> m b

storeEventsThreshold :: Int
storeEventsThreshold = 3

new
  :: Monad m
  => SplitIndexView m a e
  -> (a -> [e] -> m a)
  -> Int
  -> m a
  -> Maybe (SplitIndex m a e)
new view store depth acc
  | depth <= 0 = Nothing
  | otherwise  = Just $ SplitIndex
    { siHandle = acc
    , siEvents = []
    , siDepth  = depth
    , siStore  = store
    , siView   = view
    }

insert :: Monad m => e -> SplitIndex m a e -> m (SplitIndex m a e)
insert e ix@SplitIndex{ siEvents, siDepth } = do
  ix' <- if length siEvents > siDepth * storeEventsThreshold
         then mergeEvents ix
         else pure        ix
  pure ix' { siEvents = e : siEvents }

mergeEvents :: Monad m => SplitIndex m a e -> m (SplitIndex m a e)
mergeEvents ix@SplitIndex { siEvents, siDepth, siStore, siHandle } = do
  let liveEs   = take siDepth siEvents
      storedEs = drop siDepth siEvents
  h         <- siHandle
  nextStore <- siStore h storedEs
  pure $ ix { siHandle = pure nextStore
            , siEvents = liveEs
            }

insertL :: Monad m => [e] -> SplitIndex m a e -> m (SplitIndex m a e)
insertL es ix = foldlM (flip insert) ix es

view :: SplitIndex m a e -> SplitIndexView m a e
view = siView

historyLength :: SplitIndex m a e -> Int
historyLength SplitIndex { siDepth, siEvents } =
  min siDepth (length siEvents)

rewind :: Int -> SplitIndex m a e -> SplitIndex m a e
rewind n ix@SplitIndex { siEvents } =
  ix { siEvents = drop n siEvents }
