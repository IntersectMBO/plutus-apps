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
  , getNotifications
  ) where

import           Data.List (tails)
import Data.Foldable (foldlM)

import           Index     (IndexView (..))

data SplitIndex m h e n q r = SplitIndex
  { siHandle        :: h
  , siEvents        :: [e]
  , siBuffered      :: [e]
  , siNotifications :: [n]
  , siDepth         :: Int
  -- TODO: What about txs?
  , siStore         :: SplitIndex m h e n q r -> m ()
  , siQuery         :: SplitIndex m h e n q r -> q -> [e] -> m r
  , siOnInsert      :: e -> SplitIndex m h e n q r -> m [n]
  }

instance (Show r, Show e) => Show (SplitIndex m h e n q r) where
  show SplitIndex{siEvents, siBuffered} =
    "{ Events: " <> show siEvents <> " Buffered: " <> show siBuffered <> " }"

storeEventsThreshold :: Int
storeEventsThreshold = 3

new
  :: Monad m
  => (SplitIndex m h e n q r -> q -> [e] -> m r)
  -> (e -> SplitIndex m h e n q r -> m [n])
  -> (SplitIndex m h e n q r -> m ())
  -> Int
  -> h
  -> m (Maybe (SplitIndex m h e n q r))
new fquery foninsert fstore depth handle
  | depth <= 0        = pure Nothing
  | otherwise         = pure . Just $ SplitIndex
    { siHandle        = handle
    , siEvents        = []
    , siBuffered      = []
    , siNotifications = []
    , siDepth         = depth
    , siStore         = fstore
    , siQuery         = fquery
    , siOnInsert      = foninsert
    }

insert
  :: forall m h e n q r. Monad m
  => e
  -> SplitIndex m h e n q r
  -> m (SplitIndex m h e n q r)
insert e ix@SplitIndex{siOnInsert, siNotifications, siEvents, siDepth, siBuffered}
  | siDepth /= 1 = do
    let (siEvents', siBuffered')
          = if size ix == siDepth
            then ( e : take (siDepth - 2) siEvents
                 , last siEvents : siBuffered )
            else ( e : siEvents, siBuffered )
    ns  <- siOnInsert e ix
    let ix' = ix { siEvents   = siEvents'
                 , siBuffered = siBuffered'
                 , siNotifications = ns ++ siNotifications
                 }
    if length siBuffered' > siDepth * storeEventsThreshold
    then mergeEvents ix'
    else pure        ix'
  -- Special casing siDepth == 1 => siEvents is unused.
  | otherwise = do
    let siBuffered' = e : siBuffered
    ns  <- siOnInsert e ix
    let ix' = ix { siBuffered = e : siBuffered
                 , siNotifications = ns ++ siNotifications
                 }
    if length siBuffered' > siDepth * storeEventsThreshold
    then mergeEvents ix'
    else pure        ix'

mergeEvents :: Monad m => SplitIndex m h e n q r -> m (SplitIndex m h e n q r)
mergeEvents ix@SplitIndex{siStore} = do
  _ <- siStore ix
  pure $ ix { siBuffered = [] }

insertL :: Monad m => [e] -> SplitIndex m h e n q r -> m (SplitIndex m h e n q r)
insertL es ix = foldlM (flip insert) ix es

-- TODO: Do we actually need size < depth?
size :: SplitIndex m h e n q r -> Int
size SplitIndex {siEvents} =
  length siEvents + 1

rewind :: Int -> SplitIndex m h e n q r -> Maybe (SplitIndex m h e n q r)
rewind n ix@SplitIndex {siEvents}
  | size ix > n = Just $ ix { siEvents = drop n siEvents }
  | otherwise   = Nothing

view :: Monad m => q -> SplitIndex m h e n q r -> m (IndexView r)
view query ix@SplitIndex{siDepth} = do
  h <- getHistory query ix
  pure $ IndexView { ixDepth = siDepth
                   , ixView  = head h
                   , ixSize  = size ix
                   }

getNotifications :: Monad m => SplitIndex m h e n q r -> m [n]
getNotifications SplitIndex{siNotifications} = pure siNotifications

getHistory :: forall m h e n q r. Monad m => q -> SplitIndex m h e n q r -> m [r]
getHistory query ix@SplitIndex{siQuery, siEvents} =
  mapM (siQuery ix query) $ tails siEvents
