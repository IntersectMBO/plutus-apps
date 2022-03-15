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

import           Index         (IndexView (..))

data SplitIndex m a e = SplitIndex
  { siStoredIx :: m a
    -- ^ Combined view of `[e]` and `m a`
  , siEvents   :: [e]
  , siBuffered :: [e]
  , siDepth    :: Int
  , siStore    :: a -> m a
  , siIndex    :: a -> [e] -> a
    -- ^ Not sure how reasonble this is for a SQL db, but will leave it as-is for now
  }

instance (Show a, Show e) => Show (SplitIndex m a e) where
  show SplitIndex{siEvents, siBuffered} =
    "{ Events: " <> show siEvents <> " Buffered: " <> show siBuffered <> " }"

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
    , siBuffered = []
    , siDepth    = depth
    , siStore    = fstore
    , siIndex    = findex
    }

insert :: Monad m => e -> SplitIndex m a e -> m (SplitIndex m a e)
insert e ix@SplitIndex{siEvents, siDepth, siBuffered}
  | siDepth /= 1 = do
    let (siEvents', siBuffered')
          = if size ix == siDepth
            then ( e : take (siDepth - 2) siEvents
                 , last siEvents : siBuffered )
            else ( e : siEvents, siBuffered )
    let ix' = ix { siEvents = siEvents'
                 , siBuffered = siBuffered'
                 }
    if length siBuffered' > siDepth * storeEventsThreshold
    then mergeEvents ix'
    else pure        ix'
  -- Special casing siDepth == 1 => siEvents is unused.
  | otherwise = do
    let siBuffered' = e : siBuffered
    let ix' = ix { siBuffered = e : siBuffered }
    if length siBuffered' > siDepth * storeEventsThreshold
    then mergeEvents ix'
    else pure        ix'

mergeEvents :: Monad m => SplitIndex m a e -> m (SplitIndex m a e)
mergeEvents ix@SplitIndex {siStore, siIndex, siStoredIx, siBuffered} = do
  six       <- siStoredIx
  let six'   = siIndex six siBuffered
  nextStore <- siStore six'
  pure $ ix { siStoredIx = pure nextStore
            , siBuffered = []
            }

insertL :: Monad m => [e] -> SplitIndex m a e -> m (SplitIndex m a e)
insertL es ix = foldlM (flip insert) ix es

-- TODO: Do we actually need size < depth?
size :: SplitIndex m a e -> Int
size SplitIndex {siEvents} =
  length siEvents + 1

rewind :: Int -> SplitIndex m a e -> Maybe (SplitIndex m a e)
rewind n ix@SplitIndex {siEvents}
  | size ix > n = Just $ ix { siEvents = drop n siEvents }
  | otherwise   = Nothing

view :: Monad m => SplitIndex m a e -> m (IndexView a)
view ix@SplitIndex{siDepth} = do
  h <- getHistory ix
  pure $ IndexView { ixDepth = siDepth
                   , ixView  = head h
                   , ixSize  = size ix
                   }

getHistory :: forall m e a. Monad m => SplitIndex m a e -> m [a]
getHistory SplitIndex{siStoredIx, siIndex, siEvents, siBuffered} = do
  storedIx <- siStoredIx
  let a  = foldr index storedIx siBuffered
  pure $ scanr index a siEvents
  where
    index :: e -> a -> a
    index e a = siIndex a [e]

getEvents :: SplitIndex m a e -> [e]
getEvents SplitIndex{siEvents} = siEvents
