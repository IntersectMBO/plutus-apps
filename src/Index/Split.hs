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
  , getNotifications
  ) where

import           Data.Foldable (foldlM, foldrM)

import           Index         (IndexView (..))

data SplitIndex m a e n = SplitIndex
  { siStoredIx      :: a
    -- ^ Combined view of `[e]` and `m a`
  , siEvents        :: [e]
  , siBuffered      :: [e]
  , siNotifications :: [n]
  , siDepth         :: Int
  , siStore         :: a -> m a
  , siIndex         :: a -> [e] -> m (a, [n])
    -- ^ Not sure how reasonble this is for a SQL db, but will leave it as-is for now
  }

instance (Show a, Show e) => Show (SplitIndex m a e n) where
  show SplitIndex{siEvents, siBuffered} =
    "{ Events: " <> show siEvents <> " Buffered: " <> show siBuffered <> " }"

storeEventsThreshold :: Int
storeEventsThreshold = 3

new
  :: Monad m
  => (a -> [e] -> m (a,[n]))
  -> (a -> m a)
  -> Int
  -> a
  -> m (Maybe (SplitIndex m a e n))
new findex fstore depth ix
  | depth <= 0        = pure Nothing
  | otherwise         = pure . Just $ SplitIndex
    { siStoredIx      = ix
    , siEvents        = []
    , siBuffered      = []
    , siNotifications = []
    , siDepth         = depth
    , siStore         = fstore
    , siIndex         = findex
    }

insert
  :: forall m a e n. Monad m
  => e
  -> SplitIndex m a e n
  -> m (SplitIndex m a e n)
insert e ix@SplitIndex{siEvents, siDepth, siBuffered}
  | siDepth /= 1 = do
    let (siEvents', siBuffered')
          = if size ix == siDepth
            then ( e : take (siDepth - 2) siEvents
                 , last siEvents : siBuffered )
            else ( e : siEvents, siBuffered )
    ix' <- addNotifications $
             ix { siEvents   = siEvents'
                , siBuffered = siBuffered'
                }
    if length siBuffered' > siDepth * storeEventsThreshold
    then mergeEvents ix'
    else pure        ix'
  -- Special casing siDepth == 1 => siEvents is unused.
  | otherwise = do
    let siBuffered' = e : siBuffered
    ix' <- addNotifications $
             ix { siBuffered = e : siBuffered }
    if length siBuffered' > siDepth * storeEventsThreshold
    then mergeEvents ix'
    else pure        ix'
  where
    addNotifications :: Monad m => SplitIndex m a e n -> m (SplitIndex m a e n)
    addNotifications ix'@SplitIndex{ siNotifications
                                   , siIndex } = do
      state <- mergedState ix
      ns <- snd <$> siIndex state [e]
      pure $ ix' { siNotifications = ns ++ siNotifications }

mergedState :: Monad m => SplitIndex m a e n -> m a
mergedState SplitIndex{siIndex, siStoredIx, siEvents, siBuffered} = do
  fst <$> siIndex siStoredIx (siEvents ++ siBuffered)


mergeEvents :: Monad m => SplitIndex m a e n -> m (SplitIndex m a e n)
mergeEvents ix@SplitIndex {siStore, siIndex, siStoredIx, siBuffered} = do
  six <- fst <$> siIndex siStoredIx siBuffered
  nextStore <- siStore six
  pure $ ix { siStoredIx = nextStore
            , siBuffered = []
            }

insertL :: Monad m => [e] -> SplitIndex m a e n -> m (SplitIndex m a e n)
insertL es ix = foldlM (flip insert) ix es

-- TODO: Do we actually need size < depth?
size :: SplitIndex m a e n -> Int
size SplitIndex {siEvents} =
  length siEvents + 1

rewind :: Int -> SplitIndex m a e n -> Maybe (SplitIndex m a e n)
rewind n ix@SplitIndex {siEvents}
  | size ix > n = Just $ ix { siEvents = drop n siEvents }
  | otherwise   = Nothing

view :: Monad m => SplitIndex m a e n -> m (IndexView a)
view ix@SplitIndex{siDepth} = do
  h <- getHistory ix
  pure $ IndexView { ixDepth = siDepth
                   , ixView  = head h
                   , ixSize  = size ix
                   }

getNotifications :: Monad m => SplitIndex m a e n -> m [n]
getNotifications SplitIndex{siNotifications} = pure siNotifications

getHistory :: forall m e a n. Monad m => SplitIndex m a e n -> m [a]
getHistory SplitIndex{siStoredIx, siIndex, siEvents, siBuffered} = do
  bas <- foldrM index [siStoredIx] siBuffered
  foldrM index [head bas] siEvents
  where
    index :: e -> [a] -> m [a]
    index e as = do
      (a, _) <- siIndex (head as) [e]
      pure (a : as)

getEvents :: SplitIndex m a e n -> [e]
getEvents SplitIndex{siEvents} = siEvents
