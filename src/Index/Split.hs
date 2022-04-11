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

data SplitIndex m h s e n = SplitIndex
  { siHandle        :: h
  , siEvents        :: [e]
  , siBuffered      :: [e]
  , siNotifications :: [n]
  , siDepth         :: Int
  , siIndex         :: s -> [e] -> (s, [n])
  -- TODO: What about txs?
  , siStore         :: h -> s -> m ()
  , siLoad          :: h -> m s
  }

instance (Show s, Show e) => Show (SplitIndex m h s e n) where
  show SplitIndex{siEvents, siBuffered} =
    "{ Events: " <> show siEvents <> " Buffered: " <> show siBuffered <> " }"

storeEventsThreshold :: Int
storeEventsThreshold = 3

new
  :: Monad m
  => (s -> [e] -> (s, [n]))
  -> (h -> s -> m ())
  -> (h -> m s)
  -> Int
  -> h
  -> m (Maybe (SplitIndex m h s e n))
new findex fstore fload depth handle
  | depth <= 0        = pure Nothing
  | otherwise         = pure . Just $ SplitIndex
    { siHandle        = handle
    , siEvents        = []
    , siBuffered      = []
    , siNotifications = []
    , siDepth         = depth
    , siStore         = fstore
    , siIndex         = findex
    , siLoad          = fload
    }

insert
  :: forall m h s e n. Monad m
  => e
  -> SplitIndex m h s e n
  -> m (SplitIndex m h s e n)
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
    addNotifications :: Monad m => SplitIndex m h s e n -> m (SplitIndex m h s e n)
    addNotifications ix'@SplitIndex{ siNotifications
                                   , siIndex } = do
      state <- mergedState ix
      let ns = snd $ siIndex state [e]
      pure $ ix' { siNotifications = ns ++ siNotifications }

mergedState :: Monad m => SplitIndex m h s e n -> m s
mergedState SplitIndex{siLoad, siIndex, siHandle, siEvents, siBuffered} = do
  storedState <- siLoad siHandle
  pure . fst $ siIndex storedState (siEvents ++ siBuffered)


mergeEvents :: Monad m => SplitIndex m h s e n -> m (SplitIndex m h s e n)
mergeEvents ix@SplitIndex {siLoad, siStore, siIndex, siHandle, siBuffered} = do
  storedState <- siLoad siHandle
  let updatedStoreState = fst $ siIndex storedState siBuffered
  _ <- siStore siHandle updatedStoreState
  pure $ ix { siBuffered = [] }

insertL :: Monad m => [e] -> SplitIndex m h s e n -> m (SplitIndex m h s e n)
insertL es ix = foldlM (flip insert) ix es

-- TODO: Do we actually need size < depth?
size :: SplitIndex m h s e n -> Int
size SplitIndex {siEvents} =
  length siEvents + 1

rewind :: Int -> SplitIndex m h s e n -> Maybe (SplitIndex m h s e n)
rewind n ix@SplitIndex {siEvents}
  | size ix > n = Just $ ix { siEvents = drop n siEvents }
  | otherwise   = Nothing

view :: Monad m => SplitIndex m h s e n -> m (IndexView s)
view ix@SplitIndex{siDepth} = do
  h <- getHistory ix
  pure $ IndexView { ixDepth = siDepth
                   , ixView  = head h
                   , ixSize  = size ix
                   }

getNotifications :: Monad m => SplitIndex m h s e n -> m [n]
getNotifications SplitIndex{siNotifications} = pure siNotifications

getHistory :: forall m h e s n. Monad m => SplitIndex m h s e n -> m [s]
getHistory SplitIndex{siLoad, siHandle, siIndex, siEvents, siBuffered} = do
  storedState <- siLoad siHandle
  let bas = foldr index [storedState] siBuffered
  pure $ foldr index [head bas] siEvents
  where
    index :: e -> [s] -> [s]
    index e as = do
      let (a, _) = siIndex (head as) [e]
      a : as

getEvents :: SplitIndex m h s e n -> [e]
getEvents SplitIndex{siEvents} = siEvents
