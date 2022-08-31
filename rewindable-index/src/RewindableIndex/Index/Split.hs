module RewindableIndex.Index.Split
  ( -- * API
    SplitIndex(..)
  , new
  , insert
  , insertL
  , size
  , rewind
  ) where

import Data.Foldable (foldlM)
import Data.Sequence (Seq ((:<|)), ViewR ((:>)))
import Data.Sequence qualified as Seq

data SplitIndex m h e n q r = SplitIndex
  { siHandle        :: h
  , siEvents        :: Seq e
  , siBuffered      :: Seq e
  , siNotifications :: [n]
  , siDepth         :: Int
  -- TODO: What about txs?
  , siStore         :: SplitIndex m h e n q r -> m ()
  , siQuery         :: SplitIndex m h e n q r -> q -> Seq e -> m r
  , siOnInsert      :: e -> SplitIndex m h e n q r -> m [n]
  }

instance (Show r, Show e) => Show (SplitIndex m h e n q r) where
  show SplitIndex{siEvents, siBuffered} =
    "{ Events: " <> show siEvents <> " Buffered: " <> show siBuffered <> " }"

storeEventsThreshold :: Int
storeEventsThreshold = 2

new
  :: Monad m
  => (SplitIndex m h e n q r -> q -> Seq e -> m r)
  -> (e -> SplitIndex m h e n q r -> m [n])
  -> (SplitIndex m h e n q r -> m ())
  -> Int
  -> h
  -> m (Maybe (SplitIndex m h e n q r))
new fquery foninsert fstore depth handle
  | depth <= 0        = pure Nothing
  | otherwise         = pure . Just $ SplitIndex
    { siHandle        = handle
    , siEvents        = Seq.empty
    , siBuffered      = Seq.empty
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
            then case Seq.viewr siEvents of
                   topEvents :> lastEvent ->
                     ( e :<| topEvents
                     , lastEvent :<| siBuffered
                     )
                   Seq.EmptyR -> error "RewindableIndex.Index.Split.insert: siEvents should not be empty"
            else ( e :<| siEvents, siBuffered )
    ns  <- siOnInsert e ix
    let ix' = ix { siEvents   = siEvents'
                 , siBuffered = siBuffered'
                 , siNotifications = ns ++ siNotifications
                 }
    if Seq.length siBuffered' > siDepth * storeEventsThreshold
    then mergeEvents ix'
    else pure        ix'
  -- Special casing siDepth == 1 => siEvents is unused.
  | otherwise = do
    let siBuffered' = e :<| siBuffered
    ns  <- siOnInsert e ix
    let ix' = ix { siBuffered = siBuffered'
                 , siNotifications = ns ++ siNotifications
                 }
    if Seq.length siBuffered' > siDepth * storeEventsThreshold
    then mergeEvents ix'
    else pure        ix'

mergeEvents :: Monad m => SplitIndex m h e n q r -> m (SplitIndex m h e n q r)
mergeEvents ix@SplitIndex{siStore} = do
  _ <- siStore ix
  pure $ ix { siBuffered = Seq.empty }

insertL :: Monad m => [e] -> SplitIndex m h e n q r -> m (SplitIndex m h e n q r)
insertL es ix = foldlM (flip insert) ix es

-- TODO: Do we actually need size < depth?
size :: SplitIndex m h e n q r -> Int
size SplitIndex {siEvents} =
  length siEvents + 1

rewind :: Int -> SplitIndex m h e n q r -> Maybe (SplitIndex m h e n q r)
rewind n ix@SplitIndex {siEvents}
  | size ix > n = Just $ ix { siEvents = Seq.drop n siEvents }
  | otherwise   = Nothing

