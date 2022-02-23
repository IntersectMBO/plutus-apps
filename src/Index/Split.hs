{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Index.Split
  ( -- * API
    SplitIndex
  , new
  , insert
  , insertL
  , size
  , rewind
  ) where

import           Control.Monad ((=<<))
import           Data.Foldable (foldl', foldlM)
import           Data.Maybe    (fromJust)

import           Index         (Index (..), IndexView (..))
import qualified Index         as Ix

data SplitIndex m a e = SplitIndex
  { siHandle :: m a
  , siEvents :: [e]
  , siDepth  :: Int
  , siStore  :: a -> [e] -> m a
  }

storeEventsThreshold :: Int
storeEventsThreshold = 3

new
  :: Monad m
  => (a -> [e] -> m a)
  -> Int
  -> m a
  -> Maybe (SplitIndex m a e)
new store depth acc
  | depth <= 0 = Nothing
  | otherwise  = Just $ SplitIndex
    { siHandle = acc
    , siEvents = []
    , siDepth  = depth
    , siStore  = store
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

size :: SplitIndex m a e -> Int
size SplitIndex { siDepth, siEvents } =
  min siDepth (length siEvents)

rewind :: Int -> SplitIndex m a e -> SplitIndex m a e
rewind n ix@SplitIndex { siEvents } =
  ix { siEvents = drop n siEvents }

-- | Using Split as an interpretation of Index

toIndexView :: Monad m => SplitIndex m a e -> m (IndexView a)
toIndexView si@SplitIndex{siHandle, siDepth, siStore, siEvents} = do
  h <- siHandle
  v <- siStore h siEvents
  pure $ IndexView { ixDepth = siDepth
                   , ixSize  = size si
                   , ixView  = v
                   }

fromIndex :: forall m a e. Monad m => Index a e -> m (SplitIndex m a e)
fromIndex (Ix.New f depth acc) =
  pure $ fromJust $ new merge depth (pure acc)
  where
    merge :: Monad m => a -> [e] -> m a
    merge acc es = pure $ foldl' f acc es
fromIndex (Ix.Insert e ix) = insert e =<< fromIndex ix
fromIndex (Ix.Rewind n ix) = rewind n <$> fromIndex ix

