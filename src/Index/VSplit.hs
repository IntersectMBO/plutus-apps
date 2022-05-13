{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Index.VSplit
  ( SplitIndex(..)
  , new
  , insert
  , insertL
  , size
  , rewind
  -- * Accessors to SplitIndex
  , handle
  , storage
  , notifications
  , store
  , query
  , onInsert
  -- * Storage
  , Storage
  , getBuffer
  , getEvents
  -- * Observations
  , getNotifications
  , getHistory
  , view
  ) where

import Control.Lens.Operators
import qualified Control.Lens.TH as Lens
import Control.Monad.Primitive (PrimState, PrimMonad)
import Data.List (tails)
import Data.Foldable (toList, foldlM)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

import Index (IndexView(..))

data Storage v m e = Storage
  { _events :: (VG.Mutable v) (PrimState m) e
  , _cursor :: Int
  , _stSize :: Int
  , _k      :: Int
  }
$(Lens.makeLenses ''Storage)

maxSize
  :: VGM.MVector (VG.Mutable v) e
  => Storage v m e
  -> Int
maxSize store = store ^. events & VGM.length

bufferSize
  :: VGM.MVector (VG.Mutable v) e
  => Storage v m e
  -> Int
bufferSize store = maxSize store - store ^. k

isStorageFull
  :: VGM.MVector (VG.Mutable v) e
  => Storage v m e
  -> Bool
isStorageFull store = maxSize store == store ^. stSize

getBuffer
  :: forall v m e.
     VGM.MVector (VG.Mutable v) e
  => Foldable (VG.Mutable v (PrimState m))
  => Storage v m e
  -> [e]
getBuffer storage =
  getInterval (storage ^. cursor) (bufferSize storage) storage

getEvents
  :: forall v m e.
     VGM.MVector (VG.Mutable v) e
  => Foldable (VG.Mutable v (PrimState m))
  => Storage v m e
  -> [e]
getEvents storage =
  let c  = storage ^. cursor
      k' = storage ^. k
  in  getInterval (c - k') c storage

getInterval
  :: forall v m e.
     VGM.MVector (VG.Mutable v) e
  => Foldable (VG.Mutable v (PrimState m))
  => Int
  -> Int
  -> Storage v m e
  -> [e]
getInterval start size' store
  -- k overflows to the begining
  | start < 0 =
    getInterval (maxSize store + start) (- start) store
    ++ getInterval 0 (size' + start) store
  -- buffer overflows to the start
  | start + size' >= maxSize store =
    let endSize   = start + size' `rem` maxSize store
        startSize = size' - endSize
    in  getInterval start startSize store
        ++ getInterval 0 endSize store
  -- normal case
  | otherwise = toList $ VGM.slice start size' (store ^. events)

data SplitIndex m h v e n q r = SplitIndex
  { _handle        :: h
  , _storage       :: Storage v m e
  , _notifications :: [n]
  , _store         :: SplitIndex m h v e n q r -> m ()
  , _query         :: SplitIndex m h v e n q r -> q -> [e] -> m r
  , _onInsert      :: SplitIndex m h v e n q r -> e -> m [n]
  }
$(Lens.makeLenses ''SplitIndex)

new
  :: Monad m
  => VGM.MVector (VG.Mutable v) e
  => (SplitIndex m h v e n q r -> q -> [e] -> m r)
  -> (SplitIndex m h v e n q r -> m ())
  -> (SplitIndex m h v e n q r -> e -> m [n])
  -> Int
  -> h
  -> (VG.Mutable v) (PrimState m) e
  -> m (Maybe (SplitIndex m h v e n q r))
new query' store' onInsert' k' handle' vector
  | k' <= 0                 = pure Nothing
  -- The vector has to accomodate at least k + 1 elements.
  | k' >= VGM.length vector = pure Nothing
  | otherwise = pure . Just $ SplitIndex
    { _handle        = handle'
    , _storage       = Storage { _events = vector
                               , _cursor = 0
                               , _stSize = 0
                               , _k      = k'
                               }
    , _notifications = []
    , _store         = store'
    , _query         = query'
    , _onInsert      = onInsert'
    }

insert
  :: forall m h v e n q r.
     Monad m
  => PrimMonad m
  => VGM.MVector (VG.Mutable v) e
  => e
  -> SplitIndex m h v e n q r
  -> m (SplitIndex m h v e n q r)
insert e ix = do
  -- o | ix ^. storage . k /= 1 = do
    let es = ix ^. storage . events
        c  = ix ^. storage . cursor
    VGM.unsafeWrite es c e
    ns <- (ix ^. onInsert) ix e
    let ix' = (storage . stSize) %~ (+1)   $
              (storage . cursor) %~ (+1)   $
              notifications      %~ (++ns) $ ix
    if isStorageFull (ix' ^. storage)
    then storeEvents ix'
    else pure        ix'
  -- o | otherwise      = undefined

storeEvents
  :: Monad m
  => VGM.MVector (VG.Mutable v) e
  => SplitIndex m h v e n q r
  -> m (SplitIndex m h v e n q r)
storeEvents ix = do
  -- TODO: Change store to store :: h -> [e] -> m () (?)
  ix & ix ^. store
  let sz = bufferSize $ ix ^. storage
  pure $
    (storage . stSize) %~ (\s -> s - sz) $ ix

insertL
  :: Monad m
  => PrimMonad m
  => VGM.MVector (VG.Mutable v) e
  => [e]
  -> SplitIndex m h v e n q r
  -> m (SplitIndex m h v e n q r)
insertL es ix = foldlM (flip insert) ix es

size
  :: SplitIndex m h v e n q r
  -> Int
size ix = min (ix ^. storage . k)
              (ix ^. storage . stSize)

rewind
  :: VGM.MVector (VG.Mutable v) e
  => Int
  -> SplitIndex m h v e n q r
  -> Maybe (SplitIndex m h v e n q r)
rewind n ix
  | size ix > n = Just $
    (storage . cursor) %~ (\c -> adjust (c - n)) $ ix
  | otherwise = Nothing
    where
      adjust :: Int -> Int
      adjust p
        | p < 0     = maxSize (ix ^. storage) - p
        | otherwise = p

getNotifications
  :: SplitIndex m h v e n q r
  -> [n]
getNotifications ix = ix ^. notifications

getHistory
  :: Monad m
  => VGM.MVector (VG.Mutable v) e
  => Foldable (VG.Mutable v (PrimState m))
  => SplitIndex m h v e n q r
  -> q
  -> m [r]
getHistory ix q = do
  let es = getEvents (ix ^. storage)
  traverse ((ix ^. query) ix q) $ tails es

view
  :: Monad m
  => VGM.MVector (VG.Mutable v) e
  => Foldable (VG.Mutable v (PrimState m))
  => SplitIndex m h v e n q r
  -> q
  -> m (IndexView r)
view ix q = do
  hs <- getHistory ix q
  pure $ IndexView { ixDepth = ix ^. storage . k
                   , ixView  = head hs
                   , ixSize  = size ix
                   }
