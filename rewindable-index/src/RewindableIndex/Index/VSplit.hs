module RewindableIndex.Index.VSplit
  ( SplitIndex(..)
  , new
  , newBoxed
  , newUnboxed
  , insert
  , insertL
  , size
  , rewind
  -- * Accessors
  , handle
  , storage
  , notifications
  , store
  , query
  , onInsert
  -- * Storage
  , Storage(..)
  , getBuffer
  , getEvents
  , k
  ) where

import Control.Lens ((%~), (&), (.~), (^.))
import Control.Lens.TH qualified as Lens
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Foldable (foldlM)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU

data Storage v m e = Storage
  { _events :: (VG.Mutable v) (PrimState m) e
  , _cursor :: Int
  , _eSize  :: Int
  , _bSize  :: Int
  , _k      :: Int
  }
$(Lens.makeLenses ''Storage)

maxSize
  :: VGM.MVector (VG.Mutable v) e
  => Storage v m e
  -> Int
maxSize store = store ^. events & VGM.length

isStorageFull
  :: VGM.MVector (VG.Mutable v) e
  => Storage v m e
  -> Bool
isStorageFull store = maxSize store == store ^. eSize + store ^. bSize

getBuffer
  :: forall v m e.
     VGM.MVector (VG.Mutable v) e
  => PrimMonad m
  => Show e
  => Storage v m e
  -> m [e]
getBuffer store =
  let bufferEnd   = store ^. cursor - store ^. eSize
      bufferStart = bufferEnd - store ^. bSize
  in  reverse <$> getInterval bufferStart (store ^. bSize) store

getEvents
  :: forall v m e.
     VGM.MVector (VG.Mutable v) e
  => PrimMonad m
  => Show e
  => Storage v m e
  -> m [e]
getEvents store =
  let c   = store ^. cursor
      esz = store ^. eSize
  in  reverse <$> getInterval (c - esz) esz store

getInterval
  :: forall v m e.
     VGM.MVector (VG.Mutable v) e
  => PrimMonad m
  => Show e
  => Int
  -> Int
  -> Storage v m e
  -> m [e]
getInterval start size' store
  | size' == 0 = pure []
  -- k underflows to the begining
  | start < 0 = do
    getInterval (maxSize store + start) size' store
  -- buffer overflows to the start
  | start + size' > maxSize store =
    let endSize   = (start + size') `rem` maxSize store
        startSize = size' - endSize
    in  (++) <$> getInterval start startSize store
             <*> getInterval 0 endSize store
  -- normal case
  | otherwise = do
    VGM.foldr' (:) [] $ VGM.slice start size' (store ^. events)

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
  | k' < 0                  = pure Nothing
  -- The vector has to accomodate at least k + 1 elements.
  | k' >= VGM.length vector = pure Nothing
  | otherwise = pure . Just $ SplitIndex
    { _handle        = handle'
    , _storage       = Storage { _events = vector
                               , _cursor = 0
                               , _eSize  = 0
                               , _bSize  = 0
                               , _k      = k'
                               }
    , _notifications = []
    , _store         = store'
    , _query         = query'
    , _onInsert      = onInsert'
    }

type BoxedIndex m h e n q r =
  SplitIndex m h V.Vector e n q r

newBoxed
  :: Monad m
  => PrimMonad m
  => (BoxedIndex m h e n q r -> q -> [e] -> m r)
  -> (BoxedIndex m h e n q r -> m ())
  -> (BoxedIndex m h e n q r -> e -> m [n])
  -> Int
  -> Int
  -> h
  -> m (Maybe (BoxedIndex m h e n q r))
newBoxed query' store' onInsert' k' size' handle'
  | k' < 0 || size' <= 0 = pure Nothing
  | otherwise = do
    v <- VGM.new (k' + size')
    new query' store' onInsert' k' handle' v

type UnboxedIndex m h e n q r =
  SplitIndex m h VU.Vector e n q r

newUnboxed
  :: Monad m
  => PrimMonad m
  => VGM.MVector VU.MVector e
  => (UnboxedIndex m h e n q r -> q -> [e] -> m r)
  -> (UnboxedIndex m h e n q r -> m ())
  -> (UnboxedIndex m h e n q r -> e -> m [n])
  -> Int
  -> Int
  -> h
  -> m (Maybe (UnboxedIndex m h e n q r))
newUnboxed query' store' onInsert' k' size' handle'
  | k' < 0 || size' <= 0  = pure Nothing
  | otherwise = do
    v <- VGM.new (k' + size')
    new query' store' onInsert' k' handle' v

insert
  :: forall m h v e n q r.
     Monad m
  => PrimMonad m
  => VGM.MVector (VG.Mutable v) e
  => e
  -> SplitIndex m h v e n q r
  -> m (SplitIndex m h v e n q r)
insert e ix = do
    let es = ix ^. storage . events
        c  = ix ^. storage . cursor
        vs = VGM.length es
    VGM.unsafeWrite es c e
    ns <- (ix ^. onInsert) ix e
    let ix' = storage            %~ updateSizes                $
              (storage . cursor) %~ (\c' -> (c' + 1) `rem` vs) $
              notifications      %~ (ns++)                     $ ix
    if isStorageFull (ix' ^. storage)
    then storeEvents ix'
    else pure        ix'

  where
    updateSizes :: Storage v m e -> Storage v m e
    updateSizes st =
        -- Event sizes increase by one upto K
        eSize %~ (\sz -> min (sz + 1) (st ^. k))                        $
        -- The buffer only grows when the event buffer is full
        bSize %~ (\sz -> if st ^. eSize == st ^. k then sz + 1 else sz) $ st

storeEvents
  :: Monad m
  => SplitIndex m h v e n q r
  -> m (SplitIndex m h v e n q r)
storeEvents ix = do
  -- TODO: Change store to store :: h -> [e] -> m () (?)
  ix & ix ^. store
  pure $
    (storage . bSize) .~ 0 $ ix

insertL
  :: PrimMonad m
  => VGM.MVector (VG.Mutable v) e
  => [e]
  -> SplitIndex m h v e n q r
  -> m (SplitIndex m h v e n q r)
insertL es ix = foldlM (flip insert) ix es

size
  :: SplitIndex m h v e n q r
  -> Int
size ix = 1 + (ix ^. storage . eSize)

rewind
  :: VGM.MVector (VG.Mutable v) e
  => Int
  -> SplitIndex m h v e n q r
  -> Maybe (SplitIndex m h v e n q r)
rewind n ix
  | ix ^. storage . eSize >= n = Just $
    (storage . cursor) %~ (\c -> adjust (c - n)) $
    (storage . eSize ) %~ (\sz -> sz - n)        $ ix
  | otherwise = Nothing
    where
      adjust :: Int -> Int
      adjust p
        | p < 0     = maxSize (ix ^. storage) + p
        | otherwise = p

