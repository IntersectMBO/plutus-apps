module RewindableIndex.Storable
  ( -- * State
    Config
  , bufferSize
  , maxRewindLength
  , State
  , connection
  , config
  , emptyState
    -- * API
  , insert
  , insertMany
  , rewind
  , resume
  ) where

import Control.Applicative ((<|>))
import Control.Lens.Operators ((%~), (.~), (^.))
import Control.Lens.TH qualified as Lens
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Foldable (foldlM)
import Data.Function ((&))
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Mutable qualified as VM

import RewindableIndex.Index (IxMonad, IxPoint, Storable)
import RewindableIndex.Index qualified as Ix

type Event e = (IxPoint e, e)

data Config = Config
  { _bufferSize      :: Int
  , _maxRewindLength :: Int
  } deriving (Show, Eq)
$(Lens.makeLenses ''Config)

data Storage c e = Storage
  { _events :: VM.MVector (PrimState (IxMonad c)) (Event e)
  , _cursor :: Int
  }
$(Lens.makeLenses ''Storage)

data State c e = State
  { _config     :: Config
  , _storage    :: Storage c e
  , _connection :: c
  }
$(Lens.makeLenses ''State)

emptyState
  :: Monad (IxMonad c)
  => PrimMonad (IxMonad c)
  => Int
  -> Int
  -> c
  -> IxMonad c (State c e)
emptyState bufferSz rewindLen con = do
  v <- VM.new bufferSz
  pure $ State { _config = Config { _bufferSize = bufferSz
                                  , _maxRewindLength = rewindLen
                                  }
               , _storage = Storage { _events = v
                                    , _cursor = 0
                                    }
               , _connection = con
               }

getBufferedEvents
  :: Storage c e
  -> V.MVector (PrimState (IxMonad c)) (Event e)
getBufferedEvents s = VM.slice 0 (s ^. cursor) (s ^. events)

insert
  :: Storable c e
  => PrimMonad (IxMonad c)
  => IxPoint e
  -> e
  -> State c e
  -> IxMonad c (State c e)
insert p e s = do
  state'   <- flushBuffer s
  storage' <- appendEvent p e (state' ^. storage)
  pure $ state' { _storage = storage' }

appendEvent
  :: PrimMonad (IxMonad c)
  => IxPoint e
  -> e
  -> Storage c e
  -> IxMonad c (Storage c e)
appendEvent p e s = do
  let cr = s ^. cursor
  VM.write (s ^. events) cr (p, e)
  pure $ s & cursor %~ (+1)

flushBuffer
  :: Storable c e
  => PrimMonad (IxMonad c)
  => State c e
  -> IxMonad c (State c e)
flushBuffer s = do
  let cr = s ^. storage . cursor
      es = getBufferedEvents $ s ^. storage
  if VM.length es == cr
  then do
    v  <- V.freeze es
    c' <- Ix.store v $ s ^. connection
    pure $ s & connection       .~ c'
             & storage . cursor .~ 0
  else pure s

insertMany
  :: Foldable f
  => Storable c e
  => PrimMonad (IxMonad c)
  => f (Event e)
  -> State c e
  -> IxMonad c (State c e)
insertMany es s =
  foldlM (\s' (p, e) -> insert p e s') s es

rewind
  :: forall c e. Storable c e
  => PrimMonad (IxMonad c)
  => Eq (IxPoint e)
  => IxPoint e
  -> State c e
  -> IxMonad c (Maybe (State c e))
rewind p s = do
  m' <- rewindMemory
  s' <- rewindStorage
  pure $ m' <|> s'
  where
    rewindMemory :: IxMonad c (Maybe (State c e))
    rewindMemory = do
      v <- V.freeze $ s ^. storage . events
      pure $ do
        ix <- VG.findIndex ((== p) . fst) v
        pure $ s & storage . cursor .~ (ix + 1)
    rewindStorage :: IxMonad c (Maybe (State c e))
    rewindStorage = do
      (fmap . fmap) (\c -> s & connection .~ c) $
        Ix.truncate p $ s ^. connection

resume
  :: Storable c e
  => State c e
  -> IxMonad c (Maybe (IxPoint e))
resume s = Ix.resume (s ^. connection)
