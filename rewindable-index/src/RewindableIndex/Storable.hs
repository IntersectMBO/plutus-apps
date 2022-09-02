module RewindableIndex.Storable
  ( -- * State
    Config
  , bufferSize
  , maxRewindLength
  , State
  , connection
  , config
    -- * API
  , insert
  , insertMany
  , rewind
  , resume
  ) where

import Control.Applicative ((<|>))
import Control.Lens.Operators ((%~), (.~), (^.))
import Control.Lens.TH qualified as Lens
import Data.Foldable (foldlM)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Vector qualified as V
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
  { _events :: V.Vector (Event e)
  , _cursor :: Int
  }
$(Lens.makeLenses ''Storage)

data State c e = State
  { _config     :: Config
  , _storage    :: Storage c e
  , _connection :: c
  }
$(Lens.makeLenses ''State)

getBufferedEvents
  :: Storage c e
  -> V.Vector (Event e)
getBufferedEvents s = V.slice 0 (s ^. cursor) (s ^. events)

insert
  :: Storable c e
  => Monad (IxMonad c)
  => IxPoint e
  -> e
  -> State c e
  -> IxMonad c (State c e)
insert e p s =
  flushBuffer s <&> storage %~ appendEvent e p

appendEvent
  :: IxPoint e
  -> e
  -> Storage c e
  -> Storage c e
appendEvent p e s = do
  let cr = s ^. cursor
  s & events %~ V.modify (\v -> VM.write v cr (p, e))
    & cursor %~ (+1)

flushBuffer
  :: Storable c e
  => Monad (IxMonad c)
  => State c e
  -> IxMonad c (State c e)
flushBuffer s = do
  let cr = s ^. storage . cursor
      es = getBufferedEvents $ s ^. storage
  if V.length es == cr
  then do
    c' <- Ix.store es $ s ^. connection
    pure $ s & connection       .~ c'
             & storage . cursor .~ 0
  else pure s

insertMany
  :: Foldable f
  => Storable c e
  => Monad (IxMonad c)
  => f (Event e)
  -> State c e
  -> IxMonad c (State c e)
insertMany es s =
  foldlM (\s' (p, e) -> insert p e s') s es

rewind
  :: forall c e. Storable c e
  => Monad (IxMonad c)
  => Eq (IxPoint e)
  => IxPoint e
  -> State c e
  -> IxMonad c (Maybe (State c e))
rewind p s = (rewindMemory <|>) <$> rewindStorage
  where
    rewindMemory :: Maybe (State c e)
    rewindMemory = do
      ix <- V.findIndex ((== p) . fst) $ s ^. storage . events
      pure $
        s & storage . cursor .~ (ix + 1)
    rewindStorage :: IxMonad c (Maybe (State c e))
    rewindStorage = do
      (fmap . fmap) (\c -> s & connection .~ c) $
        Ix.truncate p $ s ^. connection

resume
  :: Storable c e
  => State c e
  -> IxMonad c (Maybe (IxPoint e))
resume s = Ix.resume (s ^. connection)
