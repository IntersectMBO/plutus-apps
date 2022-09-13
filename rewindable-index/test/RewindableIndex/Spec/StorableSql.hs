module RewindableIndex.Spec.StorableSql where

import Database.SQLite.Simple qualified as Sql

import RewindableIndex.Index (IxMonad, IxPoint)
import RewindableIndex.Storable (State)

newtype Event = Event Int
newtype Point = Point Int
type    Handle = State Sql.Connection Event

newtype instance IxPoint Event = StoragePoint Int
type instance IxMonad Handle = IO

-- newStorableSqlIndexer
--   :: Word
--   -> Word
--   -> IO Handle
-- newStorableSqlIndexer bufferSz rewindLen = do
--   connection <- Sql.open ":memory:"
--   pure $


