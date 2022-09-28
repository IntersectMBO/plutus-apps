module RewindableIndex.Spec.StorableSql where

-- import Database.SQLite.Simple qualified as Sql

-- import RewindableIndex.Index (IxMonad, IxPoint, Storable (resume, store, truncate))
-- import RewindableIndex.Storable (State, emptyState)

-- newtype Event = Event Int
-- newtype Point = Point Int
-- type    Handle = State Sql.Connection Event

-- newtype instance IxPoint Event       = StoragePoint Int
-- type instance IxMonad Sql.Connection = IO

-- newStorableSqlIndexer
--   :: Word
--   -> Word
--   -> IO Handle
-- newStorableSqlIndexer bufferSz rewindLen = do
--   connection <- Sql.open ":memory:"
--   emptyState (fromIntegral bufferSz) (fromIntegral rewindLen) connection

-- instance Storable Sql.Connection Event where
--   store = undefined
--   truncate = undefined
--   resume = undefined
