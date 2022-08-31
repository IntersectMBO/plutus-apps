module RewindableIndex.Index.VSqlite
  ( -- * API
    SqliteIndex
  , new
  , newBoxed
  , S.insert
  , S.insertL
  , S.size
  , S.rewind
  , S.getEvents
  , S.getBuffer
  , S.handle
  , S.storage
  ) where

import Control.Monad.Primitive (PrimState)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Database.SQLite.Simple (Connection, open)

import RewindableIndex.Index.VSplit (SplitIndex (SplitIndex), Storage (Storage))
import RewindableIndex.Index.VSplit qualified as S

type SqliteIndex e n q r = SplitIndex IO Connection V.Vector e n q r

new
  :: (SqliteIndex e n q r -> q -> [e] -> IO r)
  -> (SqliteIndex e n q r -> IO ())
  -> (SqliteIndex e n q r -> e -> IO [n])
  -> Int
  -> FilePath
  -> (VG.Mutable V.Vector) (PrimState IO) e
  -> IO (Maybe (SqliteIndex e n q r))
new fquery fstore foninsert k' db vector
  | k' < 0 = pure Nothing
  | otherwise  = do
    connection <- open db
    pure . Just $ SplitIndex
      { S._handle        = connection
      , S._storage = Storage { S._events = vector
                             , S._cursor = 0
                             , S._eSize  = 0
                             , S._bSize  = 0
                             , S._k      = k'
                             }
      , S._notifications = []
      , S._store         = fstore
      , S._query         = fquery
      , S._onInsert      = foninsert
      }

type BoxedIndex e n q r = SqliteIndex e n q r

newBoxed
  :: (BoxedIndex e n q r -> q -> [e] -> IO r)
  -> (BoxedIndex e n q r -> IO ())
  -> (BoxedIndex e n q r -> e -> IO [n])
  -> Int
  -> Int
  -> FilePath
  -> IO (Maybe (BoxedIndex e n q r))
newBoxed query' store' onInsert' k' size' dbPath
  | k' < 0 || size' <= 0 = pure Nothing
  | otherwise = do
    v <- VGM.new (k' + size')
    new query' store' onInsert' k' dbPath v

