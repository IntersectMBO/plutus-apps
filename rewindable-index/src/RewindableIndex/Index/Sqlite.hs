module RewindableIndex.Index.Sqlite
  ( -- * API
    SqliteIndex
  , new
  , S.insert
  , S.insertL
  , S.size
  , S.rewind
   -- * Observations
  , S.view
  , S.getHistory
  , S.getNotifications
  ) where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Database.SQLite.Simple (Connection, open)

import RewindableIndex.Index.Split (SplitIndex (SplitIndex))
import RewindableIndex.Index.Split qualified as S

type SqliteIndex e n q r = SplitIndex IO Connection e n q r

new
  :: (SqliteIndex e n q r -> q -> Seq e -> IO r)
  -> (e -> SqliteIndex e n q r -> IO [n])
  -> (SqliteIndex e n q r -> IO ())
  -> Int
  -> FilePath
  -> IO (Maybe (SqliteIndex e n q r))
new fquery foninsert fstore depth db
  | depth <= 0 = pure Nothing
  | otherwise  = do
    connection <- open db
    pure . Just $ SplitIndex
      { S.siHandle        = connection
      , S.siEvents        = Seq.empty
      , S.siBuffered      = Seq.empty
      , S.siNotifications = []
      , S.siDepth         = depth
      , S.siStore         = fstore
      , S.siQuery         = fquery
      , S.siOnInsert      = foninsert
      }
