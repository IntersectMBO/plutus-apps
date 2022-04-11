module Index.Sqlite
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
  , S.getEvents
  , S.getNotifications
  ) where

import           Database.SQLite.Simple (Connection, open)

import           Index.Split            (SplitIndex (..))
import qualified Index.Split as S

type SqliteIndex a e n = SplitIndex IO Connection a e n

new
  :: (a -> [e] -> (a, [n]))
  -> (Connection -> a -> IO ())
  -> (Connection -> IO a)
  -> Int
  -> FilePath
  -> IO (Maybe (SqliteIndex a e n))
new findex fstore fload depth db
  | depth <= 0 = pure Nothing
  | otherwise  = do
    connection <- open db
    pure . Just $ SplitIndex
      { siHandle        = connection
      , siEvents        = []
      , siBuffered      = []
      , siNotifications = []
      , siDepth         = depth
      , siStore         = fstore
      , siLoad          = fload
      , siIndex         = findex
      }
