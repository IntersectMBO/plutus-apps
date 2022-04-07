module Index.Sqlite
  ( -- * API
    PartialStore(..)
  , SqliteIndex
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

data PartialStore e =
  PartialStore { psConnection    :: Connection
               , psPendingEvents :: [e]
               }

type SqliteIndex e n = SplitIndex IO (PartialStore e) e n

new
  :: (PartialStore e -> [e] -> IO (PartialStore e, [n]))
  -> (PartialStore e -> IO (PartialStore e))
  -> Int
  -> FilePath
  -> IO (Maybe (SqliteIndex e n))
new findex fstore depth db
  | depth <= 0 = pure Nothing
  | otherwise  = do
    connection <- open db
    pure . Just $ SplitIndex
      { siStoredIx      =  PartialStore { psConnection = connection
                                        , psPendingEvents = []
                                        }
      , siEvents        = []
      , siBuffered      = []
      , siNotifications = []
      , siDepth         = depth
      , siStore         = fstore
      , siIndex         = findex
      }
