module Plutus.HystericalScreams.Index.VSqlite
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
   -- * Observations
  , S.view
  , S.getHistory
  , S.getNotifications
  ) where

import Control.Monad.Primitive (PrimState)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Database.SQLite.Simple (Connection, open)

import Plutus.HystericalScreams.Index.VSplit (SplitIndex (..), Storage (..))
import Plutus.HystericalScreams.Index.VSplit qualified as S

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
      { _handle        = connection
      , _storage = Storage { _events = vector
                           , _cursor = 0
                           , _eSize  = 0
                           , _bSize  = 0
                           , _k      = k'
                           }
      , _notifications = []
      , _store         = fstore
      , _query         = fquery
      , _onInsert      = foninsert
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

