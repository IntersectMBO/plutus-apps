module Marconi.Api.IndexerCache where

import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO, readTVarIO)
import Control.Monad.STM (atomically)
import Data.List.NonEmpty (NonEmpty, nub, toList)
import Data.Map.Strict (Map, adjust, fromList, keysSet)
import Data.Set (Set, empty, insert)


type ApiKeyValue k v = Map k (Set v)

newtype ApiCache k v = ApiCache { unCache :: TVar (ApiKeyValue k v) }

newtype ApiCacheKeys k = ApiCacheKeys { unKey :: NonEmpty k }

initCache :: Ord k => (NonEmpty k) -> IO(ApiCache k v)
initCache ks = newTVarIO m >>= pure . ApiCache
    where
        m = fromList [(x, empty) | x <- (toList . nub )ks]

updateCache :: (Ord v, Ord k) => k -> v -> ApiCache k v -> IO ()
updateCache k v cache = (atomically . modifyTVar tvar) (insertValues v k)
    where
        insertValues x = adjust (insert x)
        tvar =unCache cache

keys :: ApiCache k v -> IO (Set k)
keys cache = pure . keysSet  =<< (readTVarIO . unCache) cache

findByKey :: k -> ApiCache k v -> IO v
findByKey = undefined

findAll :: ApiCache k v -> IO [v]
findAll = undefined
