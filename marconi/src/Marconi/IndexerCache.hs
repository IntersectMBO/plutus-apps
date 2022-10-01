{-# LANGUAGE GADTs #-}

module Marconi.IndexerCache where

import Cardano.Api qualified
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO, readTVar, readTVarIO)
import Control.Monad.STM (atomically)
import Data.List.NonEmpty (NonEmpty, fromList, nub, toList)
import Data.Map.Strict qualified as Map (Map, adjust, fromList, keysSet)
import Data.Set qualified as Set (Set, empty, insert)
import Ledger (TxOutRef)

type ApiKeyValue k v = Map.Map k (Set.Set v)

newtype ApiCacheKeys k = ApiCacheKeys { unKey :: NonEmpty k }

newtype ApiCache k v = ApiCache { unCache :: TVar (ApiKeyValue k v) }

initCache :: Ord k => NonEmpty k -> IO(ApiCache k v)
initCache ks = newTVarIO m >>= pure . ApiCache
    where
        m = Map.fromList [(x, Set.empty) | x <- (toList . nub )ks]

updateCache :: (Ord v, Ord k) => k -> v -> ApiCache k v -> IO ()
updateCache k v cache = (atomically . modifyTVar tvar) (insertValues v k)
    where
        insertValues x = Map.adjust (Set.insert x)
        tvar =unCache cache

keys :: ApiCache k v -> IO (Set.Set k)
keys cache = pure . Map.keysSet  =<< (readTVarIO . unCache) cache

findByKey :: k -> ApiCache k v -> IO v
findByKey = undefined

findAll :: ApiCache k v -> IO [v]
findAll = undefined

defaultCache :: IO (ApiCache Char Char)
defaultCache = initCache (fromList "")

-- | Typre represents map of cardano addresses to Ledger TxOutRef
type AddressTxOutRefMap = (Map.Map(Cardano.Api.Address Cardano.Api.ShelleyAddr) TxOutRef)

type AddressTxOutRefCache = TVar AddressTxOutRefMap

-- | Typre represents non empty list of Bech32 compatable addresses"
type TargetAddresses = NonEmpty (Cardano.Api.Address Cardano.Api.ShelleyAddr )

-- | does the transaction contain a targetAddress
isTargetTxOut
    :: TargetAddresses
    -- ^ non empty list of target address
    -> Cardano.Api.TxOut Cardano.Api.CtxTx era
    -- ^  a cardano transaction out that contains an address
    -> Bool
isTargetTxOut targetAddresses (Cardano.Api.TxOut address _ _) = case  address of
    (Cardano.Api.AddressInEra  (Cardano.Api.ShelleyAddressInEra _) addr) -> addr `elem` targetAddresses
    _                                                                    -> False

type IndexerAddressCache = ApiCache (Cardano.Api.Address Cardano.Api.ShelleyAddr ) TxOutRef
