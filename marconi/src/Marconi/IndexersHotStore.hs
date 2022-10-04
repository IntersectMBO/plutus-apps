{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE UndecidableInstances #-}

{- | IndexerHotstore provides in concurrent in memory cache for marcoin clients
-}
module Marconi.IndexersHotStore(
    puts
    , findByAddress
    , bootstrapHotStore
    , IndexerHotStore
    , TargetAddresses (..)
    , targetAddressParser
    , isInTargetTxOut
    ) where

import Cardano.Api qualified as CApi
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO, readTVarIO)
import Control.Monad.STM (atomically)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty, fromList, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map (adjust, empty, keysSet, lookup, toList)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Set qualified as Set (empty, insert)
import Data.Text (Text, pack, unpack)
import Ledger (Address (..), TxOutRef)
import Ledger.Tx.CardanoAPI (ToCardanoError (..), fromCardanoAddress)
import Marconi.Index.Utxo qualified as Utxo

-- | Key value store where keys point to set of unique values
-- TODO consider using unordered container
newtype KeyValues k v = KeyValues  {unKeyValues :: Map k (Set v) }

-- | Key value hotstore
newtype HotStore k v = HotStore { unHot :: TVar (KeyValues k v) }

-- | Key value hotstore implementation for Plutus Address and Set of TxOutRefs
type IndexerHotStore = HotStore Address  TxOutRef

-- | Allows clients to store a list of TxOutRefs keyed by Plutus address
puts :: IndexerHotStore -- ^ hot store
    -> [Utxo.UtxoRow]  -- ^ UtxoRows to insert into hotstore
    -> IO ()
puts hotStore = traverse_ (put hotStore)

put :: IndexerHotStore -> Utxo.UtxoRow -> IO ()
put hotStore ( Utxo.UtxoRow address txOutRef)  =
    update hotStore address txOutRef

-- | initialize the hotstore for persistence operations
-- Clients should call this function to bootsrap the hotstore
bootstrapHotStore :: IO(HotStore k v)
bootstrapHotStore = newTVarIO m >>= pure . HotStore
    where
        m = KeyValues  Map.empty -- $ Map.fromList [(x, Set.empty) | x <- (toList . nub )ks]

-- | update hotstore
-- TODO consider using unordered containers as there is no requirements for the values to be sorted
update :: (Ord k, Ord v)
    => HotStore k v    -- ^ hotstore
    -> k                -- ^ key to look up in the store
    -> v                -- ^ value to update with
    -> IO ()
update hotStore k v =
    let
        insertValues  = KeyValues . ( Map.adjust (Set.insert v) k ) . unKeyValues
    in
         (atomically . modifyTVar (unHot hotStore)) insertValues

-- | returs a sorted list of keys
keys :: HotStore k v -> IO (Set k)
keys hotStore = pure . Map.keysSet . unKeyValues =<< (readTVarIO . unHot) hotStore

findKey :: Ord k => HotStore k v -> k -> IO (Set v)
findKey hotStore k =  do
    m <- readTVarIO . unHot $ hotStore
    case (Map.lookup k . unKeyValues $ m)  of
        Just vs -> pure vs
        _       -> pure Set.empty

-- | Retrieve a Set of TxOutRefs associated with the given plutus address
-- We return an empty Set if no address is found
findByPlutusAddress
    :: IndexerHotStore      -- ^ hot store
    -> Address              -- ^ Plutus address
    -> IO (Set TxOutRef)    -- ^ set of corresponding TxOutRefs
findByPlutusAddress  =  findKey

-- | Retrieve a Set of TxOutRefs associated with the given Cardano Era address
-- We return an empty Set if no address is found
findByCardanoAddress
    :: IndexerHotStore                              -- ^ hot store
    -> CApi.Address addrtype                        -- ^ Cardano address and Era
    -> IO (Either ToCardanoError (Set TxOutRef) )   -- ^ To Plutus address conversion error may occure
findByCardanoAddress hotStore address =
     case fromCardanoAddress address of
         Left e              -> pure . Left $ Tag (show e) DeserialisationError
         Right plutusAddress -> Right <$> findByPlutusAddress hotStore plutusAddress

-- | Retrieve a Set of TxOutRefs associated with the given Cardano Era address
-- We return an empty Set if no address is found
findByAddress
    :: IndexerHotStore                              -- ^ hot store
    -> Text                                         -- ^ Bech32 Address
    -> IO (Either ToCardanoError (Set TxOutRef) )   -- ^ To Plutus address conversion error may occure
findByAddress hotStore addressText =
    let
        addressEither :: Either CApi.Bech32DecodeError  CardanoAddress
        addressEither = CApi.deserialiseFromBech32 (CApi.proxyToAsType Proxy) addressText
    in
        case addressEither of
            Right address -> findByCardanoAddress hotStore address
            Left _        -> pure . Left $ Tag (unpack  addressText) DeserialisationError


findAll :: HotStore k v -> IO [(k,Set v)]
findAll hotStore =
     (readTVarIO . unHot $ hotStore) >>= (pure . Map.toList . unKeyValues)

type CardanoAddress = CApi.Address CApi.ShelleyAddr

-- | Typre represents map of cardano addresses to Ledger TxOutRef
type IndexerStoreMap = (Map(CardanoAddress) TxOutRef)

-- | Typre represents non empty list of Bech32 compatable addresses"
type TargetAddresses = NonEmpty CardanoAddress

-- | does the transaction contain a targetAddress
isInTargetTxOut
    :: TargetAddresses
    -- ^ non empty list of target address
    -> CApi.TxOut CApi.CtxTx era
    -- ^  a cardano transaction out that contains an address
    -> Bool
isInTargetTxOut targetAddresses (CApi.TxOut address _ _) = case  address of
    (CApi.AddressInEra  (CApi.ShelleyAddressInEra _) addr) -> addr `elem` targetAddresses
    _                                                      -> False

-- | does the transaction contain a targetAddress
isTargetTxOut
    :: CardanoAddress
    -- ^  a cardano shelley era adddress
    -> CApi.TxOut CApi.CtxTx era
    -- ^  a cardano transaction out that contains an address
    -> Bool
isTargetTxOut targetaddress (CApi.TxOut address _ _) = case  address of
    (CApi.AddressInEra  (CApi.ShelleyAddressInEra _) addr) -> addr == targetaddress
    _                                                      -> False

-- | parses a white space separated address list
-- Note, duplicate addresses are rmoved
targetAddressParser
    :: String -- ^ contains white spece delimeted lis of addresses
    -> TargetAddresses -- ^ a non empty list of valid addresses
targetAddressParser =  nub . fromList . fromJustWithError . traverse (deserializeToCardano . pack) . words

deserializeToCardano :: Text -> Maybe CardanoAddress
deserializeToCardano = either (const Nothing) Just . CApi.deserialiseFromBech32 (CApi.proxyToAsType Proxy)

-- | exit program with error
-- Note, if the targetAddress parser fails, or is empty, there is nothing to do for the hotStore.
-- In such case we should fail fast
fromJustWithError :: Maybe a -> a
fromJustWithError (Just a) = a
fromJustWithError _        = error "Empty or Invalid address list.  Addresses must be Bech32 compatable!"
