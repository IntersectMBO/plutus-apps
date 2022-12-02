module Marconi.Api.UtxoIndexersQuery
    ( bootstrap
    , findByCardanoAddress
    , findByAddress
    , findAll
    , reportQueryAddresses
    , UtxoRow(..)
    , reportQueryCardanoAddresses
    , reportBech32Addresses
    , withQueryAction
    ) where
import Cardano.Api qualified as CApi
import Control.Concurrent.Async (concurrently, forConcurrently)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, putTMVar, takeTMVar)
import Control.Exception (bracket)
import Control.Lens ((^.))
import Control.Monad.STM (STM)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set, fromList, toList, union)
import Data.Text (Text, intercalate, unpack)
import Database.SQLite.Simple (NamedParam ((:=)), execute_, open)
import Database.SQLite.Simple qualified as SQL
import Marconi.Api.Types (DBConfig (DBConfig, utxoConn),
                          DBQueryEnv (DBQueryEnv, _DbConf, _Network, _QueryAddresses, _QueryTMVar),
                          HasDBQueryEnv (dbConf, queryAddresses, queryTMVar),
                          QueryExceptions (AddressNotInListError, QueryError), TargetAddresses,
                          UtxoQueryTMVar (UtxoQueryTMVar), UtxoTxOutReport (UtxoTxOutReport), unUtxoIndex)
import Marconi.Index.Utxo (UtxoIndex, UtxoRow (UtxoRow, _reference), toRows)
import Marconi.Types (CardanoAddress, TxOutRef)
import RewindableIndex.Index.VSqlite qualified as Ix

-- | Bootstraps the utxo query environment.
-- The module is responsible for accessing SQLite for quries.
-- The main issue we try to avoid here is mixing inserts and quries in SQLite to avoid locking the database
bootstrap
    ::  FilePath                -- ^ file path the SQLite utxos database
    -> TargetAddresses          -- ^ user provided target addresses
    -> CApi.NetworkId           -- ^ cardano networkId
    -> IO DBQueryEnv            -- ^ returns Query runtime environment
bootstrap dbPath targetAddresses nId = do
    connection <- open dbPath
    execute_ connection "PRAGMA journal_mode=WAL"
    ix <- atomically (newEmptyTMVar :: STM ( TMVar UtxoIndex) )
    pure $ DBQueryEnv
        { _DbConf = DBConfig connection
        , _QueryTMVar = UtxoQueryTMVar ix
        , _QueryAddresses = targetAddresses
        , _Network = nId
        }
-- | finds reports for all user-provided addresses.
-- TODO we need to use streaming
--
findAll
    :: DBQueryEnv                   -- ^ Query run time environment
    -> IO (Set UtxoTxOutReport)     -- ^ set of corresponding TxOutRefs
findAll env = fromList <$> forConcurrently addresses f
    where
        addresses = NonEmpty.toList (env ^. queryAddresses)
        f  :: C.Address C.ShelleyAddr -> IO (UtxoTxOutReport)
        f addr = UtxoTxOutReport (CApi.serialiseAddress addr) <$> findByCardanoAddress env (CApi.toAddressAny addr)

-- | Query utxos address address
--
utxoQuery:: DBConfig -> CApi.AddressAny -> IO (Set TxOutRef)
utxoQuery dbConfig address = SQL.queryNamed (utxoConn dbConfig)
                  "SELECT txid, inputIx FROM utxos WHERE utxos.address=:address"
                  [":address" := address]
                  >>= pure . fromList

-- | Query utxos by Cardano Address
--  To Cardano error may occure
findByCardanoAddress
    :: DBQueryEnv           -- ^ Query run time environment
    -> CApi.AddressAny      -- ^ Cardano address to query
    -> IO (Set TxOutRef)
findByCardanoAddress env address = withQueryAction env  address utxoQuery

-- | Retrieve a Set of TxOutRefs associated with the given Cardano Era address
-- We return an empty Set if no address is found
findByAddress
    :: DBQueryEnv                                   -- ^ Query run time environment
    -> Text                                         -- ^ Bech32 Address
    -> IO (Either QueryExceptions UtxoTxOutReport)   -- ^ To Plutus address conversion error may occure
findByAddress env addressText =
    let
        f :: Either CApi.Bech32DecodeError CardanoAddress -> IO (Either QueryExceptions UtxoTxOutReport)
        f (Right address)
            | address `elem` (env ^. queryAddresses) = -- allow for targetAddress search only
              (pure . CApi.toAddressAny $ address)
              >>= findByCardanoAddress env
              >>= pure . Right . UtxoTxOutReport addressText
            | otherwise = pure . Left . AddressNotInListError . QueryError $
              unpack addressText <> " not in the provided target addresses"
        f (Left e) = pure . Left $ QueryError (unpack  addressText
                     <> " generated error: "
                     <> show e)

    in
        f $ CApi.deserialiseFromBech32 CApi.AsShelleyAddress addressText

-- | query in-momory utxos for the given address
--
queryInMemory
    :: CApi.AddressAny          -- ^ address to query
    -> UtxoIndex                -- ^ inmemory, hot-store, storage for utxos
    -> IO ( Set TxOutRef )
queryInMemory address ix =
    let
        isTargetAddress :: UtxoRow -> Bool
        isTargetAddress (UtxoRow a _ ) =  address == a
    in
        Ix.getBuffer (ix ^. Ix.storage)
        >>=  pure
            . fromList
            . fmap _reference
            . filter isTargetAddress
            . concatMap toRows

-- | Execute the query function
-- We must stop the utxo inserts before doing the query
withQueryAction
    :: DBQueryEnv                                           -- ^ Query run time environment
    -> CApi.AddressAny                                      -- ^ Cardano address to query
    -> (DBConfig -> CApi.AddressAny -> IO (Set TxOutRef) )  -- ^ Query function to run
    -> IO (Set TxOutRef)
withQueryAction env address qAction =
    let
        utxoIndexer = unUtxoIndex  $ env ^. queryTMVar
        action ndxr =  do
            (fromColdStore, fromHotStore) <- concurrently
                (qAction (env ^. dbConf) address)
                (queryInMemory address ndxr )
            pure . union fromColdStore $ fromHotStore
    in
        bracket
          (atomically $ takeTMVar  utxoIndexer)
          (atomically . (putTMVar utxoIndexer))
          action

-- | report target addresses
-- Used by JSON-RPC
reportQueryAddresses
    :: DBQueryEnv
    -> IO (Set CardanoAddress)
reportQueryAddresses env
    = pure
    . fromList
    . NonEmpty.toList
    $ (env ^. queryAddresses )

reportQueryCardanoAddresses
    :: DBQueryEnv
    -> Text
reportQueryCardanoAddresses  = intercalate ", " . toList . reportBech32Addresses

reportBech32Addresses
    :: DBQueryEnv
    -> Set Text
reportBech32Addresses env
    = fromList
    . NonEmpty.toList
    . fmap CApi.serialiseAddress
    $ (env ^. queryAddresses )
