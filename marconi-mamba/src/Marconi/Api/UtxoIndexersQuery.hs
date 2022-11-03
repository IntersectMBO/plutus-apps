{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
import Control.Exception (bracket_)
import Control.Lens ((^.))
import Control.Monad.STM (STM)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set, fromList, toList, union)
import Data.Text (Text, intercalate, unpack)
import Database.SQLite.Simple (NamedParam ((:=)), open)
import Database.SQLite.Simple qualified as SQL
import Ledger.Tx.CardanoAPI (ToCardanoError (DeserialisationError, Tag, TxBodyError))
import Marconi.Api.Types (DBConfig (DBConfig, utxoConn),
                          DBQueryEnv (DBQueryEnv, _DbConf, _Network, _QueryAddresses, _QueryComm),
                          HasDBQueryEnv (dbConf, queryAddresses, queryComm), HasUtxoQueryComm (indexer, queryReq),
                          TargetAddresses, UtxoQueryComm (UtxoQueryComm, _Indexer, _QueryReq),
                          UtxoTxOutReport (UtxoTxOutReport))
import Marconi.Index.Utxo (UtxoIndex, UtxoRow (UtxoRow, _reference), toRows)
import Marconi.Types (CardanoAddress, TxOutRef)
import RewindableIndex.Index.VSqlite qualified as Ix

bootstrap
    ::  FilePath
    -> TargetAddresses
    -> CApi.NetworkId
    -> IO DBQueryEnv
bootstrap dbPath targetAddresses nId = do
    dbconf <- DBConfig <$> open dbPath
    _QueryReq <- atomically (newEmptyTMVar :: STM (TMVar () ) )
    _Indexer <- atomically (newEmptyTMVar :: STM ( TMVar UtxoIndex) )
    pure $ DBQueryEnv
        { _DbConf = dbconf
        , _QueryComm = UtxoQueryComm {..}
        , _QueryAddresses = targetAddresses
        , _Network = nId
        }

findAll
    :: DBQueryEnv
    -> IO (Set UtxoTxOutReport)    -- ^ set of corresponding TxOutRefs
findAll env = fromList <$> forConcurrently addresses f
    where
        addresses = NonEmpty.toList (env ^. queryAddresses)
        f  :: CardanoAddress -> IO (UtxoTxOutReport)
        f addr = UtxoTxOutReport (CApi.serialiseAddress addr) <$> findByCardanoAddress env (CApi.toAddressAny addr)

utxoQuery:: DBConfig -> CApi.AddressAny -> IO (Set TxOutRef)
utxoQuery dbConfig address = SQL.queryNamed (utxoConn dbConfig)
                  "SELECT * FROM utxos WHERE utxos.address=:address"
                  [":address" := address]
                  >>= pure . fromList . fmap _reference

findByCardanoAddress
    :: DBQueryEnv
    -> CApi.AddressAny
    -> IO (Set TxOutRef)    -- ^ To Plutus address conversion error may occure
findByCardanoAddress env address = withQueryAction env  address utxoQuery

-- | Retrieve a Set of TxOutRefs associated with the given Cardano Era address
-- We return an empty Set if no address is found
findByAddress
    :: DBQueryEnv
    -> Text                                         -- ^ Bech32 Address
    -> IO (Either ToCardanoError UtxoTxOutReport)   -- ^ To Plutus address conversion error may occure
findByAddress env addressText =
    let
        f :: Either CApi.Bech32DecodeError CardanoAddress -> IO (Either ToCardanoError UtxoTxOutReport)
        f (Right address)
            | address `elem` (env ^. queryAddresses) =
              (pure . CApi.toAddressAny $ address)
              >>= findByCardanoAddress env
              >>= pure . Right . UtxoTxOutReport addressText
            | otherwise = pure . Left . TxBodyError $
              unpack addressText <> " not in the provided target addresses"
        f (Left e) = pure . Left $ Tag (unpack  addressText
                     <> " generated error: "
                     <> show e)
                     DeserialisationError
    in
        f $ CApi.deserialiseFromBech32 CApi.AsShelleyAddress addressText


queryInMemory :: CApi.AddressAny -> UtxoIndex-> IO ( Set TxOutRef )
queryInMemory address ix =
      Ix.getBuffer (ix ^. Ix.storage)
      >>=  pure
            . fromList
            . fmap _reference
            . filter (isTargetAddress address)
            . concatMap toRows

isTargetAddress :: CApi.AddressAny -> UtxoRow -> Bool
isTargetAddress address (UtxoRow a _ ) =  address == a

withQueryAction
    :: DBQueryEnv
    -> CApi.AddressAny
    -> (DBConfig -> CApi.AddressAny -> IO (Set TxOutRef) )
    -> IO (Set TxOutRef)
withQueryAction env address qAction =
    let
        qreq = env ^. queryComm . queryReq
        action =  do
            ndxr <- atomically $ takeTMVar (env ^. queryComm . indexer)
            (fromColdStore, fromHotStore) <- concurrently
                (qAction (env ^. dbConf) address)
                (queryInMemory address ndxr )
            pure . union fromColdStore $ fromHotStore
    in
        bracket_
            (atomically $ putTMVar qreq () )
            (atomically $ putTMVar qreq () )
            action

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
