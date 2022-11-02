{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Marconi.Api.UtxoIndexersQuery
    ( bootstrap
    , findByPlutusAddress
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
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set, fromList, toList, union)
import Data.Text (Text, intercalate, unpack)
import Database.SQLite.Simple (NamedParam ((:=)), open)
import Database.SQLite.Simple qualified as SQL
import Ledger (Address, TxOutRef)
import Ledger.Tx.CardanoAPI (ToCardanoError (DeserialisationError, Tag, TxBodyError), fromCardanoAddress)
import Marconi.Api.Types (CardanoAddress, DBConfig (DBConfig, utxoConn),
                          DBQueryEnv (DBQueryEnv, _DbConf, _Network, _QueryAddresses, _QueryComm),
                          HasDBQueryEnv (dbConf, queryAddresses, queryComm), HasUtxoQueryComm (indexer, queryReq),
                          TargetAddresses, UtxoQueryComm (UtxoQueryComm, _Indexer, _QueryReq),
                          UtxoTxOutReport (UtxoTxOutReport))
import Marconi.Index.Utxo (UtxoIndex, UtxoRow (UtxoRow, _reference), toRows)
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
        f addr = UtxoTxOutReport (CApi.serialiseAddress addr) <$> findByCardanoAddress env addr

utxoQuery:: DBConfig -> Address -> IO (Set TxOutRef)
utxoQuery dbConfig address = SQL.queryNamed (utxoConn dbConfig)
                  "SELECT * FROM utxos WHERE utxos.address=:address"
                  [":address" := address]
                  >>= pure . fromList . fmap _reference

findByPlutusAddress
    :: DBQueryEnv
    -> Address              -- ^ Plutus address
    -> IO (Set TxOutRef)
findByPlutusAddress env address =
        withUtxoQueryAction env  address utxoQuery


findByCardanoAddress
    :: DBQueryEnv
    -> CApi.Address addrtype                        -- ^ Cardano address and Era
    -> IO (Set TxOutRef)    -- ^ To Plutus address conversion error may occure
findByCardanoAddress env = findByPlutusAddress env . fromCardanoAddress

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
            | address `elem` (env ^. queryAddresses) = pure . Right . UtxoTxOutReport addressText
              =<< findByCardanoAddress env address
            | otherwise = pure . Left . TxBodyError $
              unpack addressText <> " not in the provided target addresses"
        f (Left e) = pure . Left $ Tag (unpack  addressText
                     <> " generated error: "
                     <> show e)
                     DeserialisationError
    in
         f $ CApi.deserialiseFromBech32 (CApi.proxyToAsType Proxy) addressText


queryInMemory :: Address -> UtxoIndex-> IO ( Set TxOutRef )
queryInMemory address ix =
      Ix.getBuffer (ix ^. Ix.storage)
      >>=  pure
            . fromList
            . fmap _reference
            . filter (isTargetAddress address)
            . concatMap toRows

isTargetAddress :: Address -> UtxoRow -> Bool
isTargetAddress address (UtxoRow a _ ) =  address == a

withQueryAction
    :: DBQueryEnv
    -> Address
    -> (DBConfig -> Address -> IO (Set TxOutRef) )
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

withUtxoQueryAction
    :: DBQueryEnv
    -> Address
    -> (DBConfig -> Address -> IO (Set TxOutRef) )
    -> IO (Set TxOutRef)
withUtxoQueryAction  = withQueryAction


reportQueryAddresses
    :: DBQueryEnv
    -> IO (Set Address)
reportQueryAddresses env
    = pure
    . fromList
    . NonEmpty.toList
    . fmap fromCardanoAddress
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
