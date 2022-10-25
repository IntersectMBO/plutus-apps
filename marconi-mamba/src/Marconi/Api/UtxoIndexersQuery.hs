{-# LANGUAGE OverloadedStrings #-}

module Marconi.Api.UtxoIndexersQuery
    ( bootstrap
    , findByPlutusAddress
    , findByCardanoAddress
    , findByAddress
    , findUtxos
    , findTxOutRefs
    , reportQueryAddresses
    , UtxoRow(..)
    , reportQueryCardanoAddresses
    , withQueryAction
    , withDBAction
    ) where

import Cardano.Api qualified as CApi
import Control.Concurrent.QSemN (QSemN, newQSemN, signalQSemN, waitQSemN)
import Control.Exception (bracket_)
import Control.Lens ((^.))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set, fromList)
import Data.Text (Text, intercalate, unpack)
import Database.SQLite.Simple (NamedParam ((:=)), open)
import Database.SQLite.Simple qualified as SQL
import Ledger (Address, TxOutRef)
import Ledger.Tx.CardanoAPI (ToCardanoError (DeserialisationError, Tag), fromCardanoAddress)
import Marconi.Api.Types (CardanoAddress, DBConfig (DBConfig),
                          DBQueryEnv (DBQueryEnv, _dbConf, _network, _queryAddresses, _queryQSem),
                          HasDBConfig (utxoConn), HasDBQueryEnv (dbConf, queryAddresses, queryQSem, queryQSem),
                          TargetAddresses, UtxoRowWrapper (UtxoRowWrapper))
import Marconi.Index.Utxo (UtxoRow (UtxoRow, _reference))

bootstrap
    ::  FilePath
    -> TargetAddresses
    -> CApi.NetworkId
    -> IO DBQueryEnv
bootstrap dbPath targetAddresses nId = do
    dbconf <- DBConfig <$> open dbPath
    qsem <- newQSemN 1
    pure $ DBQueryEnv
        {_dbConf = dbconf
        , _queryQSem = qsem
        , _queryAddresses = targetAddresses
        , _network = nId
        }



findByPlutusAddress
    :: DBQueryEnv
    -> Address              -- ^ Plutus address
    -> IO (Set TxOutRef)    -- ^ set of corresponding TxOutRefs
findByPlutusAddress env address = pure . fromList . fmap _reference =<< withQueryAction action (env ^. queryQSem)
    where
        action :: IO [UtxoRow]
        action = SQL.queryNamed
                  (env ^. dbConf . utxoConn)
                  "SELECT * FROM utxos WHERE utxos.address=:address"
                  [":address" := address]

-- | Retrieve a Set of TxOutRefs associated with the given Cardano Era address
-- We return an empty Set if no address is found
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
    -> IO (Either ToCardanoError (Set TxOutRef) )   -- ^ To Plutus address conversion error may occure
findByAddress env addressText =
    let
        addressEither :: Either CApi.Bech32DecodeError  CardanoAddress
        addressEither = CApi.deserialiseFromBech32 (CApi.proxyToAsType Proxy) addressText
    in
        case addressEither of
            Right address -> pure . Right =<< findByCardanoAddress env address
            Left e        -> pure . Left $ Tag (unpack  addressText <> "generated error: " <> show e) DeserialisationError

findUtxos
    :: DBQueryEnv
    -> IO (Set UtxoRowWrapper)
findUtxos env = withQueryAction action (env ^. queryQSem)
    where
        action =
                (SQL.query_
                 (env ^. dbConf . utxoConn)
                 "SELECT address, txId, inputIx FROM utxos limit 100" :: IO [UtxoRow])
            >>= pure . fmap UtxoRowWrapper >>= pure . fromList

findTxOutRefs
    :: DBQueryEnv
    -> IO (Set TxOutRef)
findTxOutRefs env = withQueryAction action (env ^. queryQSem)
    where
        action =
                (SQL.query_
                    (env ^. dbConf . utxoConn)
                    "SELECT address, txId, inputIx FROM utxos limit 100" :: IO [UtxoRow] )
                >>= pure . fromList . fmap _reference

withDBAction
    :: IO a     -- ^ connection
    -> DBQueryEnv    -- ^ database query
    -> IO a

withDBAction  action env = bracket_ (pure ()) (SQL.close conn) action
    where
        conn = env ^. dbConf . utxoConn
withQueryAction
    :: IO a     -- ^ connection
    -> QSemN    -- ^ database query
    -> IO a
withQueryAction action qsem = bracket_ (waitQSemN qsem 1) (signalQSemN qsem 1) action

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
    -> IO Text
reportQueryCardanoAddresses env
    = pure
    . intercalate ", "
    . NonEmpty.toList
    . fmap CApi.serialiseAddress
    $ (env ^. queryAddresses )
