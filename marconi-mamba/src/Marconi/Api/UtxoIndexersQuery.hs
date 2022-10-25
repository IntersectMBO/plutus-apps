{-# LANGUAGE OverloadedStrings #-}

module Marconi.Api.UtxoIndexersQuery
    ( bootstrap
    , findByPlutusAddress
    , findByCardanoAddress
    , findByAddress
    , findUtxos
    , findTxOutRefs
    , findAll
    , reportQueryAddresses
    , UtxoRow(..)
    , reportQueryCardanoAddresses
    , reportBech32Addresses
    , withQueryAction
    ) where

import Cardano.Api qualified as CApi
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.QSemN (QSemN, newQSemN, signalQSemN, waitQSemN)
import Control.Exception (bracket_)
import Control.Lens ((^.))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set, fromList, toList)
import Data.Text (Text, intercalate, unpack)
import Database.SQLite.Simple (NamedParam ((:=)), open)
import Database.SQLite.Simple qualified as SQL
import Ledger (Address, TxOutRef)
import Ledger.Tx.CardanoAPI (ToCardanoError (DeserialisationError, Tag), fromCardanoAddress)
import Marconi.Api.Types (CardanoAddress, DBConfig (DBConfig),
                          DBQueryEnv (DBQueryEnv, _dbConf, _network, _queryAddresses, _queryQSem),
                          HasDBConfig (utxoConn), HasDBQueryEnv (dbConf, queryAddresses, queryQSem, queryQSem),
                          TargetAddresses, UtxoRowWrapper (UtxoRowWrapper), UtxoTxOutReport (..))
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

findAll
    :: DBQueryEnv
    -> IO (Set UtxoTxOutReport)    -- ^ set of corresponding TxOutRefs
findAll env =
    withQueryAction (findAll' env) (env ^. queryQSem)

findAll'
    :: DBQueryEnv
    -> IO (Set UtxoTxOutReport)    -- ^ set of corresponding TxOutRefs
findAll' env = fromList <$> forConcurrently addresses f
    where
        addresses = NonEmpty.toList (env ^. queryAddresses)
        f  :: CardanoAddress -> IO (UtxoTxOutReport)
        f addr = UtxoTxOutReport (CApi.serialiseAddress addr) <$> findByCardanoAddress' env addr

findByPlutusAddress
    :: DBQueryEnv
    -> Address              -- ^ Plutus address
    -> IO (Set TxOutRef)    -- ^ set of corresponding TxOutRefs
findByPlutusAddress env address =
    withQueryAction (findByPlutusAddress' env address) (env ^. queryQSem)

findByPlutusAddress'
    :: DBQueryEnv
    -> Address              -- ^ Plutus address
    -> IO (Set TxOutRef)
findByPlutusAddress' env address =
    SQL.queryNamed (env ^. dbConf . utxoConn)
                  "SELECT * FROM utxos WHERE utxos.address=:address"
                  [":address" := address]
    >>= pure . fromList . fmap _reference

-- | Retrieve a Set of TxOutRefs associated with the given Cardano Era address
-- We return an empty Set if no address is found
findByCardanoAddress
    :: DBQueryEnv
    -> CApi.Address addrtype                        -- ^ Cardano address and Era
    -> IO (Set TxOutRef)    -- ^ To Plutus address conversion error may occure
findByCardanoAddress env address =
    withQueryAction (findByCardanoAddress' env address) (env ^. queryQSem)

findByCardanoAddress'
    :: DBQueryEnv
    -> CApi.Address addrtype                        -- ^ Cardano address and Era
    -> IO (Set TxOutRef)    -- ^ To Plutus address conversion error may occure
findByCardanoAddress' env = findByPlutusAddress' env . fromCardanoAddress


-- | Retrieve a Set of TxOutRefs associated with the given Cardano Era address
-- We return an empty Set if no address is found
findByAddress
    :: DBQueryEnv
    -> Text                                         -- ^ Bech32 Address
    -> IO (Either ToCardanoError UtxoTxOutReport)   -- ^ To Plutus address conversion error may occure
findByAddress env addressText =
    let
        addressEither :: Either CApi.Bech32DecodeError  CardanoAddress
        addressEither = CApi.deserialiseFromBech32 (CApi.proxyToAsType Proxy) addressText
    in
        case addressEither of
            Right address -> pure . Right . UtxoTxOutReport addressText =<< findByCardanoAddress env address
            Left e        -> pure . Left $ Tag (unpack  addressText <> "generated error: " <> show e) DeserialisationError

findUtxos
    :: DBQueryEnv
    -> IO (Set UtxoRowWrapper)
findUtxos env = withQueryAction (findUtxos' env) (env ^. queryQSem)


findUtxos'
    :: DBQueryEnv
    -> IO (Set UtxoRowWrapper)
findUtxos' env = (SQL.query_
                 (env ^. dbConf . utxoConn)
                 "SELECT address, txId, inputIx FROM utxos limit 100" :: IO [UtxoRow])
            >>= pure . fmap UtxoRowWrapper >>= pure . fromList

findTxOutRefs
    :: DBQueryEnv
    -> IO (Set TxOutRef)
findTxOutRefs env = withQueryAction (findTxOutRefs' env) (env ^. queryQSem)


findTxOutRefs'
    :: DBQueryEnv
    -> IO (Set TxOutRef)
findTxOutRefs' env = (SQL.query_
                    (env ^. dbConf . utxoConn)
                    "SELECT address, txId, inputIx FROM utxos limit 100" :: IO [UtxoRow] )
                >>= pure . fromList . fmap _reference

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
