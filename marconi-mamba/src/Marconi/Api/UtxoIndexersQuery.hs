{-# LANGUAGE OverloadedStrings #-}

module Marconi.Api.UtxoIndexersQuery
    ( bootstrap
    , findByPlutusAddress
    , findByCardanoAddress
    , findByAddress
    , findUtxos
    , findTxOutRefs
    , UtxoRow(..)
    ) where

import Cardano.Api qualified as CApi
import Control.Concurrent.QSemN (QSemN, newQSemN, signalQSemN, waitQSemN)
import Control.Exception (bracket_)
import Control.Lens ((^.))
import Data.Aeson (ToJSON (..), defaultOptions, genericToEncoding)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set, fromList)
import Data.Text (Text, unpack)
import Database.SQLite.Simple (NamedParam (..), open)
import Database.SQLite.Simple qualified as SQL
import Ledger (Address (..), TxOutRef)
import Ledger.Tx.CardanoAPI (ToCardanoError (..), fromCardanoAddress)
import Marconi.Api.Types (CardanoAddress, DBConfig (..), DBQueryEnv (..), HasDBConfig (..), HasDBQueryEnv (..),
                          TargetAddresses)
import Marconi.Index.Utxo (UtxoRow (..))
import Marconi.Index.Utxo qualified as Utxo

bootstrap
    ::  FilePath
    -> TargetAddresses
    -> IO DBQueryEnv
bootstrap dbPath targetAddresses = do
    dbconf <- DBConfig <$> open dbPath
    qsem <- newQSemN 1
    pure $ DBQueryEnv
        {_dbConf = dbconf
        , _queryQSem = qsem
        , _queryAddresses = targetAddresses}

findByPlutusAddress
    :: DBQueryEnv
    -> Address              -- ^ Plutus address
    -> IO (Set TxOutRef)    -- ^ set of corresponding TxOutRefs
findByPlutusAddress env address = withQueryAction (env ^. queryQSem) action
    where
        action = (fromList <$> SQL.queryNamed
                    (env ^. dbConf . utxoConn)
                    "SELECT txId FROM utxos WHERE utxos.address=:address"
                    [":address" := address])

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
    -> IO (Set UtxoRow)
findUtxos env = withQueryAction (env ^. queryQSem) action
    where
        action =
                (SQL.query_
                 (env ^. dbConf . utxoConn)
                 "SELECT address, txId, inputIx FROM utxos limit 100" :: IO [UtxoRow])
            >>= pure . fromList

findTxOutRefs
    :: DBQueryEnv
    -> IO (Set TxOutRef)
findTxOutRefs env = withQueryAction (env ^. queryQSem) action
    where
        action =
                (SQL.query_
                    (env ^. dbConf . utxoConn)
                    "SELECT address, txId, inputIx FROM utxos limit 100" :: IO [UtxoRow] )
                >>= pure . fromList . fmap _reference

withQueryAction :: QSemN -> IO a -> IO a
withQueryAction qsem action = bracket_ (waitQSemN qsem 1) (signalQSemN qsem 1) action

instance Ord UtxoRow where
    compare (UtxoRow a _) (UtxoRow b _) =  compare a b

instance Eq UtxoRow where
    (UtxoRow a1 t1) == (UtxoRow a2 t2) = a1 == a2 &&  t1 == t2

instance ToJSON UtxoRow where
    toEncoding = genericToEncoding defaultOptions
