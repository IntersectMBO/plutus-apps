{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Marconi.DB.SqlUtils where

import Cardano.Api qualified as CApi
import Control.Concurrent.Async (forConcurrently_)
import Control.Lens ((^.))
import Control.Monad (void)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import Ledger qualified as Plutus
import Ledger.Tx.CardanoAPI.Internal (toCardanoAddressInEra)
import Marconi.Api.Types (DBQueryEnv (..), HasDBQueryEnv (..), utxoConn)
import Marconi.Api.UtxoIndexersQuery (withQueryAction)
data ShelleyFrequencyTable a = ShelleyFrequencyTable
    { _sAddress   :: !a
    , _sFrequency :: Int
    } deriving Generic

instance (FromField a) => FromRow (ShelleyFrequencyTable a) where
    fromRow = ShelleyFrequencyTable <$> field <*> field
instance (ToField a) => ToRow (ShelleyFrequencyTable a )where
  toRow (ShelleyFrequencyTable ad f) = toRow(ad, f)

bootstrap :: IO ()
bootstrap =  putStrLn "hello"

freqUtxoTable :: DBQueryEnv -> IO ()
freqUtxoTable env = do
    let conn = env ^. dbConf . utxoConn
        qsem = env ^. queryQSem
    void $ withQueryAction (SQL.execute_ conn
                            "drop table if exists frequtxos") qsem
    void $ withQueryAction (SQL.execute_ conn
                            "drop table if exists shelleyaddresses") qsem
    void $ withQueryAction (SQL.execute_ conn
                            "create table frequtxos as select address, count (address) as frequency from utxos group by address order by frequency DESC") qsem
    void $ withQueryAction ( SQL.execute_ conn
                             "create TABLE shelleyaddresses (address text not null, frequency int not null)" ) qsem
    pure ()

freqShelleyTable :: DBQueryEnv -> IO [Text]
freqShelleyTable env = do
    let conn = env ^. dbConf . utxoConn
        qsem = env ^. queryQSem
        nid = env ^. network
    addressFreq <- withQueryAction (SQL.query_ conn
                                "SELECT address, frequency FROM frequtxos") qsem :: IO [ShelleyFrequencyTable Plutus.Address]
    let addresses = catMaybes . fmap (toShelly' nid  ) $ addressFreq

    SQL.execute_ conn "BEGIN TRANSACTION"
    forConcurrently_  addresses ( \(ShelleyFrequencyTable a f) ->
                                      (SQL.execute conn
                                       "insert into shelleyaddresses (address, frequency) values (?, ?)"
                                       (a, f)))
    SQL.execute_ conn "COMMIT"

    pure . fmap _sAddress $ addresses

toShelly :: CApi.NetworkId  -> Plutus.Address -> Maybe Text
toShelly nid address = case toCardanoAddressInEra nid address of
            Left _     -> Nothing
            Right addr -> Just $ CApi.serialiseAddress addr

toShelly' :: CApi.NetworkId  -> ShelleyFrequencyTable Plutus.Address -> Maybe (ShelleyFrequencyTable Text)
toShelly' nid (ShelleyFrequencyTable a f) = let
    b = toShelly nid a
    in fmap (flip ShelleyFrequencyTable f) b
