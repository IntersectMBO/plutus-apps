{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module extracts Shelley addresses from a utxo SQLite database.
--   Addresses are:
--      store in shelleyaddresses table
--      stored in `Text` Bech32 format, Shelly addresses
--      ranked on their corresponding number of utxos
--
-- to get a sample of the data :
--  sqlite3 ./.marconidb/2/utxo-db "select * from shelleyaddresses limit 10;" ".exit"
module Marconi.DB.SqlUtils where

import Cardano.Api qualified as C
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (bracket_)
import Control.Lens ((^.))
import Control.Monad (void)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Database.SQLite.Simple (Connection, execute, execute_, query_)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import Marconi.Api.Types (DBConfig, DBQueryEnv, HasDBQueryEnv (dbConf, network), utxoConn)

-- | Represents Shelley type addresses with most utxo transactions
--
data ShelleyFrequencyTable a = ShelleyFrequencyTable
    { _sAddress   :: !a
    , _sFrequency :: Int
    } deriving Generic

instance (FromField a) => FromRow (ShelleyFrequencyTable a) where
    fromRow = ShelleyFrequencyTable <$> field <*> field
instance (ToField a) => ToRow (ShelleyFrequencyTable a )where
  toRow (ShelleyFrequencyTable ad f) = toRow(ad, f)

-- | create a small SQL pipeline:
-- first create a table of addresses and their coresponding utxo counts.
-- Next, create the shelleyaddresses table
--
freqUtxoTable :: DBQueryEnv -> IO ()
freqUtxoTable env =
    void $ withQueryAction (env ^. dbConf) ( \conn ->
        execute_ conn "drop table if exists frequtxos"
        >> execute_ conn "drop table if exists shelleyaddresses"
        >> execute_ conn "create table frequtxos as select address, count (address) as frequency from utxos group by address order by frequency DESC"
        >> execute_ conn "delete from frequtxos where frequency < 50" -- we only want `intersing` data
        >> execute_ conn
           "create TABLE shelleyaddresses (address text not null, frequency int not null)")

withQueryAction :: DBConfig -> (Connection -> IO a) -> IO a
withQueryAction conf action =
     let
         f = do
             now <- getCurrentTime
             putStrLn $ "queryAction started at: "  <> show now
         g = do
             now <- getCurrentTime
             putStrLn $ "queryAction completed at: "  <> show now
        in
         bracket_  f g (action (utxoConn conf ))

-- | populate the shelleyFrequency table
-- first create a table of addresses and their coresponding utxo counts.
-- Next, create the shelleyaddresses table
--
freqShelleyTable :: DBQueryEnv -> IO [Text]
freqShelleyTable env = do
    addressFreq <- withQueryAction (env ^. dbConf)( \conn -> (query_ conn
                                "SELECT address, frequency FROM frequtxos") :: IO [ShelleyFrequencyTable C.AddressAny])
    let addresses = catMaybes . fmap (toShelley ( env ^. network) ) $ addressFreq

    withQueryAction (env ^. dbConf) ( \conn -> (
        -- execute_ conn "BEGIN TRANSACTION"
        forConcurrently_  addresses ( \(ShelleyFrequencyTable a f) ->
                                      (execute conn
                                       "insert into shelleyaddresses (address, frequency) values (?, ?)"
                                       (a, f))))
        -- >> execute_ conn "COMMIT"
        )
    pure . fmap _sAddress $ addresses

-- | we want to store addresses as Text.
-- first conver to cardano address, then seriase to text
--
toShelley :: C.NetworkId  -> ShelleyFrequencyTable C.AddressAny -> Maybe (ShelleyFrequencyTable Text)
toShelley _ (ShelleyFrequencyTable (C.AddressShelley a) f) =
    let
        addrTxt = C.serialiseAddress a
    in
        Just ( ShelleyFrequencyTable addrTxt f)
toShelley _ _ = Nothing
