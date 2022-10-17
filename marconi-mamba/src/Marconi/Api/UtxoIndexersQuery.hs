module Marconi.Api.UtxoIndexersQuery where

import Control.Concurrent.STM.TMVar (newEmptyTMVarIO)
import Data.Set (Set)
import Data.Set qualified as Set (empty, insert)
import Data.Text (Text, pack, unpack)
import Database.SQLite.Simple (Connection, open)
import Ledger.Address (Address)
import Ledger.Tx (TxOutRef)
import Ledger.Tx.CardanoAPI.Internal (ToCardanoError (DeserialisationError, Tag), fromCardanoAddress)
import Marconi.Api.Types
import Marconi.Index.Utxo qualified as Utxo

bootstrap
    ::  FilePath
    -> TargetAddresses
    -> IO DBQueryEnv
bootstrap dbPath targetAddresses = do
    dbconf <- DbConfig <$> open dbPath
    querytmvar <- newEmptyTMVarIO
    pure $ DBQueryEnv
        {_dbConf = dbconf
        , _queryTMVar = querytmvar
        , _queryAddresses = targetAddresses}

findByAddress
    :: DBQueryEnv
    -> Text                                         -- ^ Bech32 Address
    -> IO (Either ToCardanoError (Set TxOutRef) )   -- ^ To Plutus address conversion error may occure
findByAddress = undefined

findByPlutusAddress
    :: DBQueryEnv
    -> Address              -- ^ Plutus address
    -> IO (Set TxOutRef)    -- ^ set of corresponding TxOutRefs
findByPlutusAddress  = undefined

findAll
    :: DBQueryEnv
    -> IO [(k,Set v)]
findAll = undefined
