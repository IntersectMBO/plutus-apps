{-# LANGUAGE LambdaCase #-}

module Marconi.Api.UtxoIndexersQuery
    ( bootstrap
    , findByCardanoAddress
    , findByAddress
    , findAll
    , reportQueryAddresses
    , Utxo.UtxoRow(..)
    , Utxo.UtxoIndex
    , reportQueryCardanoAddresses
    , reportBech32Addresses
    , withQueryAction
    , writeTMVar
    ) where
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, putTMVar, takeTMVar, tryTakeTMVar)
import Control.Exception (bracket)
import Control.Lens ((^.))
import Control.Monad.STM (STM)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text, intercalate, pack, unpack)

import Cardano.Api qualified as C
import Marconi.Api.Types (DBQueryEnv (DBQueryEnv, _queryAddresses, _queryTMVar),
                          HasDBQueryEnv (queryAddresses, queryTMVar),
                          QueryExceptions (AddressNotInListError, QueryError), TargetAddresses,
                          UtxoTxOutReport (UtxoTxOutReport))
import Marconi.Index.Utxo qualified as Utxo
import Marconi.Indexers (UtxoQueryTMVar (UtxoQueryTMVar, unUtxoIndex))

-- | Bootstraps the utxo query environment.
-- The module is responsible for accessing SQLite for quries.
-- The main issue we try to avoid here is mixing inserts and quries in SQLite to avoid locking the database
bootstrap
    :: TargetAddresses          -- ^ user provided target addresses
    -> IO DBQueryEnv            -- ^ returns Query runtime environment
bootstrap targetAddresses = do
    ix <- atomically (newEmptyTMVar :: STM (TMVar Utxo.UtxoIndex) )
    pure $ DBQueryEnv
        { _queryTMVar = UtxoQueryTMVar ix
        , _queryAddresses = targetAddresses
        }
-- | finds reports for all user-provided addresses.
-- TODO consider sqlite streaming, https://hackage.haskell.org/package/sqlite-simple-0.4.18.2/docs/Database-SQLite-Simple.html#g:14
--
findAll
    :: DBQueryEnv                   -- ^ Query run time environment
    -> IO [UtxoTxOutReport]         -- ^ set of corresponding TxOutRefs
findAll env = forConcurrently addresses f
    where
        addresses = NonEmpty.toList (env ^. queryAddresses)

        f  :: C.Address C.ShelleyAddr -> IO (UtxoTxOutReport)
        f addr = (findByCardanoAddress env . C.toAddressAny $ addr) >>= pure . (UtxoTxOutReport (pack . show $ addr))


-- | Query utxos by Cardano Address
--  To Cardano error may occure
findByCardanoAddress
    :: DBQueryEnv                   -- ^ Query run time environment
    -> C.AddressAny                 -- ^ Cardano address to query
    -> IO [Utxo.UtxoRow]
findByCardanoAddress  = withQueryAction

-- | Retrieve a Set of TxOutRefs associated with the given Cardano Era address
-- We return an empty Set if no address is found
findByAddress
    :: DBQueryEnv                                   -- ^ Query run time environment
    -> Text                                         -- ^ Bech32 Address
    -> IO (Either QueryExceptions UtxoTxOutReport)  -- ^ To Plutus address conversion error may occure
findByAddress env addressText =
    let
        f :: Either C.Bech32DecodeError (C.Address C.ShelleyAddr) -> IO (Either QueryExceptions UtxoTxOutReport)
        f (Right address)
            | address `elem` (env ^. queryAddresses) = -- allow for targetAddress search only
              (pure . C.toAddressAny $ address)
              >>= findByCardanoAddress env
              >>= pure . Right . UtxoTxOutReport addressText
            | otherwise = pure . Left . AddressNotInListError . QueryError $
              unpack addressText <> " not in the provided target addresses"
        f (Left e) = pure . Left $ QueryError (unpack  addressText
                     <> " generated error: "
                     <> show e)
    in
        f $ C.deserialiseFromBech32 C.AsShelleyAddress addressText

-- | Execute the query function
-- We must stop the utxo inserts before doing the query
withQueryAction
    :: DBQueryEnv                                            -- ^ Query run time environment
    -> C.AddressAny                                          -- ^ Cardano address to query
    -> IO [Utxo.UtxoRow]
withQueryAction env address =
    let
        utxoIndexer = unUtxoIndex  $ env ^. queryTMVar
        action :: Utxo.UtxoIndex -> IO [Utxo.UtxoRow]
        action ndxr = do
            mayberows <- (Utxo.queryPlusVolatile ndxr address)
            let rows =  case mayberows of
                    Nothing -> []
                    Just r  -> r
            pure rows
    in
        bracket
          (atomically $ takeTMVar  utxoIndexer)
          (atomically . (putTMVar utxoIndexer))
          action

-- | report target addresses
-- Used by JSON-RPC
reportQueryAddresses
    :: DBQueryEnv
    -> IO [(C.Address C.ShelleyAddr)]
reportQueryAddresses env
    = pure
    . NonEmpty.toList
    $ (env ^. queryAddresses )

reportQueryCardanoAddresses
    :: DBQueryEnv
    -> Text
reportQueryCardanoAddresses  = intercalate ", " . reportBech32Addresses

reportBech32Addresses
    :: DBQueryEnv
    -> [Text]
reportBech32Addresses env
    = NonEmpty.toList
    . fmap C.serialiseAddress
    $ (env ^. queryAddresses )

-- | Non-blocking write of a new value to a 'TMVar'
-- Puts if empty. Replaces if populated.
writeTMVar :: TMVar a -> a -> STM ()
writeTMVar t new = tryTakeTMVar t >> putTMVar t new
