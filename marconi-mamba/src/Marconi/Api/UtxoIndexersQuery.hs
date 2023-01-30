module Marconi.Api.UtxoIndexersQuery
    ( bootstrap
    , findByCardanoAddress
    , findByAddress
    , findAll
    , reportQueryAddresses
    , Utxo.UtxoRow(..)
    , Utxo.UtxoIndexer
    , reportQueryCardanoAddresses
    , reportBech32Addresses
    , withQueryAction
    , writeTMVar
    , writeTMVar'
    ) where
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, putTMVar, takeTMVar, tryTakeTMVar)
import Control.Exception (bracket)
import Control.Lens ((^.))
import Control.Monad.STM (STM)
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text, intercalate, pack, unpack)

import Cardano.Api qualified as C
import Marconi.Api.Types (HasUtxoIndexerEnv (uiIndexer, uiQaddresses),
                          QueryExceptions (AddressNotInListError, QueryError),
                          UtxoIndexerEnv (UtxoIndexerEnv, _uiIndexer, _uiQaddresses),
                          UtxoIndexerWrapper (UtxoIndexerWrapper, unWrap), UtxoReport (UtxoReport))
import Marconi.Index.Utxo qualified as Utxo
import Marconi.Types (TargetAddresses)
import RewindableIndex.Storable qualified as Storable

-- | Bootstraps the utxo query environment.
-- The module is responsible for accessing SQLite for quries.
-- The main issue we try to avoid here is mixing inserts and quries in SQLite to avoid locking the database
bootstrap
    :: TargetAddresses          -- ^ user provided target addresses
    -> IO UtxoIndexerEnv        -- ^ returns Query runtime environment
bootstrap targetAddresses = do
    ix <- atomically (newEmptyTMVar :: STM (TMVar Utxo.UtxoIndexer) )
    pure $ UtxoIndexerEnv
        { _uiIndexer = UtxoIndexerWrapper ix
        , _uiQaddresses = targetAddresses
        }
-- | finds reports for all user-provided addresses.
-- TODO consider sqlite streaming, https://hackage.haskell.org/package/sqlite-simple-0.4.18.2/docs/Database-SQLite-Simple.html#g:14
--
findAll
    :: UtxoIndexerEnv          -- ^ Query run time environment
    -> IO [UtxoReport]         -- ^ set of corresponding TxOutRefs
findAll env = forConcurrently addresses f
    where
        addresses = NonEmpty.toList (env ^. uiQaddresses)
        f  :: C.Address C.ShelleyAddr -> IO UtxoReport
        f addr = (findByCardanoAddress env . C.toAddressAny $ addr) <&> UtxoReport (pack . show $ addr)

-- | Query utxos by Cardano Address
--  To Cardano error may occure
findByCardanoAddress
    :: UtxoIndexerEnv               -- ^ Query run time environment
    -> C.AddressAny                 -- ^ Cardano address to query
    -> IO [Utxo.UtxoRow]
findByCardanoAddress  = withQueryAction

-- | Retrieve a Set of TxOutRefs associated with the given Cardano Era address
-- We return an empty Set if no address is found
findByAddress
    :: UtxoIndexerEnv                          -- ^ Query run time environment
    -> Text                                    -- ^ Bech32 Address
    -> IO (Either QueryExceptions UtxoReport)  -- ^ To Plutus address conversion error may occure
findByAddress env addressText =
    let
        f :: Either C.Bech32DecodeError (C.Address C.ShelleyAddr) -> IO (Either QueryExceptions UtxoReport)
        f (Right address)
            | address `elem` (env ^. uiQaddresses) = -- allow for targetAddress search only
              (pure . C.toAddressAny $ address)
              >>= findByCardanoAddress env
              <&> Right . UtxoReport addressText
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
    :: UtxoIndexerEnv       -- ^ Query run time environment
    -> C.AddressAny         -- ^ Cardano address to query
    -> IO [Utxo.UtxoRow]
withQueryAction env address =
    let
        utxoIndexer = unWrap  $ env ^. uiIndexer
        action :: Utxo.UtxoIndexer -> IO [Utxo.UtxoRow]
        action indexer = do
            Utxo.UtxoResult rows <- Storable.query Storable.QEverything indexer (Utxo.UtxoAddress address)
            pure rows
    in
        bracket
          (atomically . takeTMVar $ utxoIndexer)
          (atomically . putTMVar utxoIndexer)
          action

-- | report target addresses
-- Used by JSON-RPC
reportQueryAddresses
    :: UtxoIndexerEnv
    -> IO [C.Address C.ShelleyAddr]
reportQueryAddresses env
    = pure
    . NonEmpty.toList
    $ (env ^. uiQaddresses )

reportQueryCardanoAddresses
    :: UtxoIndexerEnv
    -> Text
reportQueryCardanoAddresses  = intercalate ", " . reportBech32Addresses

reportBech32Addresses
    :: UtxoIndexerEnv
    -> [Text]
reportBech32Addresses env
    = NonEmpty.toList
    . fmap C.serialiseAddress
    $ (env ^. uiQaddresses )

-- | Non-blocking write of a new value to a 'TMVar'
-- Puts if empty. Replaces if populated.
writeTMVar :: TMVar a -> a -> STM ()
writeTMVar t new = tryTakeTMVar t >> putTMVar t new

writeTMVar' :: UtxoIndexerWrapper-> Utxo.UtxoIndexer -> STM ()
writeTMVar' (UtxoIndexerWrapper t) =  writeTMVar t
