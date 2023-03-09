{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.Sidechain.Api.Query.Indexers.Utxo (tests) where

import Control.Concurrent.STM (atomically)
import Control.Lens.Operators ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Text (unpack)
import Data.Traversable (for)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.JsonRpc.Client.Types ()
import Network.JsonRpc.Types (JsonRpcResponse (Result))
import Network.Wai.Handler.Warp qualified as Warp
import Servant.API ((:<|>) ((:<|>)))
import Servant.Client (BaseUrl (BaseUrl), ClientEnv, HasClient (Client), Scheme (Http), client, hoistClient,
                       mkClientEnv, runClientM)

import Cardano.Api qualified as C
import Gen.Marconi.ChainIndex.Indexers.Utxo (genShelleyEraUtxoEvents)
import Helpers (addressAnyToShelley)
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.Core.Storable qualified as Storable
import Marconi.Sidechain.Api.HttpServer (marconiApp)
import Marconi.Sidechain.Api.Query.Indexers.Utxo qualified as AddressUtxoIndexer
import Marconi.Sidechain.Api.Query.Indexers.Utxo qualified as UIQ
import Marconi.Sidechain.Api.Routes (AddressUtxoResult (AddressUtxoResult), JsonRpcAPI)
import Marconi.Sidechain.Api.Types (SidechainEnv, sidechainAddressUtxoIndexer, sidechainEnvIndexers)
import Marconi.Sidechain.Bootstrap (initializeSidechainEnv)

import Hedgehog (Property, forAll, property, (===))
import Hedgehog qualified
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests = testGroup "marconi-sidechain-utxo query Api Specs"
    [ testPropertyNamed
        "marconi-sidechain-utxo, Insert events and query for utxo's with address in the generated ShelleyEra targetAddresses"
        "queryTargetAddressTest"
        queryTargetAddressTest

    , utxoJsonRpcTestTree
    ]

-- | JSON-RPC Testtree
-- We put all the JSON-RPC tests under this `TestTree`
utxoJsonRpcTestTree :: TestTree
utxoJsonRpcTestTree = withResource initRpcTestEnv (\_ -> pure ()) runUtxoJsonRpcTests

runUtxoJsonRpcTests
  :: RpcClientAction -- ^ the IO hoisted RPC client action that call the RPC Server
  -> TestTree
runUtxoJsonRpcTests rpcenv = testGroup "marconi-sidechain-utxo JSON-RPC test-group"
    [ testPropertyNamed
        "stores UtxoEvents, and retrieve them through the RPC server using an RPC client"
        "propUtxoEventInsertionAndJsonRpcQueryRoundTrip"
        (propUtxoEventInsertionAndJsonRpcQueryRoundTrip rpcenv)
    ]

-- | A type for Storable Action to store events
newtype RpcEnv = RpcEnv ([Utxo.StorableEvent Utxo.UtxoHandle] -> IO ()) --  callback

-- | Alias for the storable-action and RPC-client-Action pair, to simplify the type signatures
type RpcClientAction = IO ( RpcEnv, String -> IO (JsonRpcResponse String AddressUtxoResult))

mkRpcEnv :: SidechainEnv -> RpcEnv
mkRpcEnv env =
  let cb :: Utxo.UtxoIndexer -> IO ()
      cb = atomically . UIQ.updateEnvState (env ^. sidechainEnvIndexers . sidechainAddressUtxoIndexer)
  in RpcEnv (mocUtxoWorker cb)

-- | Initialize RPC server on a free available port
initRpcTestEnv :: RpcClientAction
initRpcTestEnv = do
  env <- initializeSidechainEnv Nothing Nothing
  let f = \s -> Warp.testWithApplication
                    (pure $ marconiApp env)
                    (queryUtxoFromRpcServer s)
  pure (mkRpcEnv env, f)

depth :: Utxo.Depth
depth = Utxo.Depth 200

-- | Insert events, and perform the callback
-- Note, the in-memory DB provides the isolation we need per property test as the memory cache
-- is owned and visible only o the process that opened the connection
mocUtxoWorker
  :: (AddressUtxoIndexer.UtxoIndexer -> IO ())
  -> [Utxo.StorableEvent Utxo.UtxoHandle]
  -> IO ()
mocUtxoWorker callback events  =
  Utxo.open ":memory:" depth False >>= Storable.insertMany events >>= callback

-- | generate some Utxo events, store them and fetch the Unspent Utxos, and make sure JSON conversion is idempotent

queryTargetAddressTest :: Property
queryTargetAddressTest = property $ do
  events <- forAll genShelleyEraUtxoEvents
  env <- liftIO $ initializeSidechainEnv Nothing Nothing
  let
    callback :: Utxo.UtxoIndexer -> IO ()
    callback = atomically
             . AddressUtxoIndexer.updateEnvState
                (env ^. sidechainEnvIndexers . sidechainAddressUtxoIndexer) -- update the indexer
  liftIO $ mocUtxoWorker callback events
  fetchedRows <-
    liftIO
    . fmap concat
    . traverse (AddressUtxoIndexer.findByAddress $ env ^. sidechainEnvIndexers . sidechainAddressUtxoIndexer)
    . Set.toList . Set.fromList  -- required to remove the potential duplicate addresses
    . fmap Utxo._address
    . concatMap (Set.toList . Utxo.ueUtxos)
    $ events

  let numOfFetched = length fetchedRows
  Hedgehog.classify "Retrieved Utxos are greater than or Equal to 5" $ numOfFetched >= 5
  Hedgehog.classify "Retrieved Utxos are greater than 1" $ numOfFetched > 1

  Hedgehog.assert (not . null $ fetchedRows)
  (Set.fromList . mapMaybe (Aeson.decode .  Aeson.encode ) $ fetchedRows) === Set.fromList fetchedRows

-- | Create http JSON-RPC client function to test RPC route and handlers
queryUtxoFromRpcServer
  :: String     -- ^ bech32 ShelleyEra address to fetch Utxo's for
  -> Warp.Port  -- ^ http port
  -> IO (JsonRpcResponse String AddressUtxoResult)  -- ^  response to the client RPC http call
queryUtxoFromRpcServer address port = do
  manager' <- newManager defaultManagerSettings
  let env = mkClientEnv manager' (baseUrl port)
  let (rpcEcho :<|> rpcTargets :<|> _ :<|> rpcUtxos :<|> _ :<|> _ :<|> _) = mkHoistedHttpRpcClient env
  -- test will fail if these RPC methods fail.
  _ <- rpcEcho "client calling "
  _ <- rpcTargets ""
  rpcUtxos address

-- | Test round trip Utxos thruough JSON-RPC http server
-- We compare a represenation of the generated UtxoEvents
-- with those fetched from the JSON-RPC  server. The purpose of this is:
--   + RPC server routes the request to the correct handler
--   + Events are serialized/deserialized thru the RPC layer and it various wrappers correctly
propUtxoEventInsertionAndJsonRpcQueryRoundTrip
  :: RpcClientAction
  -> Property
propUtxoEventInsertionAndJsonRpcQueryRoundTrip action = property $ do
  (RpcEnv storableAction, rpcClient) <- liftIO action
  events <- forAll genShelleyEraUtxoEvents
  liftIO $ storableAction events
  let (qAddresses :: [String]) =
        Set.toList . Set.fromList . fmap (unpack . C.serialiseAddress)
        . mapMaybe (addressAnyToShelley . Utxo._address)
        . concatMap  (Set.toList . Utxo.ueUtxos)
        $ events
  rpcResponses <- liftIO $ for qAddresses rpcClient
  let
    fetched :: [Utxo.UtxoRow] = concatMap fromQueryResult rpcResponses

  Hedgehog.assert (not . null $ fetched)
  (Set.fromList . mapMaybe (Aeson.decode .  Aeson.encode ) $ fetched) === Set.fromList fetched

hoistClientApi :: Proxy JsonRpcAPI
hoistClientApi = Proxy

-- hoist http servant client from clientM to IO
mkHoistedHttpRpcClient :: ClientEnv -> Client IO JsonRpcAPI
mkHoistedHttpRpcClient cEnv
  = hoistClient hoistClientApi
                ( fmap (either (error . show) id)
                . flip runClientM cEnv
                )
                (client hoistClientApi)

baseUrl :: Warp.Port -> BaseUrl
baseUrl port = BaseUrl Http "localhost" port ""

fromQueryResult :: JsonRpcResponse e AddressUtxoResult -> [Utxo.UtxoRow]
fromQueryResult (Result _ (AddressUtxoResult rows) ) = rows
fromQueryResult _                                    = []
