{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
module Main
    ( main
    ) where

import Control.Monad (forM, void)
import Control.Monad.Freer (interpret)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, Result (..), ToJSON, encode, fromJSON)
import Data.Default (Default (def))
import Data.Map.Strict qualified as Map
import Data.Monoid qualified as Monoid
import Data.OpenApi.Schema qualified as OpenApi
import Data.Semigroup qualified as Semigroup
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger.Ada (adaSymbol, adaToken)
import Plutus.Contract
import Plutus.Contracts.Currency qualified as Currency
import Plutus.Contracts.Uniswap qualified as Uniswap
import Plutus.Contracts.Uniswap.Trace as US
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Simulator (SimulatorEffectHandlers, logString)
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.PAB.Webserver.Server qualified as PAB.Server
import Prelude hiding (init)
import Prettyprinter (Pretty (..), viaShow)
import Wallet.Emulator.Types (knownWallet)

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    logString @(Builtin UniswapContracts) "Starting Uniswap PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidInit  <- Simulator.activateContract (knownWallet 1) Init
    cs       <- flip Simulator.waitForState cidInit $ \json -> case fromJSON json of
                    Success (Just (Semigroup.Last cur)) -> Just $ Currency.currencySymbol cur
                    _                                   -> Nothing
    _        <- Simulator.waitUntilFinished cidInit

    logString @(Builtin UniswapContracts) $ "Initialization finished. Minted: " ++ show cs

    let coins = Map.fromList [(tn, Uniswap.mkCoin cs tn) | tn <- tokenNames]
        ada   = Uniswap.mkCoin adaSymbol adaToken

    cidStart <- Simulator.activateContract (knownWallet 1) UniswapStart
    us       <- flip Simulator.waitForState cidStart $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Uniswap.Uniswap))) of
                    Success (Monoid.Last (Just (Right us))) -> Just us
                    _                                       -> Nothing
    logString @(Builtin UniswapContracts) $ "Uniswap instance created: " ++ show us

    cids <- fmap Map.fromList $ forM US.wallets $ \w -> do
        cid <- Simulator.activateContract w $ UniswapUser us
        logString @(Builtin UniswapContracts) $ "Uniswap user contract started for " ++ show w
        Simulator.waitForEndpoint cid "funds"
        _ <- Simulator.callEndpointOnInstance cid "funds" ()
        v <- flip Simulator.waitForState cid $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Uniswap.UserContractState))) of
                Success (Monoid.Last (Just (Right (Uniswap.Funds v)))) -> Just v
                _                                                      -> Nothing
        logString @(Builtin UniswapContracts) $ "initial funds in wallet " ++ show w ++ ": " ++ show v
        return (w, cid)

    let cp = Uniswap.CreateParams ada (coins Map.! "A") 100000 500000
    logString @(Builtin UniswapContracts) $ "creating liquidity pool: " ++ show (encode cp)
    let cid2 = cids Map.! knownWallet 2
    Simulator.waitForEndpoint cid2 "create"
    _  <- Simulator.callEndpointOnInstance cid2 "create" cp
    flip Simulator.waitForState (cids Map.! knownWallet 2) $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Uniswap.UserContractState))) of
        Success (Monoid.Last (Just (Right Uniswap.Created))) -> Just ()
        _                                                    -> Nothing
    logString @(Builtin UniswapContracts) "liquidity pool created"

    _ <- liftIO getLine
    shutdown

data UniswapContracts =
      Init
    | UniswapStart
    | UniswapUser Uniswap.Uniswap
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty UniswapContracts where
    pretty = viaShow

instance HasDefinitions UniswapContracts where
    getDefinitions = [Init, UniswapStart]
    getSchema = \case
        UniswapUser _ -> Builtin.endpointsToSchemas @Uniswap.UniswapUserSchema
        UniswapStart  -> Builtin.endpointsToSchemas @Uniswap.UniswapOwnerSchema
        Init          -> Builtin.endpointsToSchemas @Empty
    getContract = \case
        UniswapUser us -> SomeBuiltin . awaitPromise $ Uniswap.userEndpoints us
        UniswapStart   -> SomeBuiltin Uniswap.ownerEndpoint
        Init           -> SomeBuiltin US.setupTokens

handlers :: SimulatorEffectHandlers (Builtin UniswapContracts)
handlers =
    Simulator.mkSimulatorHandlers def
    $ interpret (contractHandler (Builtin.handleBuiltin @UniswapContracts))
