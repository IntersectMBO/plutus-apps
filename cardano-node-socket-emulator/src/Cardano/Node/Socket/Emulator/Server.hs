{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Cardano.Node.Socket.Emulator.Server
    ( main
    ) where

import Cardano.BM.Data.Trace (Trace)
import Cardano.Node.Emulator.Internal.Node (Params (..), SlotConfig (SlotConfig, scSlotLength, scSlotZeroTime))
import Cardano.Node.Socket.Emulator qualified as Server
import Cardano.Node.Socket.Emulator.API (API)
import Cardano.Node.Socket.Emulator.Mock (consumeEventHistory, healthcheck, processChainEffects, slotCoordinator)
import Cardano.Node.Socket.Emulator.Params qualified as Params
import Cardano.Node.Socket.Emulator.Types (AppState (..), NodeServerConfig (..), PABServerLogMsg (..),
                                           initialChainState)
import Control.Concurrent (MVar, forkIO, newMVar)
import Control.Monad (void)
import Control.Monad.Freer.Extras.Delay (delayThread, handleDelayEffect)
import Control.Monad.Freer.Extras.Log (logInfo)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Units (Millisecond, Second)
import Ledger.Value.CardanoAPI qualified as CardanoAPI
import Network.Wai.Handler.Warp qualified as Warp
import Plutus.Monitoring.Util qualified as LM
import Servant (Application, hoistServer, serve, (:<|>) ((:<|>)))
import Servant.Client (BaseUrl (baseUrlPort))
import Wallet.Emulator.Wallet (fromWalletNumber)

app ::
    Trace IO PABServerLogMsg
 -> Params
 -> MVar AppState
 -> Application
app trace params stateVar =
    serve (Proxy @API) $
    hoistServer
        (Proxy @API)
        (liftIO . processChainEffects (LM.convertLog ProcessingChainEvent trace) params stateVar)
        (healthcheck :<|> (fmap (fmap ProcessingChainEvent) <$> consumeEventHistory stateVar))

data Ctx = Ctx { serverHandler :: Server.ServerHandler
               , serverState   :: MVar AppState
               , mockTrace     :: Trace IO PABServerLogMsg
               }

main :: Trace IO PABServerLogMsg -> NodeServerConfig -> IO () -> IO ()
main trace nodeServerConfig@NodeServerConfig { nscBaseUrl
                            , nscSlotConfig
                            , nscKeptBlocks
                            , nscInitialTxWallets
                            , nscSocketPath } whenStarted = LM.runLogEffects trace $ do

    -- make initial distribution of 1 billion Ada to all configured wallets
    let dist = Map.fromList $ zip (fromWalletNumber <$> nscInitialTxWallets) (repeat (CardanoAPI.adaValueOf 1_000_000_000))
    initialState <- initialChainState dist
    let appState = AppState
            { _chainState = initialState
            , _eventHistory = mempty
            }
    params <- liftIO $ Params.fromNodeServerConfig nodeServerConfig
    serverHandler <- liftIO $ Server.runServerNode (LM.convertLog ProcessingChainEvent trace) nscSocketPath nscKeptBlocks (_chainState appState) params
    serverState   <- liftIO $ newMVar appState
    handleDelayEffect $ delayThread (2 :: Second)

    let ctx = Ctx { serverHandler = serverHandler
                  , serverState   = serverState
                  , mockTrace     = trace
                  }

    runSlotCoordinator ctx

    logInfo $ StartingPABServer $ baseUrlPort nscBaseUrl
    liftIO $ Warp.runSettings warpSettings $ app trace params serverState

        where
            warpSettings = Warp.defaultSettings & Warp.setPort (baseUrlPort nscBaseUrl) & Warp.setBeforeMainLoop whenStarted

            runSlotCoordinator (Ctx serverHandler _ _)  = do
                let SlotConfig{scSlotZeroTime, scSlotLength} = nscSlotConfig
                logInfo $ StartingSlotCoordination (posixSecondsToUTCTime $ realToFrac scSlotZeroTime / 1000)
                                                   (fromInteger scSlotLength :: Millisecond)
                void $ liftIO $ forkIO $ slotCoordinator nscSlotConfig serverHandler
