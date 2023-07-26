{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Cardano.Node.Socket.Emulator
    ( main
    ) where

import Cardano.BM.Data.Trace (Trace)
import Cardano.Node.Emulator.Internal.Node (Params (..), SlotConfig (SlotConfig, scSlotLength, scSlotZeroTime))
import Cardano.Node.Socket.Emulator.API (API)
import Cardano.Node.Socket.Emulator.Mock (consumeEventHistory, healthcheck, slotCoordinator)
import Cardano.Node.Socket.Emulator.Params qualified as Params
import Cardano.Node.Socket.Emulator.Server qualified as Server
import Cardano.Node.Socket.Emulator.Types (AppState (..), CNSEServerLogMsg (..), NodeServerConfig (..),
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
import Ledger.CardanoWallet (knownAddresses)
import Ledger.Value.CardanoAPI qualified as CardanoAPI
import Network.Wai.Handler.Warp qualified as Warp
import Plutus.Monitoring.Util qualified as LM
import Servant (Application, hoistServer, serve, (:<|>) ((:<|>)))
import Servant.Client (BaseUrl (baseUrlPort))

app ::
    Trace IO CNSEServerLogMsg
 -> Params
 -> MVar AppState
 -> Application
app trace params stateVar =
    serve (Proxy @API) $
    hoistServer
        (Proxy @API)
        (liftIO . Server.processChainEffects (LM.convertLog ProcessingEmulatorMsg trace) params stateVar)
        (healthcheck :<|> consumeEventHistory stateVar)

data Ctx = Ctx { serverHandler :: Server.ServerHandler
               , serverState   :: MVar AppState
               , mockTrace     :: Trace IO CNSEServerLogMsg
               }

main :: Trace IO CNSEServerLogMsg -> NodeServerConfig -> IO () -> IO ()
main trace nodeServerConfig@NodeServerConfig { nscBaseUrl
                            , nscSlotConfig
                            , nscKeptBlocks
                            , nscInitialTxWallets
                            , nscSocketPath } whenStarted = LM.runLogEffects trace $ do

    -- make initial distribution of 1 billion Ada to all configured wallets
    let getAddress n = knownAddresses !! (fromIntegral n - 1)
        dist = Map.fromList $ zip (getAddress <$> nscInitialTxWallets) (repeat (CardanoAPI.adaValueOf 1_000_000_000))
    initialState <- initialChainState dist
    let appState = AppState initialState mempty
    params <- liftIO $ Params.fromNodeServerConfig nodeServerConfig
    serverHandler <- liftIO $ Server.runServerNode (LM.convertLog ProcessingEmulatorMsg trace) nscSocketPath nscKeptBlocks appState params
    serverState   <- liftIO $ newMVar appState
    handleDelayEffect $ delayThread (2 :: Second)

    let ctx = Ctx { serverHandler = serverHandler
                  , serverState   = serverState
                  , mockTrace     = trace
                  }

    runSlotCoordinator ctx

    logInfo $ StartingCNSEServer $ baseUrlPort nscBaseUrl
    liftIO $ Warp.runSettings warpSettings $ app trace params serverState

        where
            warpSettings = Warp.defaultSettings & Warp.setPort (baseUrlPort nscBaseUrl) & Warp.setBeforeMainLoop whenStarted

            runSlotCoordinator (Ctx serverHandler _ _)  = do
                let SlotConfig{scSlotZeroTime, scSlotLength} = nscSlotConfig
                logInfo $ StartingSlotCoordination (posixSecondsToUTCTime $ realToFrac scSlotZeroTime / 1000)
                                                   (fromInteger scSlotLength :: Millisecond)
                void $ liftIO $ forkIO $ slotCoordinator nscSlotConfig serverHandler
