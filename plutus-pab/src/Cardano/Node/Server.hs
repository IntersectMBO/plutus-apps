{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Cardano.Node.Server
    ( main
    ) where

import Cardano.BM.Data.Trace (Trace)
import Cardano.Node.API (API)
import Cardano.Node.Mock
import Cardano.Node.Params qualified as Params
import Cardano.Node.Types
import Cardano.Protocol.Socket.Mock.Client qualified as Client
import Cardano.Protocol.Socket.Mock.Server qualified as Server
import Control.Concurrent (MVar, forkIO, newMVar)
import Control.Concurrent.Availability (Availability, available)
import Control.Monad (void)
import Control.Monad.Freer.Delay (delayThread, handleDelayEffect)
import Control.Monad.Freer.Extras.Log (logInfo)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Units (Millisecond, Second)
import Ledger.Ada qualified as Ada
import Ledger.Params (Params (..))
import Ledger.TimeSlot (SlotConfig (SlotConfig, scSlotLength, scSlotZeroTime))
import Network.Wai.Handler.Warp qualified as Warp
import Plutus.PAB.Arbitrary ()
import Plutus.PAB.Monitoring.Monitoring qualified as LM
import Servant (Application, hoistServer, serve, (:<|>) ((:<|>)))
import Servant.Client (BaseUrl (baseUrlPort))
import Wallet.Emulator.Wallet (fromWalletNumber)

app ::
    Trace IO PABServerLogMsg
 -> Params
 -> Client.TxSendHandle
 -> MVar AppState
 -> Application
app trace params clientHandler stateVar =
    serve (Proxy @API) $
    hoistServer
        (Proxy @API)
        (liftIO . processChainEffects trace params (Just clientHandler) stateVar)
        (healthcheck :<|> consumeEventHistory stateVar)

data Ctx = Ctx { serverHandler :: Server.ServerHandler
               , txSendHandle  :: Client.TxSendHandle
               , serverState   :: MVar AppState
               , mockTrace     :: Trace IO PABServerLogMsg
               }

main :: Trace IO PABServerLogMsg -> PABServerConfig -> Availability -> IO ()
main trace nodeServerConfig@PABServerConfig { pscBaseUrl
                            , pscSlotConfig
                            , pscKeptBlocks
                            , pscInitialTxWallets
                            , pscSocketPath } availability = LM.runLogEffects trace $ do

    -- make initial distribution of 1 billion Ada to all configured wallets
    let dist = Map.fromList $ zip (fromWalletNumber <$> pscInitialTxWallets) (repeat (Ada.adaValueOf 1000_000_000))
    initialState <- initialChainState dist
    let appState = AppState
            { _chainState = initialState
            , _eventHistory = mempty
            }
    params <- liftIO $ Params.fromPABServerConfig nodeServerConfig
    serverHandler <- liftIO $ Server.runServerNode trace pscSocketPath pscKeptBlocks (_chainState appState) params
    serverState   <- liftIO $ newMVar appState
    handleDelayEffect $ delayThread (2 :: Second)
    clientHandler <- liftIO $ Client.runTxSender pscSocketPath

    let ctx = Ctx { serverHandler = serverHandler
                  , txSendHandle  = clientHandler
                  , serverState   = serverState
                  , mockTrace     = trace
                  }

    runSlotCoordinator ctx

    logInfo $ StartingPABServer $ baseUrlPort pscBaseUrl
    liftIO $ Warp.runSettings warpSettings $ app trace params clientHandler serverState

        where
            warpSettings = Warp.defaultSettings & Warp.setPort (baseUrlPort pscBaseUrl) & Warp.setBeforeMainLoop (available availability)

            runSlotCoordinator (Ctx serverHandler _ _ _)  = do
                let SlotConfig{scSlotZeroTime, scSlotLength} = pscSlotConfig
                logInfo $ StartingSlotCoordination (posixSecondsToUTCTime $ realToFrac scSlotZeroTime / 1000)
                                                   (fromInteger scSlotLength :: Millisecond)
                void $ liftIO $ forkIO $ slotCoordinator pscSlotConfig serverHandler
