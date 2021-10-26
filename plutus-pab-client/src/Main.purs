module Main where

import Prologue
import Effect (Effect)
import Effect.Aff (forkAff, launchAff_)
import Effect.Class (liftEffect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import MainFrame (initialMainFrame)
import Plutus.PAB.Webserver.Types (CombinedWSStreamToClient, CombinedWSStreamToServer)
import Types (HAction(..), Query(..), Output(..))
import WebSocket.Support (WebSocketManager, mkWebSocketManager)
import WebSocket.Support as WS

main :: Effect Unit
main = do
  runHalogenAff do
    body <- awaitBody
    driver <- runUI initialMainFrame Init body
    --
    wsManager :: WebSocketManager CombinedWSStreamToClient CombinedWSStreamToServer <-
      mkWebSocketManager
    void
      $ forkAff
      $ WS.runWebSocketManager
          (WS.URI "/ws")
          (\msg -> void $ driver.query $ ReceiveWebSocketMessage msg unit)
          wsManager
    void
      $ liftEffect
      $ HS.subscribe driver.messages
      $ case _ of
          (SendWebSocketMessage msg) -> do
            launchAff_ $ WS.managerWriteOutbound wsManager $ WS.SendMessage msg
