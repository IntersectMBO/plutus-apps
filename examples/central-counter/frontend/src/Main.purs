module Main where

import Control.Bind ((<=<))
import Control.Monad.Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Except.Trans
import Control.Monad.Reader.Trans
import Counter.ServerTypes
import Counter.WebAPI
import DOM.Node.Node (baseURI)
import Data.Argonaut.Generic.Aeson
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either
import Data.Foldable (foldr, fold)
import Data.Generic
import Data.List (List(Nil, Cons))
import Data.Maybe
import Data.Tuple (Tuple(Tuple))
import Network.HTTP.Affjax (AJAX, get)
import Prelude
import Pux (renderToDOM, fromSimple, start, EffModel, noEffects)
import Pux.Html (Html, text, button, span, div, p)
import Pux.Html.Events (onClick)
import Servant.PureScript.Affjax
import Servant.PureScript.Settings
import Servant.Subscriber (SubscriberEff, Notification(WebSocketClosed, WebSocketError, Modified), makeSubscriber, Subscriber, Config)
import Servant.Subscriber.Request (HttpRequest(..))
import Servant.Subscriber as Subscriber
import Servant.Subscriber.Types (Path(Path))
import Signal (Signal)
import Signal.Channel (Channel, subscribe, send, channel, CHANNEL)
import Unsafe.Coerce (unsafeCoerce)
import WebSocket (WEBSOCKET)



data Action = Increment
            | Decrement
            | Update Int
            | ReportError AjaxError
            | SubscriberLog String

type State = {
    counter :: Int
  , lastError :: Maybe AjaxError
  , settings :: MySettings
  , subscriberLog :: List String
  }

type MySettings = SPSettings_ SPParams_



type APIEffect eff = ReaderT MySettings (ExceptT AjaxError (Aff ( ajax :: AJAX, channel :: CHANNEL | eff)))

type ServantModel =
    { state :: State
    , effects :: Array (APIEffect () Action)
    }

subscriberRequest :: HttpRequest
subscriberRequest = HttpRequest {
    httpMethod : "GET"
  , httpPath : Path ["counter"]
  , httpHeaders : [ Tuple "authtoken" "\"topsecret\""]
  , httpQuery : []
  , httpBody : ""
  }

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update Increment state = runEffectActions state [Update <$> putCounter (CounterAdd 1)]
update Decrement state = runEffectActions state [Update <$> putCounter (CounterAdd (-1))]
update (Update val) state  = { state : state { counter = val }, effects : []}
update (ReportError err ) state = { state : state { lastError = Just err}, effects : []}
update (SubscriberLog msg) state = { state : state { subscriberLog = Cons msg state.subscriberLog}, effects : []}

view :: State -> Html Action
view state =
  div []
    [ div
        []
        [ button [ onClick (const Increment) ] [ text "+" ]
        , span [] [ text (show state.counter) ]
        , button [ onClick (const Decrement) ] [ text "-" ]
        ]
    , div []
        [ span [] [ text $ "Error: " <> show state.lastError ]
        , div  []
            [ text $ "Subscriber Log: "
            , div []
                (foldr Array.cons [] <<< map (\l -> p [] [ text l ]) $ state.subscriberLog)
            ]
        ]
    ]

runEffectActions :: State -> Array (APIEffect () Action) -> EffModel State Action (ajax :: AJAX)
runEffectActions state effects = { state : state, effects : map (runEffect state.settings) effects }

runEffect :: MySettings -> APIEffect () Action -> Aff (channel :: CHANNEL, ajax :: AJAX) Action
runEffect settings m = do
    er <- runExceptT $ runReaderT m settings
    case er of
      Left err -> pure $ ReportError err
      Right v -> pure v

type SubscriberData = {
  subscriber :: Subscriber
, messages :: Signal (Maybe Notification)
}


initSubscriber :: forall eff. SubscriberEff (channel :: CHANNEL | eff) SubscriberData
initSubscriber = do
  ch <- coerceEffects $ channel (Nothing :: Maybe Notification)
  let
    c :: Config
    c = {
        url : "ws://localhost:8081/subscriber"
      , notify : coerceEffects <<< send ch <<< Just
      }
  sub <- makeSubscriber c
  let sig = subscribe ch
  Subscriber.subscribe subscriberRequest sub
  pure $ { subscriber : sub, messages : sig }


toAction :: Maybe Notification -> Action
toAction Nothing                 = SubscriberLog "Received Nothing"
toAction (Just notification)     = case notification of
  Modified path val -> handleModify val
  _ -> SubscriberLog $ gShow notification

handleModify :: String -> Action
handleModify msg = case decodeJson <=< jsonParser $ msg of
                      Left err -> SubscriberLog $ "Could not parse response body: " <> err
                      Right v -> Update v


-- main :: forall e. Eff (ajax :: AJAX, err :: EXCEPTION, channel :: CHANNEL | e) Unit
main :: forall eff. Eff (ajax :: AJAX, err :: EXCEPTION, channel :: CHANNEL, ref :: REF, ws :: WEBSOCKET | eff) Unit
main = do
  sub <- initSubscriber
  let settings = SPSettings_ {
                    encodeJson : encodeJson
                  , decodeJson : decodeJson
                  , toURLPiece : gShow
                  , params : SPParams_ {
                      authToken : VerySecret "topsecret"
                    , baseURL : "http://localhost:8081/"
                    }
                  }
  let initState = { counter : 0, settings : settings, lastError : Nothing, subscriberLog : Nil }
  app <- coerceEffects <<< start $
    { initialState: initState
    , update: update
    , view: view
    , inputs: [map toAction sub.messages]
    --, inputs : []
    }

  renderToDOM "#app" app.html

coerceEffects :: forall eff0 eff1 a. Eff eff0 a -> Eff eff1 a
coerceEffects = unsafeCoerce
