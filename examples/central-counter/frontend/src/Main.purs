module Main where

import Prelude
import Control.Monad.Reader.Trans
import Control.Monad.Except.Trans
import Control.Monad.Aff
import Data.Either.Unsafe
import Servant.PureScript.Affjax
import Servant.PureScript.Settings
import Counter.WebAPI
import Counter.ServerTypes
import Data.Argonaut.Aeson
import Data.Generic
import Data.Maybe
import Data.Either
import Data.Array as Array
import Servant.Subscriber as Subscriber
import CSS (purple)
import Control.Bind ((<=<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM.Node.Node (baseURI)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Foldable (foldr, fold)
import Data.List (List(Nil, Cons))
import Data.Tuple (Tuple(Tuple))
import Network.HTTP.Affjax (AJAX, get)
import Pux (renderToDOM, fromSimple, start, EffModel, noEffects)
import Pux.Html (Html, text, button, span, div, p)
import Pux.Html.Events (onClick)
import Servant.Subscriber (SubscriberEff, NotifyEvent(WebSocketClosed, WebSocketError, NotifyEvent), makeSubscriber, Subscriber)
import Servant.Subscriber.Request (HttpRequest(HttpRequest))
import Servant.Subscriber.Response (Status(Status), Response(Unsubscribed, Deleted, Modified, Subscribed, RequestError), RequestError(AlreadySubscribed, NoSuchSubscription, HttpRequestFailed, ParseError), HttpResponse(HttpResponse))
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
  , httpHeaders : [ Tuple "authtoken" "VerySecret \"topsecret\""]
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
      Left err -> return $ ReportError err
      Right v -> return v

type SubscriberData = {
  subscriber :: Subscriber
, messages :: Signal (Maybe NotifyEvent)
}


initSubscriber :: forall eff. SubscriberEff (channel :: CHANNEL | eff) SubscriberData
initSubscriber = do
  ch <- channel (Nothing :: Maybe NotifyEvent)
  sub <- makeSubscriber "ws://localhost:8081/subscriber" (send ch <<< Just)
  let sig = subscribe ch
  Subscriber.subscribe subscriberRequest sub
  return $ { subscriber : sub, messages : sig }


toAction :: Maybe NotifyEvent -> Action
toAction Nothing                 = SubscriberLog "Received Nothing"
toAction (Just (NotifyEvent ev)) = responseToAction ev
toAction (Just (Subscriber.ParseError str)) = SubscriberLog $ "ParseError: " <> str
toAction (Just WebSocketError)   = SubscriberLog $ "WebSocket error"
toAction (Just WebSocketClosed)  = SubscriberLog $ "WebSocket closed"

responseToAction :: Response -> Action
responseToAction (Subscribed (Path p)) = SubscriberLog $ "Successfully subscribed: " <> show p
responseToAction (Modified (Path ["counter"]) (HttpResponse resp)) = let
      status = case resp.httpStatus of (Status s) -> s
    in
      if status.statusCode /= 200
        then SubscriberLog $ "Received error code: " <> show status.statusCode <> ", message: " <> resp.httpBody
        else handleModify resp.httpBody
responseToAction (Modified (Path p) (HttpResponse _)) = SubscriberLog $ "Unknown path modified: " <> show p
responseToAction (Deleted (Path p)) = SubscriberLog $ "Path got deleted: " <> show p
responseToAction (Unsubscribed (Path p)) = SubscriberLog $ "Path got unsubscribed: " <> show p
responseToAction (RequestError ParseError) = SubscriberLog "Server could not parse our request"
responseToAction (RequestError (HttpRequestFailed _ _)) = SubscriberLog "Subscription failed because the http request failed."
responseToAction (RequestError (NoSuchSubscription (Path p))) = SubscriberLog $ "No such subscription: " <> show p
responseToAction (RequestError (AlreadySubscribed (Path p)))  = SubscriberLog $ "Already subscribed: " <> show p

handleModify :: String -> Action
handleModify msg = case gAesonDecodeJson <=< jsonParser $ msg of
                      Left err -> SubscriberLog $ "Could not parse response body: " <> err
                      Right v -> Update v


-- main :: forall e. Eff (ajax :: AJAX, err :: EXCEPTION, channel :: CHANNEL | e) Unit
main :: forall eff. Eff (ajax :: AJAX, err :: EXCEPTION, channel :: CHANNEL, ref :: REF, ws :: WEBSOCKET | eff) Unit
main = do
  sub <- initSubscriber
  let settings = SPSettings_ {
                    encodeJson : gAesonEncodeJson
                  , decodeJson : gAesonDecodeJson
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
