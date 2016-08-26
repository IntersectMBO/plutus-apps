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
import Data.Argonaut.Core (Json)
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
import Servant.Subscriber.Request (HttpRequest(..))
import Servant.Subscriber.Types (Path(Path))
import Signal (Signal)
import Signal.Channel (Channel, subscribe, send, channel, CHANNEL)
import Unsafe.Coerce (unsafeCoerce)
import WebSocket (WEBSOCKET)
import Servant.Subscriber (Subscriber, makeSubscriber, SubscriberEff, Config, makeSubscriptions)
import Servant.Subscriber as Subscriber



data Action = Increment
            | Decrement
            | Update Int
            | ReportError AjaxError
            | SubscriberLog String
            | Nop

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
update Nop state = noEffects state

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

type SubscriberData eff = {
  subscriber :: Subscriber eff Action
, messages :: Signal Action
}


initSubscriber :: forall eff. SubscriberEff (channel :: CHANNEL | eff) (SubscriberData (channel :: CHANNEL | eff))
initSubscriber = do
  ch <- channel Nop
  let
    c :: Config (channel :: CHANNEL | eff) Action
    c = {
        url : "ws://localhost:8081/subscriber"
      , notify : send ch <<< SubscriberLog <<< gShow
      , callback : send ch
      }
  sub <- makeSubscriber c
  let sig = subscribe ch
  let subs = makeSubscriptions subscriberRequest handleResponse
  Subscriber.deploy subs sub
  pure $ { subscriber : sub, messages : sig }


-- toAction :: Maybe Notification -> Action
-- toAction Nothing                 = SubscriberLog "Received Nothing"
-- toAction (Just notification)     = case notification of
--   Modified path val -> handleModify val
--   _ -> SubscriberLog $ gShow notification

handleResponse :: Maybe Json -> Either String Action
handleResponse Nothing = pure Nop
handleResponse (Just v) = map Update <<< decodeJson $ v


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
    , inputs: [sub.messages]
    --, inputs : []
    }

  renderToDOM "#app" app.html

coerceEffects :: forall eff0 eff1 a. Eff eff0 a -> Eff eff1 a
coerceEffects = unsafeCoerce
