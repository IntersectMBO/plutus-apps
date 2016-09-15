-- | TODO: This example could use a rewrite ;-)
module Main where

import Control.Monad.Aff
import Control.Monad.Except.Trans
import Control.Monad.Reader.Trans
import Counter.ServerTypes
import Counter.WebAPI
import Data.Argonaut.Generic.Aeson
import Data.Either
import Data.Generic
import Data.Maybe
import Prelude
import Servant.PureScript.Affjax
import Servant.PureScript.Settings
import Counter.WebAPI.MakeRequests as MakeReq
import Counter.WebAPI.Subscriber as Sub
import Data.Array as Array
import Servant.Subscriber as Subscriber
import Servant.Subscriber.Connection as C
import Control.Bind ((<=<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM.Node.Node (baseURI)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Foldable (foldr, fold)
import Data.List (List(Nil, Cons))
import Data.Tuple (Tuple(Tuple))
import Network.HTTP.Affjax (AJAX, get)
import Pux (renderToDOM, fromSimple, start, EffModel, noEffects)
import Pux.Html (Html, text, button, span, div, p)
import Pux.Html.Events (onClick)
import Servant.Subscriber (Subscriber, makeSubscriber, SubscriberEff, Config, makeSubscriptions)
import Servant.Subscriber.Request (HttpRequest(..))
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
            | Nop

type State = {
    counter :: Int
  , lastError :: Maybe AjaxError
  , settings :: MySettings
  , subscriberLog :: List String
  }

type MySettings = SPSettings_ SPParams_



type APIEffect eff = ReaderT MySettings (ExceptT AjaxError (Aff ( ajax :: AJAX, channel :: CHANNEL, err :: EXCEPTION  | eff)))

type ServantModel =
    { state :: State
    , effects :: Array (APIEffect () Action)
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
        [ span [] [ text $ "Error: " <> maybe "Nothing" errorToString state.lastError ]
        , div  []
            [ text $ "Subscriber Log: "
            , div []
                (foldr Array.cons [] <<< map (\l -> p [] [ text l ]) $ state.subscriberLog)
            ]
        ]
    ]

runEffectActions :: State -> Array (APIEffect () Action) -> EffModel State Action (ajax :: AJAX)
runEffectActions state effects = { state : state, effects : map (runEffect state.settings) effects }

runEffect :: MySettings -> APIEffect () Action -> Aff (channel :: CHANNEL, ajax :: AJAX, err :: EXCEPTION) Action
runEffect settings m = do
    er <- runExceptT $ runReaderT m settings
    case er of
      Left err -> pure $ ReportError err
      Right v -> pure v

type SubscriberData eff = {
  subscriber :: Subscriber eff Action
, messages :: Signal Action
}


initSubscriber :: forall eff. MySettings -> SubscriberEff (channel :: CHANNEL | eff) (SubscriberData (channel :: CHANNEL | eff))
initSubscriber settings = do
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
  pongReq <- flip runReaderT settings $ MakeReq.putCounter (CounterAdd 1) -- | Let's play a bit! :-)
  closeReq <- flip runReaderT settings $ MakeReq.putCounter (CounterSet 100)
  subs <- flip runReaderT settings $ Sub.getCounter (maybe Nop Update)
  let c = Subscriber.getConnection sub
  C.setPongRequest pongReq c -- |< Hihi :-)
  C.setCloseRequest closeReq c
  Subscriber.deploy subs sub
  pure $ { subscriber : sub, messages : sig }


-- main :: forall e. Eff (ajax :: AJAX, err :: EXCEPTION, channel :: CHANNEL | e) Unit
main :: forall eff. Eff (ajax :: AJAX, err :: EXCEPTION, channel :: CHANNEL, ref :: REF, ws :: WEBSOCKET | eff) Unit
main = do
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
  sub <- initSubscriber settings
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
