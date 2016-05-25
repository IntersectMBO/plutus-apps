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
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM.Node.Node (baseURI)
import Data.Bifunctor (lmap)
import Network.HTTP.Affjax (AJAX, get)
import Pux (renderToDOM, fromSimple, start, EffModel, noEffects)
import Pux.Html (Html, text, button, span, div)
import Pux.Html.Events (onClick)
import Signal.Channel (CHANNEL)
import Data.Maybe
import Data.Either



data Action = Increment
            | Decrement
            | Update Int
            | ReportError AjaxError

type State = {
    counter :: Int
  , lastError :: Maybe AjaxError
  , settings :: MySettings
  }

type MySettings = SPSettings_ SPParams_



type APIEffect eff = ReaderT MySettings (ExceptT AjaxError (Aff ( ajax :: AJAX, channel :: CHANNEL | eff)))

type ServantModel =
    { state :: State
    , effects :: Array (APIEffect () Action)
    }

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update Increment state = runEffectActions state [Update <$> putCounter (CounterAdd 1)]
update Decrement state = runEffectActions state [Update <$> putCounter (CounterAdd (-1))]
update (Update val) state  = { state : state { counter = val }, effects : []}
update (ReportError err ) state = { state : state { lastError = Just err}, effects : []}

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

-- main :: forall e. Eff (ajax :: AJAX, err :: EXCEPTION, channel :: CHANNEL | e) Unit
main = do
  let settings = SPSettings_ {
                    encodeJson : gAesonEncodeJson
                  , decodeJson : gAesonDecodeJson
                  , toURLPiece : gShow
                  , params : SPParams_ {
                      authToken : VerySecret "topsecret"
                    , baseURL : "http://localhost:8081/"
                    }
                  }
  let initState = { counter : 0, settings : settings, lastError : Nothing }
  app <- start
    { initialState: initState
    , update: update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html
