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
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM.Node.Node (baseURI)
import Network.HTTP.Affjax (AJAX, get)
import Pux (renderToDOM, fromSimple, start, EffModel, noEffects)
import Pux.Html (Html, text, button, span, div)
import Pux.Html.Events (onClick)
import Signal.Channel (CHANNEL)
import Data.Argonaut.Aeson
import Data.Generic



data Action = Increment | Decrement | Update Int

type State = Int

type MySettings = SPSettings_ SPParams_



type APIEffect eff = ReaderT MySettings (ExceptT AjaxError (Aff ( ajax :: AJAX, channel :: CHANNEL | eff)))

type ServantModel eff =
    { state :: State
    , effects :: Array (APIEffect eff Action)
    }

update :: forall eff. Action -> State -> ServantModel eff
update Increment count = { state : count,  effects : [
      Update <$> putCounter (CounterAdd 1)
    ]
  }
update Decrement count = { state : count, effects : [
      Update <$> putCounter (CounterAdd (-1))
    ]
  }
update (Update val) _  = { state : val, effects : []}

view :: State -> Html Action
view count =
  div
    []
    [ button [ onClick (const Increment) ] [ text "+" ]
    , span [] [ text (show count) ]
    , button [ onClick (const Decrement) ] [ text "-" ]
    ]

runUpdate :: MySettings -> ServantModel () -> EffModel State Action (ajax :: AJAX)
runUpdate settings m = {
      state   : m.state
    , effects : map runStack m.effects
    }
  where
    runStack :: APIEffect () Action -> Aff (ajax :: AJAX, channel :: CHANNEL) Action
    runStack = map fromRight <<< runExceptT <<< flip runReaderT settings

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
  app <- start
    { initialState: 0
    , update: \action state -> runUpdate settings (update action state)
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html
