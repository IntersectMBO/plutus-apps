module AjaxUtils
  ( AjaxErrorPaneAction(..)
  , ajaxErrorPane
  , closeableAjaxErrorPane
  , ajaxErrorRefLabel
  ) where

import Prelude hiding (div)

import Bootstrap (alertDanger_, btn, floatRight)
import Data.Argonaut (Json, JsonDecodeError)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (printJsonDecodeError)
import Halogen (RefLabel(RefLabel))
import Halogen.HTML (ClassName(..), HTML, br_, button, div, p_, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes, ref)
import Icons (Icon(..), icon)
import Servant.PureScript (AjaxError, printAjaxError)

data AjaxErrorPaneAction
  = CloseErrorPane

ajaxErrorPane :: forall p i. AjaxError JsonDecodeError Json -> HTML p i
ajaxErrorPane error =
  div
    [ class_ ajaxErrorClass
    , ref ajaxErrorRefLabel
    ]
    [ alertDanger_
        [ text $ printAjaxError stringify printJsonDecodeError error
        , br_
        , text "Please try again or contact support for assistance."
        ]
    ]

closeableAjaxErrorPane
  :: forall p. AjaxError JsonDecodeError Json -> HTML p AjaxErrorPaneAction
closeableAjaxErrorPane error =
  div
    [ class_ ajaxErrorClass ]
    [ alertDanger_
        [ button
            [ classes [ btn, floatRight, ClassName "ajax-error-close-button" ]
            , onClick $ const $ CloseErrorPane
            ]
            [ icon Close ]
        , p_ [ text $ printAjaxError stringify printJsonDecodeError error ]
        ]
    ]

ajaxErrorRefLabel :: RefLabel
ajaxErrorRefLabel = RefLabel "ajax-error"

ajaxErrorClass :: ClassName
ajaxErrorClass = ClassName "ajax-error"
