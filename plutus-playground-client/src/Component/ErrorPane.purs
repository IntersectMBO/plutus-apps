module Component.ErrorPane where

import Prelude hiding (div)

import Bootstrap (alertDanger_, btn, floatRight)
import Halogen (RefLabel(RefLabel))
import Halogen.HTML (ClassName(..), HTML, br_, button, div, p_, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes, ref)
import Icons (Icon(..), icon)

data ErrorPaneAction = CloseErrorPane

errorPane :: forall p i. String -> HTML p i
errorPane error =
  div
    [ class_ errorClass
    , ref errorRefLabel
    ]
    [ alertDanger_
        [ text error
        , br_
        , text "Please try again or contact support for assistance."
        ]
    ]

closeableErrorPane
  :: forall p. String -> HTML p ErrorPaneAction
closeableErrorPane error =
  div
    [ class_ errorClass ]
    [ alertDanger_
        [ button
            [ classes [ btn, floatRight, ClassName "ajax-error-close-button" ]
            , onClick $ const $ CloseErrorPane
            ]
            [ icon Close ]
        , p_ [ text error ]
        ]
    ]

errorRefLabel :: RefLabel
errorRefLabel = RefLabel "ajax-error"

errorClass :: ClassName
errorClass = ClassName "ajax-error"
