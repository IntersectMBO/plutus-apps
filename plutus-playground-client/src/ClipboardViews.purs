module ClipboardViews
  ( clipboardButton
  , showShortCopyLong
  ) where

import Prologue hiding (div)

import Bootstrap (btn, btnLink, displayFlex, textTruncate, alignItemsCenter)
import Clipboard (Action(..))
import Data.Maybe (fromMaybe)
import Halogen.HTML (ClassName(..), HTML, button, div, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes)
import Icons (Icon(..), icon)

clipboardButton :: forall p. String -> HTML p Action
clipboardButton str =
  div
    [ class_ $ ClassName "clipboard" ]
    [ button
        [ classes [ btn, btnLink ]
        , onClick $ const $ CopyToClipboard str
        ]
        [ icon Clipboard ]
    ]

showShortCopyLong
  :: forall p. String -> Maybe (Array (HTML p Action)) -> HTML p Action
showShortCopyLong str content =
  div [ classes [ displayFlex, alignItemsCenter ] ]
    [ div [ classes [ textTruncate ] ]
        (fromMaybe [ text str ] content)
    , clipboardButton str
    ]
