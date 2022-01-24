module Gists.View
  ( gistControls
  , idPublishGist
  , idLoadGist
  ) where

import Prologue hiding (div)

import Auth (AuthRole(..), AuthStatus, authStatusAuthRole)
import Bootstrap (btn, btnDanger, btnSecondary, btnSmall, empty, formControl, formGroup, isInvalid, isValid, nbsp)
import Component.ErrorPane (closeableErrorPane)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Lens (view)
import Data.Maybe (fromMaybe)
import Gist (Gist, GistId, gistHtmlUrl)
import Gists.Types (GistAction(..), parseGistUrl)
import Halogen.HTML (ClassName(ClassName), HTML, IProp, a, button, div, input, label, text)
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties (class_, classes, disabled, for, href, id, target, type_, value)
import Icons (Icon(..), icon)
import MainFrame.Types (WebData)
import Network.RemoteData (RemoteData(NotAsked, Loading, Failure, Success))

idPublishGist :: forall r i. IProp (id :: String | r) i
idPublishGist = id "publish-gist"

idLoadGist :: forall r i. IProp (id :: String | r) i
idLoadGist = id "load-gist"

gistControls
  :: forall a p
   . { authStatus :: WebData AuthStatus
     , createGistResult :: WebData Gist
     , gistErrorPaneVisible :: Boolean
     , gistUrl :: Maybe String
     | a
     }
  -> HTML p GistAction
gistControls { authStatus, createGistResult, gistErrorPaneVisible, gistUrl } =
  authButton
    $ div
        [ class_ $ ClassName "gist-controls" ]
        [ div
            [ class_ $ ClassName "form-inline" ]
            [ div
                [ class_ formGroup ]
                [ label
                    [ for gistIdInputId ]
                    [ text "Gist ID" ]
                , input
                    [ type_ InputText
                    , value $ fromMaybe "" $ gistUrl
                    , id gistIdInputId
                    , classes
                        ( [ formControl, ClassName "form-control-sm" ]
                            <> case parsedGistId of
                              Just (Left _) -> [ isInvalid ]
                              Just (Right _) -> [ isValid ]
                              Nothing -> []
                        )
                    , onValueInput SetGistUrl
                    ]
                ]
            , loadButton
            , publishButton
            ]
        , case createGistResult, gistErrorPaneVisible of
            Success gist, _ -> gistPane gist
            Failure err, true -> ErrorPaneAction <$> closeableErrorPane err
            Failure _, false -> empty
            _, _ -> empty
        ]
  where
  gistIdInputId = "gist-id"

  parsedGistId :: Maybe (Either String GistId)
  parsedGistId = parseGistUrl <$> gistUrl

  authButton authorisedView = case view authStatusAuthRole <$> authStatus of
    Failure _ ->
      button
        [ idPublishGist
        , classes [ btn, btnSmall, btnDanger ]
        ]
        [ text "Failure" ]
    Success Anonymous ->
      a
        [ idPublishGist
        , classes [ btn, btnSmall, btnSecondary ]
        , href "/api/oauth/github"
        ]
        [ icon Github
        , nbsp
        , text "Log In"
        ]
    Success GithubUser -> authorisedView
    Loading ->
      button
        [ idPublishGist
        , classes [ btn, btnSmall, btnSecondary ]
        , disabled true
        ]
        [ icon Spinner ]
    NotAsked ->
      button
        [ idPublishGist
        , classes [ btn, btnSmall, btnSecondary ]
        , disabled true
        ]
        [ icon Spinner ]

  publishButton = case createGistResult of
    Success _ ->
      button
        [ idPublishGist
        , classes [ btn, btnSmall, btnSecondary ]
        , onClick $ const PublishOrUpdateGist
        ]
        [ text "Save" ]
    Loading ->
      -- make the button extra wide in this case, because there's no load button
      button
        [ idPublishGist
        , classes [ btn, btnSmall, btnSecondary, ClassName "double-width" ]
        , disabled true
        ]
        [ icon Spinner ]
    _ ->
      button
        [ idPublishGist
        , classes [ btn, btnSmall, btnSecondary ]
        , onClick $ const PublishOrUpdateGist
        ]
        [ text "Save" ]

  loadButton = case createGistResult of
    -- no load button in this case; publish button should be twice the size
    Loading -> empty
    _ ->
      button
        [ idLoadGist
        , classes
            [ btn
            , btnSmall
            , case parsedGistId of
                Just (Left _) -> btnDanger
                Just (Right _) -> btnSecondary
                Nothing -> btnSecondary
            ]
        , onClick $ const LoadGist
        , disabled
            $ case parsedGistId of
                Just (Left _) -> true
                Just (Right _) -> false
                Nothing -> true
        ]
        [ text "Load" ]

gistPane :: forall p i. Gist -> HTML p i
gistPane gist =
  div [ class_ $ ClassName "gist-link" ]
    [ a
        [ href $ view gistHtmlUrl gist
        , target "_blank"
        ]
        [ text $ "View on Github" ]
    ]
