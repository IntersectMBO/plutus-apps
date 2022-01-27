module Simulator.View
  ( simulatorTitle
  , simulationsPane
  , simulationsNav
  , simulatorTitleRefLabel
  , simulationsErrorRefLabel
  ) where

import Action.View (actionsPane)
import Action.Validation (actionIsValid)
import Bootstrap (active, alertDanger_, btn, empty, floatRight, nav, navItem, navLink)
import Component.ErrorPane (errorPane)
import Cursor (Cursor, current)
import Cursor as Cursor
import Data.Array as Array
import Data.Either (Either(..))
import Data.Lens (_Right, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..))
import Data.String as String
import Halogen (RefLabel(RefLabel))
import Halogen.HTML (ClassName(ClassName), HTML, a, button, code_, div, div_, h1_, li, p_, pre_, span, text, ul)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes, disabled, id, ref)
import Icons (Icon(..), icon)
import Language.Haskell.Interpreter (CompilationError(..))
import Language.Haskell.Interpreter as PI
import Plutus.V1.Ledger.Value (Value)
import Network.RemoteData (RemoteData(..), _Success)
import MainFrame.Lenses (_functionSchema, _result)
import MainFrame.Types (FullSimulation, HAction(..), View(..), SimulatorAction, WebCompilationResult, WebEvaluationResult)
import Playground.Types (PlaygroundError(..), Simulation(..), SimulatorWallet)
import Prelude (const, not, pure, show, (#), ($), (<$>), (<<<), (<>), (==), (>))
import Wallet.View (walletsPane)

simulatorTitle :: forall p. HTML p HAction
simulatorTitle =
  div
    [ class_ $ ClassName "main-header"
    , ref simulatorTitleRefLabel
    ]
    [ h1_ [ text "Simulator" ]
    , a
        [ class_ btn
        , onClick $ const $ ChangeView Editor
        ]
        [ text "< Return to Editor" ]
    ]

simulationsPane :: forall p. Value -> Maybe Int -> WebCompilationResult -> Cursor FullSimulation -> HTML p HAction
simulationsPane initialValue actionDrag compilationResult simulations = case current simulations of
  Just { simulation: Simulation { simulationWallets, simulationActions }, evaluationResult } ->
    div
      [ class_ $ ClassName "simulations" ]
      [ simulationsNav simulations
      , div
          [ class_ $ ClassName "simulation" ]
          [ div
              [ classes [ ClassName "simulation-controls", floatRight ] ]
              [ evaluateActionsButton simulationWallets simulationActions evaluationResult
              , viewTransactionsButton evaluationResult
              ]
          , walletsPane endpointSignatures initialValue simulationWallets
          , actionsPane actionDrag simulationWallets simulationActions
          , div
              [ classes [ ClassName "simulation-controls" ] ]
              [ evaluateActionsButton simulationWallets simulationActions evaluationResult
              , viewTransactionsButton evaluationResult
              ]
          , case evaluationResult of
              Failure error -> errorPane error
              Success (Left error) -> actionsErrorPane error
              _ -> empty
          ]
      ]
  Nothing ->
    div
      [ class_ $ ClassName "simulations" ]
      [ p_ [ text "Return to the Editor and compile a contract to get started." ] ]
  where
  endpointSignatures = view (_Success <<< _Right <<< _Newtype <<< _result <<< _functionSchema) compilationResult

simulationsNav :: forall p. Cursor FullSimulation -> HTML p HAction
simulationsNav simulations =
  ul
    [ classes [ nav, ClassName "nav-tabs" ]
    ]
    ( ( simulations
          # Cursor.mapWithIndex (simulationNavItem (Cursor.length simulations > 1) (Cursor.getIndex simulations))
          # Cursor.toArray
          # Array.concat
      )
        <> [ addSimulationControl ]
    )

simulationNavItem :: forall p. Boolean -> Int -> Int -> FullSimulation -> Array (HTML p HAction)
simulationNavItem canClose activeIndex index { simulation: Simulation { simulationName } } =
  [ li
      [ id $ "simulation-nav-item-" <> show index
      , class_ navItem
      ]
      [ a
          [ classes navLinkClasses
          , onClick $ const $ SetSimulationSlot index
          ]
          [ text simulationName ]
      , if canClose then
          button
            [ classes [ btn, navItemButtonClass ]
            , onClick $ const $ RemoveSimulationSlot index
            ]
            [ icon Close ]
        else
          empty
      ]
  ]
  where
  navLinkClasses = if activeIndex == index then [ navLink, active ] else [ navLink ]

addSimulationControl :: forall p. HTML p HAction
addSimulationControl =
  li
    [ id "simulation-nav-item-add"
    , class_ navItem
    ]
    [ span
        [ class_ navLink ]
        [ button
            [ classes [ btn, navItemButtonClass ]
            , onClick $ const $ AddSimulationSlot
            ]
            [ icon Plus ]
        ]
    ]

evaluateActionsButton :: forall p. Array SimulatorWallet -> Array SimulatorAction -> WebEvaluationResult -> HTML p HAction
evaluateActionsButton simulationWallets simulationActions evaluationResult =
  button
    [ classes [ btn, ClassName "btn-green" ]
    , disabled $ not valid
    , onClick $ const EvaluateActions
    ]
    [ btnText evaluationResult valid ]
  where
  valid = (Array.all <<< actionIsValid) simulationWallets simulationActions

  btnText Loading _ = icon Spinner

  btnText _ false = text "Fix Errors"

  btnText _ _ = text "Evaluate"

viewTransactionsButton :: forall p. WebEvaluationResult -> HTML p HAction
viewTransactionsButton evaluationResult =
  button
    [ classes [ btn, ClassName "btn-turquoise" ]
    , disabled isDisabled
    , onClick $ const $ ChangeView Transactions
    ]
    [ text "Transactions" ]
  where
  isDisabled = case evaluationResult of
    Success _ -> false
    _ -> true

actionsErrorPane :: forall p i. PlaygroundError -> HTML p i
actionsErrorPane error =
  div
    [ class_ $ ClassName "error-pane"
    , ref simulationsErrorRefLabel
    ]
    [ alertDanger_
        ( (div_ <<< pure)
            <$> (showPlaygroundError error <> [ text "Please try again or contact support for assistance." ])
        )
    ]

------------------------------------------------------------
-- | There's a few errors that make sense to display nicely, others should not occur so lets
-- | not deal with them.
showPlaygroundError :: forall p i. PlaygroundError -> Array (HTML p i)
showPlaygroundError (CompilationErrors errors) =
  [ text "Compilation Errors" ]
    <> (showCompilationError <$> errors)

showPlaygroundError (InterpreterError (PI.TimeoutError error)) =
  [ text "Interpreter Timed Out"
  , code_ [ text error ]
  ]

showPlaygroundError (InterpreterError (PI.CompilationErrors errors)) =
  [ text "Interpreter Errors" ]
    <> (showCompilationError <$> errors)

showPlaygroundError (RollupError error) =
  [ text "Error Calculating Final Blockchain State"
  , code_ [ text error ]
  ]

showPlaygroundError (OtherError error) =
  [ text "Unknown Evaluation Error"
  , code_ [ text error ]
  ]

showPlaygroundError (JsonDecodingError { expected, decodingError, input }) =
  [ text "Decoding Error"
  , code_ [ text $ "Expected: " <> expected ]
  , code_ [ text $ "Error: " <> decodingError ]
  , code_ [ text $ "Input: " <> input ]
  ]

showCompilationError :: forall p i. CompilationError -> HTML p i
showCompilationError (RawError error) = code_ [ text error ]

showCompilationError (CompilationError { text: errors }) = pre_ [ text (String.joinWith "\n" errors) ]

------------------------------------------------------------
simulatorTitleRefLabel :: RefLabel
simulatorTitleRefLabel = RefLabel "simulations"

simulationsErrorRefLabel :: RefLabel
simulationsErrorRefLabel = RefLabel "simulation-errors"

navItemButtonClass :: ClassName
navItemButtonClass = ClassName "simulation-nav-item-control"
