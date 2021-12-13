module MainFrame.Lenses
  ( _actionDrag
  , _authStatus
  , _balancesChartSlot
  , _blockchainVisualisationState
  , _compilationResult
  , _contractDemoEditorContents
  , _contractDemos
  , _createGistResult
  , _currentDemoName
  , _currentView
  , _demoFilesMenuVisible
  , _editorSlot
  , _editorState
  , _evaluationResult
  , _functionSchema
  , _gistErrorPaneVisible
  , _gistUrl
  , _knownCurrencies
  , _lastSuccessfulCompilationResult
  , _result
  , _resultRollup
  , _simulation
  , _simulationActions
  , _simulationBlockchainVisualisationState
  , _simulationEvaluationResult
  , _simulationId
  , _simulationWallets
  , _simulations
  , _successfulCompilationResult
  , _successfulEvaluationResult
  , _walletKeys
  , _warnings
  , getKnownCurrencies
  ) where

import Prologue
import Auth (AuthStatus)
import Chain.Types as Chain
import Control.Monad.State.Class (class MonadState)
import Cursor (Cursor, _current)
import Data.Lens (Lens', _Right, lens, preview, set)
import Data.Lens.Extra (peruse)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Types (AffineTraversal')
import Data.Maybe (fromMaybe)
import Editor.Types (State) as Editor
import Gist (Gist)
import Language.Haskell.Interpreter (InterpreterError, InterpreterResult, SourceCode, _InterpreterResult)
import Ledger.CardanoWallet (WalletNumber)
import Ledger.Address (PaymentPubKeyHash)
import MainFrame.Types (FullSimulation, State, View, WebData, WebEvaluationResult)
import Network.RemoteData (_Success, RemoteData(NotAsked))
import Playground.Types (CompilationResult, ContractCall, ContractDemo, EvaluationResult, FunctionSchema, KnownCurrency, Simulation, SimulatorWallet)
import Schema (FormSchema)
import Schema.Types (FormArgument)
import Type.Proxy (Proxy(..))
import Wallet.Rollup.Types (AnnotatedTx)

_demoFilesMenuVisible :: Lens' State Boolean
_demoFilesMenuVisible = _Newtype <<< prop (Proxy :: _ "demoFilesMenuVisible")

_gistErrorPaneVisible :: Lens' State Boolean
_gistErrorPaneVisible = _Newtype <<< prop (Proxy :: _ "gistErrorPaneVisible")

_currentView :: Lens' State View
_currentView = _Newtype <<< prop (Proxy :: _ "currentView")

_contractDemos :: Lens' State (Array ContractDemo)
_contractDemos = _Newtype <<< prop (Proxy :: _ "contractDemos")

_currentDemoName :: Lens' State (Maybe String)
_currentDemoName = _Newtype <<< prop (Proxy :: _ "currentDemoName")

_editorState :: Lens' State Editor.State
_editorState = _Newtype <<< prop (Proxy :: _ "editorState")

_simulations :: Lens' State (Cursor FullSimulation)
_simulations = _Newtype <<< prop (Proxy :: _ "simulations")

_actionDrag :: Lens' State (Maybe Int)
_actionDrag = _Newtype <<< prop (Proxy :: _ "actionDrag")

_evaluationResult :: Lens' State WebEvaluationResult
_evaluationResult = withDefault NotAsked (_simulations <<< _current <<< _simulationEvaluationResult)

_successfulEvaluationResult :: AffineTraversal' State EvaluationResult
_successfulEvaluationResult = _evaluationResult <<< _Success <<< _Right

_compilationResult :: Lens' State (WebData (Either InterpreterError (InterpreterResult CompilationResult)))
_compilationResult = _Newtype <<< lens g s
  where
  g r = r.compilationResult

  s r c = case preview (_Success <<< _Right <<< _InterpreterResult <<< _result) c of
    Just cr -> r { compilationResult = c, lastSuccessfulCompilationResult = Just cr }
    Nothing -> r { compilationResult = c }

_successfulCompilationResult :: AffineTraversal' State CompilationResult
_successfulCompilationResult = _compilationResult <<< _Success <<< _Right <<< _InterpreterResult <<< _result

_lastSuccessfulCompilationResult :: Lens' State (Maybe CompilationResult)
_lastSuccessfulCompilationResult = _Newtype <<< prop (Proxy :: _ "lastSuccessfulCompilationResult")

_authStatus :: Lens' State (WebData AuthStatus)
_authStatus = _Newtype <<< prop (Proxy :: _ "authStatus")

_createGistResult :: Lens' State (WebData Gist)
_createGistResult = _Newtype <<< prop (Proxy :: _ "createGistResult")

_gistUrl :: Lens' State (Maybe String)
_gistUrl = _Newtype <<< prop (Proxy :: _ "gistUrl")

_blockchainVisualisationState :: Lens' State Chain.State
_blockchainVisualisationState = withDefault Chain.initialState (_simulations <<< _current <<< _simulationBlockchainVisualisationState)

_simulation :: Lens' FullSimulation Simulation
_simulation = prop (Proxy :: _ "simulation")

_simulationEvaluationResult :: Lens' FullSimulation WebEvaluationResult
_simulationEvaluationResult = prop (Proxy :: _ "evaluationResult")

_simulationBlockchainVisualisationState :: Lens' FullSimulation Chain.State
_simulationBlockchainVisualisationState = prop (Proxy :: _ "blockchainVisualisationState")

------------------------------------------------------------
withDefault :: forall s a. a -> AffineTraversal' s a -> Lens' s a
withDefault def l = lens doGet doSet
  where
  doGet s = fromMaybe def $ preview l s

  doSet s a = set l a s

------------------------------------------------------------
_editorSlot :: Proxy "editorSlot"
_editorSlot = Proxy

_balancesChartSlot :: Proxy "balancesChartSlot"
_balancesChartSlot = Proxy

------------------------------------------------------------
_contractDemoEditorContents :: Lens' ContractDemo SourceCode
_contractDemoEditorContents = _Newtype <<< prop (Proxy :: _ "contractDemoEditorContents")

_simulationId :: Lens' Simulation Int
_simulationId = _Newtype <<< prop (Proxy :: _ "simulationId")

_simulationActions :: Lens' Simulation (Array (ContractCall FormArgument))
_simulationActions = _Newtype <<< prop (Proxy :: _ "simulationActions")

_simulationWallets :: Lens' Simulation (Array SimulatorWallet)
_simulationWallets = _Newtype <<< prop (Proxy :: _ "simulationWallets")

_resultRollup :: Lens' EvaluationResult (Array (Array AnnotatedTx))
_resultRollup = _Newtype <<< prop (Proxy :: _ "resultRollup")

_functionSchema :: Lens' CompilationResult (Array (FunctionSchema FormSchema))
_functionSchema = _Newtype <<< prop (Proxy :: _ "functionSchema")

_walletKeys :: Lens' EvaluationResult (Array (Tuple PaymentPubKeyHash WalletNumber))
_walletKeys = _Newtype <<< prop (Proxy :: _ "walletKeys")

_knownCurrencies :: Lens' CompilationResult (Array KnownCurrency)
_knownCurrencies = _Newtype <<< prop (Proxy :: _ "knownCurrencies")

--- Language.Haskell.Interpreter ---
_result :: forall s a. Lens' { result :: a | s } a
_result = prop (Proxy :: _ "result")

_warnings :: forall s a. Lens' { warnings :: a | s } a
_warnings = prop (Proxy :: _ "warnings")

getKnownCurrencies :: forall m. MonadState State m => m (Array KnownCurrency)
getKnownCurrencies = fromMaybe [] <$> peruse (_successfulCompilationResult <<< _knownCurrencies)
