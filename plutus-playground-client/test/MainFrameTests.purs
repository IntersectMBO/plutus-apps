module MainFrameTests
  ( all
  ) where

import Prologue

import Animation (class MonadAnimate)
import Clipboard (class MonadClipboard)
import Control.Monad.Except (except)
import Control.Monad.Except.Trans (class MonadThrow, throwError)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (StateT(..), execStateT, state)
import Control.Monad.State.Class (class MonadState, get)
import Control.Monad.State.Extra (zoomStateT)
import Control.Monad.Trans.Class (lift)
import Cursor as Cursor
import Data.Argonaut (Json, JsonDecodeError, encodeJson)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Extra (parseDecodeJson)
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.HTTP.Method (Method(..))
import Data.Lens (Lens', _1, _2, _Just, _Left, assign, preview, set, use, view, (^.))
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse_)
import Editor.Types (State(..)) as Editor
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error)
import Gist (Gist, GistId(..), gistId)
import Gists.Types (GistAction(..))
import Halogen.Monaco (KeyBindings(..)) as Editor
import Language.Haskell.Interpreter (InterpreterError, InterpreterResult, SourceCode(..))
import MainFrame.Lenses (_authStatus, _contractDemoEditorContents, _createGistResult, _currentView, _simulations)
import MainFrame.MonadApp (class MonadApp)
import MainFrame.State (handleAction, mkInitialState)
import MainFrame.Types (HAction(..), State, View(Editor, Simulations))
import Network.RemoteData (isFailure, isNotAsked, isSuccess)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Playground.Gists (playgroundGistFile)
import Playground.Server as Server
import Playground.Types (CompilationResult, ContractDemo, EvaluationResult)
import Servant.PureScript (class MonadAjax, segmentsToPathAbsolute)
import StaticData (bufferLocalStorageKey, lookupContractDemo, mkContractDemos)
import Test.QuickCheck ((<?>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))
import URI.Extra.QueryPairs as QueryPairs
import URI.Host as Host
import URI.RelativePart (_relPath)
import URI.RelativeRef (_relPart)
import URI.RelativeRef as RelativeRef

all :: Spec Unit
all =
  describe "MainFrame" do
    evalTests

------------------------------------------------------------
type World =
  { gists :: Map GistId Gist
  , editorContents :: Maybe SourceCode
  , localStorage :: Map String String
  , evaluationResult :: Either String EvaluationResult
  , compilationResult :: (Either String (Either InterpreterError (InterpreterResult CompilationResult)))
  }

_gists :: forall r a. Lens' { gists :: a | r } a
_gists = prop (Proxy :: _ "gists")

_editorContents :: forall r a. Lens' { editorContents :: a | r } a
_editorContents = prop (Proxy :: _ "editorContents")

_localStorage :: forall r a. Lens' { localStorage :: a | r } a
_localStorage = prop (Proxy :: _ "localStorage")

-- | A dummy implementation of `MonadApp`, for testing the main handleAction loop.
newtype MockApp m a = MockApp (StateT (Tuple World State) m a)

derive instance newtypeMockApp :: Newtype (MockApp m a) _

derive newtype instance functorMockApp :: Functor m => Functor (MockApp m)

derive newtype instance applicativeMockApp :: Monad m => Applicative (MockApp m)

derive newtype instance applyMockApp :: Monad m => Apply (MockApp m)

derive newtype instance bindMockApp :: Monad m => Bind (MockApp m)

derive newtype instance monadMockApp :: Monad m => Monad (MockApp m)

instance monadStateMockApp :: Monad m => MonadState State (MockApp m) where
  state = MockApp <<< zoomStateT _2 <<< state

instance Monad m => MonadAjax JsonDecodeError Json String (MockApp m) where
  request req = do
    let mMethod = preview _Left req.method
    let mUri = preview (_relPart <<< _relPath <<< _Just) req.uri
    jsonResult <- case mMethod, mUri of
      Just GET, Just [ "oauth", "status" ] ->
        pure $ encodeJson { _authStatusAuthRole: "GithubUser" }
      Just GET, Just [ "gists", gistId ] -> do
        Tuple { gists } _ <- lift $ MockApp $ get
        case Map.lookup (GistId gistId) gists of
          Nothing -> throwError $ "Not found: " <> gistId
          Just gist -> pure $ encodeJson gist
      Just POST, Just [ "contract" ] -> do
        Tuple { compilationResult } _ <- lift $ MockApp $ get
        except $ map encodeJson $ compilationResult
      _, _ -> throwError
        $ "Resource not mocked: "
            <> show mMethod
            <> " "
            <> RelativeRef.print
              { printUserInfo: identity
              , printHosts: Host.print
              , printPath: identity
              , printRelPath: Left <<< segmentsToPathAbsolute
              , printQuery: QueryPairs.print identity identity
              , printFragment: identity
              }
              req.uri
    except $ lmap printJsonDecodeError $ req.decode jsonResult

instance monadAppMockApp :: Monad m => MonadApp (MockApp m) where
  editorGetContents =
    MockApp do
      editorContents <- use (_1 <<< _editorContents)
      pure editorContents
  editorSetContents contents _ =
    MockApp
      $ assign (_1 <<< _editorContents) (Just contents)
  editorHandleAction _ = pure unit
  editorSetAnnotations _ = pure unit
  --
  saveBuffer contents =
    MockApp
      $ assign (_1 <<< _localStorage <<< at (unwrap bufferLocalStorageKey)) (Just contents)
  preventDefault _ = pure unit
  setDropEffect _ _ = pure unit
  setDataTransferData _ _ _ = pure unit
  readFileFromDragEvent _ = pure "TEST"
  --
  getOauthStatus = Server.getOauthStatus
  getGistByGistId = Server.getGistsByGistId
  postEvaluation = Server.postEvaluate
  postGist = Server.postGists
  postGistByGistId = Server.postGistsByGistId
  postContract = Server.postContract
  resizeEditor = pure unit
  resizeBalancesChart = pure unit
  scrollIntoView _ = pure unit

instance monadRecMockApp :: Monad m => MonadRec (MockApp m) where
  tailRecM step a = do
    v <- step a
    case v of
      Loop cont -> tailRecM step cont
      Done result -> pure result

-- | The mock app makes no attempt to animate anything, and just calls the embedded `action`.
instance monadAnimateMockApp :: MonadAnimate (MockApp m) State where
  animate _ action = action

instance monadClipboardMockApp :: Monad m => MonadClipboard (MockApp m) where
  copy _ = pure unit

execMockApp :: forall m. MonadThrow Error m => World -> Array HAction -> m (Tuple World State)
execMockApp world queries = do
  initialState <-
    mkInitialState
      ( Editor.State
          { keyBindings: Editor.DefaultBindings
          , feedbackPaneMinimised: false
          , lastCompiledCode: Nothing
          , currentCodeIsCompiled: false
          , feedbackPaneDragStart: Nothing
          , feedbackPaneExtend: 0
          , feedbackPanePreviousExtend: 0
          }
      )
  state <-
    execStateT
      (unwrap (traverse_ handleAction queries :: MockApp m Unit))
      (Tuple world initialState)
  pure state

------------------------------------------------------------
mockWorld :: World
mockWorld =
  { gists: Map.empty
  , editorContents: Nothing
  , localStorage: Map.empty
  , compilationResult: Left "Not mocked"
  , evaluationResult: Left "Not mocked"
  }

evalTests :: Spec Unit
evalTests =
  describe "handleAction" do
    it "CheckAuthStatus" do
      Tuple _ finalState <- execMockApp mockWorld [ CheckAuthStatus ]
      (finalState ^. _authStatus) `shouldSatisfy` isSuccess
    it "ChangeView" do
      quickCheck \aView -> do
        let
          result = execMockApp mockWorld [ ChangeView aView ]
        case result of
          Right (Tuple _ finalState) -> (aView == view _currentView finalState) <?> "Unexpected final view."
          Left err -> false <?> show err
    describe "LoadGist" do
      it "Bad URL" do
        Tuple _ finalState <-
          execMockApp mockWorld
            [ GistAction $ SetGistUrl "9cfe"
            , GistAction LoadGist
            ]
        (finalState ^. _createGistResult) `shouldSatisfy` isNotAsked
      it "Invalid URL" do
        Tuple _ finalState <-
          execMockApp mockWorld
            [ GistAction $ SetGistUrl "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            , GistAction LoadGist
            ]
        (finalState ^. _createGistResult) `shouldSatisfy` isFailure
      it "Gist loaded successfully" do
        contents <- liftEffect $ FS.readTextFile UTF8 "test/gist1.json"
        case parseDecodeJson contents of
          Left err -> fail $ printJsonDecodeError err
          Right gist -> do
            Tuple finalWorld finalState <-
              execMockApp
                (set (_gists <<< at (view gistId gist)) (Just gist) mockWorld)
                [ GistAction $ SetGistUrl (unwrap (view gistId gist))
                , GistAction LoadGist
                ]
            (finalState ^. _createGistResult) `shouldSatisfy` isSuccess
            Cursor.length (view _simulations finalState) `shouldEqual` 2
            case view playgroundGistFile gist of
              Nothing -> fail "Could not read gist content. Sample it data may be incorrect."
              Just sourceFile -> do
                view _editorContents finalWorld `shouldEqual` Just (SourceCode sourceFile)
                preview (_localStorage <<< ix (unwrap bufferLocalStorageKey)) finalWorld
                  `shouldEqual`
                    Just sourceFile
    it "Loading a script works." do
      Tuple finalWorld _ <-
        ( execMockApp (set _editorContents Nothing mockWorld)
            [ LoadScript "Game" ]
        )
      contractDemos :: Array ContractDemo <-
        either
          (throwError <<< error <<< printJsonDecodeError)
          pure
          mkContractDemos
      finalWorld.editorContents
        `shouldEqual`
          (view _contractDemoEditorContents <$> lookupContractDemo "Game" contractDemos)
    it "Loading a script switches back to the editor." do
      loadCompilationResponse1
        >>= \compilationResult -> do
          Tuple _ finalState <-
            execMockApp (mockWorld { compilationResult = Right $ Right compilationResult })
              [ ChangeView Simulations
              , LoadScript "Game"
              ]
          view _currentView finalState `shouldEqual` Editor

loadCompilationResponse1
  :: forall m
   . MonadEffect m
  => MonadThrow Error m
  => m (InterpreterResult CompilationResult)
loadCompilationResponse1 = do
  contents <- liftEffect $ FS.readTextFile UTF8 "generated/compilation_response.json"
  case parseDecodeJson contents of
    Left err -> throwError $ error $ printJsonDecodeError err
    Right value -> pure value
