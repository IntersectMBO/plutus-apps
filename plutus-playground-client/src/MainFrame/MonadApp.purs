module MainFrame.MonadApp
  ( class MonadApp
  , HalogenApp(..)
  , editorGetContents
  , editorHandleAction
  , editorSetAnnotations
  , editorSetContents
  , getGistByGistId
  , getOauthStatus
  , postContract
  , postEvaluation
  , postGist
  , postGistByGistId
  , preventDefault
  , readFileFromDragEvent
  , resizeBalancesChart
  , resizeEditor
  , runHalogenApp
  , saveBuffer
  , scrollIntoView
  , setDataTransferData
  , setDropEffect
  ) where

import Prologue

import Animation (class MonadAnimate, animate)
import Auth (AuthStatus)
import Clipboard (class MonadClipboard, copy)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (mapExceptT, withExceptT)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Argonaut (Json, JsonDecodeError, printJsonDecodeError, stringify)
import Data.Array (cons)
import Data.Lens (over)
import Data.Maybe (maybe)
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype, unwrap, wrap)
import Editor.State (handleAction, saveBuffer) as Editor
import Editor.Types (Action) as Editor
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Gist (Gist, GistId, NewGist)
import Halogen (HalogenM, RefLabel, query, tell)
import Halogen as H
import Halogen.Chartist as Chartist
import Halogen.Extra as HE
import Halogen.Monaco as Monaco
import Language.Haskell.Interpreter (InterpreterError, SourceCode(SourceCode), InterpreterResult)
import MainFrame.Lenses (_balancesChartSlot, _editorSlot, _editorState)
import MainFrame.Types (ChildSlots, HAction, State)
import Monaco (IMarkerData)
import Playground.Server as Server
import Playground.Types (CompilationResult, Evaluation, EvaluationResult, PlaygroundError)
import Servant.PureScript (class MonadAjax, AjaxError, printAjaxError, request)
import StaticData (bufferLocalStorageKey)
import URI.RelativePart (_relPath)
import URI.RelativeRef (_relPart)
import Web.Event.Extra (class IsEvent)
import Web.Event.Extra as WebEvent
import Web.HTML.Event.DataTransfer (DropEffect)
import Web.HTML.Event.DataTransfer as DataTransfer
import Web.HTML.Event.DragEvent (DragEvent, dataTransfer)

class Monad m <= MonadApp m where
  editorGetContents :: m (Maybe SourceCode)
  editorSetContents :: SourceCode -> Maybe Int -> m Unit
  editorHandleAction :: Editor.Action -> m Unit
  editorSetAnnotations :: Array IMarkerData -> m Unit
  --
  saveBuffer :: String -> m Unit
  setDropEffect :: DropEffect -> DragEvent -> m Unit
  setDataTransferData :: DragEvent -> MediaType -> String -> m Unit
  readFileFromDragEvent :: DragEvent -> m String
  --
  getOauthStatus :: ExceptT String m AuthStatus
  getGistByGistId :: GistId -> ExceptT String m Gist
  postEvaluation :: Evaluation -> ExceptT String m (Either PlaygroundError EvaluationResult)
  postGist :: NewGist -> ExceptT String m Gist
  postGistByGistId :: NewGist -> GistId -> ExceptT String m Gist
  postContract :: SourceCode -> ExceptT String m (Either InterpreterError (InterpreterResult CompilationResult))
  resizeEditor :: m Unit
  resizeBalancesChart :: m Unit
  --
  preventDefault :: forall e. IsEvent e => e -> m Unit
  scrollIntoView :: RefLabel -> m Unit

newtype HalogenApp m a = HalogenApp (HalogenM State HAction ChildSlots Void m a)

derive instance newtypeHalogenApp :: Newtype (HalogenApp m a) _

derive newtype instance functorHalogenApp :: Functor (HalogenApp m)

derive newtype instance applicativeHalogenApp :: Applicative (HalogenApp m)

derive newtype instance applyHalogenApp :: Apply (HalogenApp m)

derive newtype instance bindHalogenApp :: Bind (HalogenApp m)

derive newtype instance monadHalogenApp :: Monad (HalogenApp m)

derive newtype instance monadTransHalogenApp :: MonadTrans HalogenApp

derive newtype instance monadStateHalogenApp :: MonadState State (HalogenApp m)

derive newtype instance monadAskHalogenApp :: MonadAsk env m => MonadAsk env (HalogenApp m)

derive newtype instance monadEffectHalogenApp :: MonadEffect m => MonadEffect (HalogenApp m)

derive newtype instance monadAffHalogenApp :: MonadAff m => MonadAff (HalogenApp m)

instance monadAnimateHalogenApp :: MonadAff m => MonadAnimate (HalogenApp m) State where
  animate toggle action = HalogenApp $ animate toggle (unwrap action)

instance monadClipboardHalogenApp :: MonadEffect m => MonadClipboard (HalogenApp m) where
  copy = liftEffect <<< copy

instance monadThrowHalogenApp :: MonadThrow e m => MonadThrow e (HalogenApp m) where
  throwError e = lift (throwError e)

instance
  MonadAjax JsonDecodeError Json (AjaxError JsonDecodeError Json) m =>
  MonadAjax JsonDecodeError Json String (HalogenApp m) where
  request r = withExceptT (printAjaxError stringify printJsonDecodeError)
    $ mapExceptT (HalogenApp <<< lift)
    $ request
    $ r
        { uri = over
            (_relPart <<< _relPath)
            (Just <<< maybe [ "api" ] (cons "api"))
            r.uri
        }

------------------------------------------------------------
runHalogenApp :: forall m a. HalogenApp m a -> HalogenM State HAction ChildSlots Void m a
runHalogenApp = unwrap

instance
  ( MonadAjax JsonDecodeError Json (AjaxError JsonDecodeError Json) m
  , MonadAff m
  ) =>
  MonadApp (HalogenApp m) where
  editorGetContents = do
    mText <- wrap $ query _editorSlot unit $ Monaco.GetText identity
    pure $ map SourceCode mText
  editorSetContents (SourceCode contents) _ = wrap $ void $ tell _editorSlot unit $ Monaco.SetText contents
  editorHandleAction action = wrap $ HE.imapState _editorState $ Editor.handleAction bufferLocalStorageKey action
  editorSetAnnotations annotations = wrap $ void $ query _editorSlot unit $ Monaco.SetModelMarkers annotations identity
  setDropEffect dropEffect event = wrap $ liftEffect $ DataTransfer.setDropEffect dropEffect $ dataTransfer event
  setDataTransferData event mimeType value = wrap $ liftEffect $ DataTransfer.setData mimeType value $ dataTransfer event
  readFileFromDragEvent event = wrap $ liftAff $ WebEvent.readFileFromDragEvent event
  saveBuffer text = wrap $ Editor.saveBuffer bufferLocalStorageKey text
  getOauthStatus = Server.getOauthStatus
  getGistByGistId = Server.getGistsByGistId
  postEvaluation = Server.postEvaluate
  postGist = Server.postGists
  postGistByGistId = Server.postGistsByGistId
  postContract = Server.postContract
  resizeEditor = wrap $ void $ H.query _editorSlot unit (Monaco.Resize unit)
  resizeBalancesChart = wrap $ void $ H.query _balancesChartSlot unit (Chartist.Resize unit)
  preventDefault event = wrap $ liftEffect $ WebEvent.preventDefault event
  scrollIntoView ref = wrap $ HE.scrollIntoView ref

instance MonadApp m => MonadApp (StateT s m) where
  editorGetContents = lift editorGetContents
  editorSetContents contents cursor = lift $ editorSetContents contents cursor
  editorHandleAction action = lift $ editorHandleAction action
  editorSetAnnotations annotations = lift $ editorSetAnnotations annotations
  setDropEffect dropEffect event = lift $ setDropEffect dropEffect event
  setDataTransferData event mimeType value = lift $ setDataTransferData event mimeType value
  readFileFromDragEvent event = lift $ readFileFromDragEvent event
  saveBuffer text = lift $ saveBuffer text
  getOauthStatus = mapExceptT lift getOauthStatus
  getGistByGistId gistId = mapExceptT lift $ getGistByGistId gistId
  postEvaluation evaluation = mapExceptT lift $ postEvaluation evaluation
  postGist newGist = mapExceptT lift $ postGist newGist
  postGistByGistId newGist gistId = mapExceptT lift $ postGistByGistId newGist gistId
  postContract source = mapExceptT lift $ postContract source
  resizeEditor = lift resizeEditor
  resizeBalancesChart = lift resizeBalancesChart
  preventDefault event = lift $ preventDefault event
  scrollIntoView = lift <<< scrollIntoView
