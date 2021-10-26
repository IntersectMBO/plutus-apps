module Editor.Lenses
  ( _warnings
  , _keyBindings
  , _feedbackPaneMinimised
  , _lastCompiledCode
  , _currentCodeIsCompiled
  , _feedbackPaneDragStart
  , _feedbackPaneExtend
  , _feedbackPanePreviousExtend
  ) where

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Type.Proxy (Proxy(..))
import Editor.Types (State)
import Halogen.Monaco (KeyBindings)
import Language.Haskell.Interpreter (SourceCode)
import Prelude ((<<<))

_warnings :: forall s a. Lens' { warnings :: a | s } a
_warnings = prop (Proxy :: _ "warnings")

_keyBindings :: Lens' State KeyBindings
_keyBindings = _Newtype <<< prop (Proxy :: _ "keyBindings")

_feedbackPaneMinimised :: Lens' State Boolean
_feedbackPaneMinimised = _Newtype <<< prop (Proxy :: _ "feedbackPaneMinimised")

_lastCompiledCode :: Lens' State (Maybe SourceCode)
_lastCompiledCode = _Newtype <<< prop (Proxy :: _ "lastCompiledCode")

_currentCodeIsCompiled :: Lens' State Boolean
_currentCodeIsCompiled = _Newtype <<< prop (Proxy :: _ "currentCodeIsCompiled")

_feedbackPaneDragStart :: Lens' State (Maybe Int)
_feedbackPaneDragStart = _Newtype <<< prop (Proxy :: _ "feedbackPaneDragStart")

_feedbackPaneExtend :: Lens' State Int
_feedbackPaneExtend = _Newtype <<< prop (Proxy :: _ "feedbackPaneExtend")

_feedbackPanePreviousExtend :: Lens' State Int
_feedbackPanePreviousExtend = _Newtype <<< prop (Proxy :: _ "feedbackPanePreviousExtend")
