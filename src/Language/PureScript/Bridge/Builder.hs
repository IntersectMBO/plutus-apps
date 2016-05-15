{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Language.PureScript.Bridge.Builder where

import           Control.Applicative
import           Control.Error.Util
import           Control.Lens
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Reader          (ReaderT (..))
import           Data.Proxy
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Typeable
import           Language.PureScript.Bridge.Internal
import           Unsafe.Coerce                       (unsafeCoerce)

type FullBridge = TypeInfo 'Haskell -> TypeInfo 'PureScript



type FixUpBridge = Either (TypeInfo 'Haskell) (TypeInfo 'PureScript) -> TypeInfo 'PureScript

data BridgeData = BridgeData {
  -- | The Haskell type to translate.
    _haskType   :: TypeInfo 'Haskell
  -- | Reference to the bride itself, needed for translation of type constructors.
  , _fullBridge :: FullBridge
  }
makeLenses ''BridgeData


newtype BridgeBuilder a =
  BridgeBuilder (ReaderT BridgeData Maybe a)
    deriving (Functor, Applicative, MonadReader BridgeData)


type BridgePart = BridgeBuilder (TypeInfo 'Haskell)

instance MonadReader (TypeInfo 'Haskell) BridgeBuilder where
  ask = view haskType
  local f = local (f . _haskType)

-- | Bridge to PureScript by simply clearing out the '_typePackage' field.
--   This bridge is used by default as 'FixUpBridge' by buildBridge:
--
-- > buildBridge = buildBridgeWithCustomFixUp (lmap bridgeClearPackage)
--
--   You can customize the fixup by chaining you own fixup function:

-- > buildBridgeWithCustomFixUp (lmap bridgeClearPackage)
clearPackageBridge :: FullBridge
clearePackageBridge haskT = TypeInfo {
    _typeName       = haskT ^. typeName
  , _typeModule     = haskT ^. typeModule
  , _typeParameters = haskT ^. typeParameters
  , _typePackage    = haskT ^. typePackage
  }

-- | Build a bridge
buildBridge :: BridgePart -> FullBridge
buildBridge = buildBridgeWithCustomFixUp (lmap bridgeByClearPackage)

buildBridgeWithCustomFixUp :: FixUpBridge -> BridgePart -> FullBridge
buildBridgeWithCustomFixUp fixUp (BridgeBuilder reader) = let
    mayBridge haskT = runReaderT reader (BridgeData haskT bridge) haskT
    bridge haskT = fixUp . note haskT . mayBridge $ haskT
  in
    bridge


instance Alternative BridgeBuilder where
  empty = BridgeBuilder . ReaderT $ const Nothing
  BridgeBuilder a <|> BridgeBuilder b = BridgeBuilder . ReaderT $ \br -> let
          ia = runReaderT a br
          ib = runReaderT b br
        in
          ia <|> ib
