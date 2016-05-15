{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}




module Language.PureScript.Bridge.Internal where

import           Control.Applicative
import           Control.Error.Util
import           Control.Lens
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Reader (ReaderT(..))
import           Data.Proxy
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Typeable
import           Unsafe.Coerce              (unsafeCoerce)


data Language = Haskell | PureScript

-- | Basic info about a data type:
data TypeInfo (lang :: Language) = TypeInfo {
  -- | Hackage package
  _typePackage    :: !Text
  -- | Full Module path
, _typeModule     :: !Text
, _typeName       :: !Text
, _typeParameters :: ![TypeInfo lang]
} deriving (Eq, Ord, Show)

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

buildBridge :: FixUpBridge -> BridgePart -> FullBridge
buildBridge fixUp (BridgeBuilder reader) = let
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


doCheck :: (a -> Bool) -> Lens' (TypeInfo 'Haskell) a -> BridgeBuilder ()
doCheck check l = guard $ views l check

(^==) :: Lens' (TypeInfo 'Haskell) a -> a -> BridgeBuilder ()
l ^== a = doCheck l (== a)

test :: BridgeBuilder (TypeInfo 'PureScript)
test = do
  eqTypeName "[]" <|> typeName ^== "Text"
  doCheck typeName (== "[]")
  typeName ^== "[]"
  typePackage ^== "huhu"
  return psString
  eqTypeParameters [mkTypeInfo (Proxy :: Proxy Char)]




newtype FunctionMay i a = FunctionMay (i -> Maybe a)

instance Functor (FunctionMay i) where
  fmap f (FunctionMay fun) = FunctionMay $ fmap f . fun

instance Applicative (FunctionMay i) where
  pure a = FunctionMay $ const (Just a)
  FunctionMay a <*> FunctionMay b = FunctionMay $ \i -> a i <*> b i

instance Alternative (FunctionMay i) where
  empty = FunctionMay $ const Nothing
  FunctionMay f1 <|> FunctionMay f2 = FunctionMay $ \i -> f1 i <|> f2 i


makeLenses ''TypeInfo
