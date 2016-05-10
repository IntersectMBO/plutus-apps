{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}


module Servant.PureScript.Internal where

import           Control.Lens
import           Data.Aeson
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe                          (mapMaybe, maybeToList)
import           Data.Monoid
import           Data.Proxy
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import           Data.Typeable
import           GHC.Generics                        hiding (to)
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.Printer
import           Language.PureScript.Bridge.TypeInfo
import           Servant.API
import           Servant.Foreign


-- | Our language type is Paramized, so you can choose a custom 'TypeBridge' for your translation, by
--   providing your own data type and implementing 'HasBridge' for it.
--
-- > data MyBridge
-- >
-- > myBridge :: TypeBridge
-- > myBridge = defaultBridge <|> customBridge1 <|> customBridge2
-- >
-- > instance HasBridge MyBridge where
-- >   languageBridge _ = myBridge
--
data PureScript bridgeSelector

instance (Generic a, Typeable a, HasBridge bridgeSelector) => HasForeignType (PureScript bridgeSelector) TypeInfo a where
  typeFor _ _ _ = doBridge (languageBridge (Proxy :: Proxy bridgeSelector)) (mkTypeInfo (Proxy :: Proxy a))

class HasBridge a where
  languageBridge :: Proxy a -> TypeBridge

-- | Use 'PureScript' 'DefaultBridge' if 'defaultBridge' suffices for your needs.
data DefaultBridge

-- | 'languageBridge' for 'DefaultBridge' evaluates to 'defaultBridge' - no surprise there.
instance HasBridge DefaultBridge where
  languageBridge _ = defaultBridge


data Settings = Settings {
  -- | This function parameters should instead be put in a reader monad.
  --   'baseUrl' will be put there by default, you can add additional parameters.
  putInReader     :: Set ParamName
, standardImports :: ImportLines
}

defaultSettings :: Settings
defaultSettings = Settings {
    putInReader     = Set.singleton baseURLId
  , standardImports = importsFromList
        [ ImportLine "Prelude" (Set.fromList [ "Unit(..)" ])
        , ImportLine "Control.Monad.Reader.Class" (Set.fromList [ "class MonadReader" ])
        , ImportLine "Control.Monad.Aff.Class" (Set.fromList [ "class MonadAff" ])
        , ImportLine "Global" (Set.fromList [ "encodeURIComponent" ]) -- from package globals
        ]
  }

importsFromList ::  [ImportLine] -> Map Text ImportLine
importsFromList lines = let
    pairs = zip (map importModule lines) lines
    merge a b = ImportLine (importModule a) (importTypes a `Set.union` importTypes b)
  in
    Map.fromListWith merge pairs

type ParamName = Text

baseURLId :: ParamName
baseURLId = "baseURL"

data Param f = Param {
  _pName :: Text
, _pType :: f
}

makeLenses ''Param

{--
apiToPureScript :: forall bridgeSelector api.
  ( HasForeign (PureScript bridgeSelector) TypeInfo api
  , GenerateList TypeInfo (Foreign TypeInfo api)
  , HasBridge bridgeSelector
  ) => Proxy api -> Proxy bridgeSelector -> Text
apiToPureScript pAPI pBridge = let
    reqs =  apiToList pAPI pBridge
    specificImports = T.unlines . map importLineToText . Map.elems . reqsToImportLines $ reqs
    genericImports =  T.unlines ["import Network.HTTP.Affjax"]
    reqFunctions = map reqToFunction reqs
    psModule = genericImports <> "\n" <> specificImports <> "\n" <> T.intercalate "\n" reqFunctions
  in
    psModule
--}

apiToList :: forall bridgeSelector api.
  ( HasForeign (PureScript bridgeSelector) TypeInfo api
  , GenerateList TypeInfo (Foreign TypeInfo api)
  , HasBridge bridgeSelector
  ) => Proxy api -> Proxy bridgeSelector -> [Req TypeInfo]
apiToList _ _ = listFromAPI (Proxy :: Proxy (PureScript bridgeSelector)) (Proxy :: Proxy TypeInfo) (Proxy :: Proxy api)
