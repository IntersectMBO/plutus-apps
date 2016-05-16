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
import           Data.Bifunctor
import           Data.Char
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
import           Language.PureScript.Bridge.PSTypes
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
  typeFor _ _ _ = languageBridge (Proxy :: Proxy bridgeSelector) (mkTypeInfo (Proxy :: Proxy a))

class HasBridge a where
  languageBridge :: Proxy a -> FullBridge

-- | Use 'PureScript' 'DefaultBridge' if 'defaultBridge' suffices for your needs.
data DefaultBridge

-- | 'languageBridge' for 'DefaultBridge' evaluates to 'buildBridge' 'defaultBridge' - no surprise there.
instance HasBridge DefaultBridge where
  languageBridge _ = buildBridge defaultBridge

-- | A proxy for 'DefaultBridge'
defaultBridgeProxy :: Proxy DefaultBridge
defaultBridgeProxy = Proxy

data Settings = Settings {
  _apiModuleName   :: Text
  -- | This function parameters should instead be put in a Reader monad.
  --
  --   'baseUrl' will be put there by default, you can add additional parameters.
  --
  --   If your API uses a given parameter name multiple times with different types,
  --   only the ones matching the type of the first occurrence
  --   will be put in the Reader monad, all others will still be passed as function parameter.
, _readerParams    :: Set ParamName
, _standardImports :: ImportLines
}

defaultSettings :: Settings
defaultSettings = Settings {
    _apiModuleName    = "ServerAPI"
  , _readerParams    = Set.singleton $ Param baseURLId psString
  , _standardImports = importsFromList
        [ ImportLine "Prelude" (Set.fromList [ "Unit(..)" ])
        , ImportLine "Control.Monad.Reader.Class" (Set.fromList [ "class MonadReader" ])
        , ImportLine "Control.Monad.Error.Class" (Set.fromList [ "class MonadError" ])
        , ImportLine "Control.Monad.Aff.Class" (Set.fromList [ "class MonadAff" ])
        , ImportLine "Network.HTTP.Affjax" (Set.fromList [ "AJAX" ])
        , ImportLine "Global" (Set.fromList [ "encodeURIComponent" ]) -- from package globals
        , ImportLine "Data.Nullable" (Set.fromList [ "Nullable()", "toNullable" ])
        , ImportLine "Servant.PureScript.Affjax" (Set.fromList [ "defaultRequest", "affjax" ])
        , ImportLine "Servant.PureScript.Settings" (Set.fromList [ "Settings" ])
        , ImportLine "Servant.PureScript.Util" (Set.fromList [ "encodeListQuery", "encodeQueryItem", "getResult" ])
        ]
  }

-- | Add a parameter name to be us put in the Reader monad instead of being passed to the
--   generated functions.
addReaderParam :: ParamName -> Settings -> Settings
addReaderParam n opts = opts & over readerParams (Set.insert n)

type ParamName = Text

baseURLId :: ParamName
baseURLId = "baseURL"

baseURLParam :: Param TypeInfo
baseURLParam = Param baseURLId psString

data Param f = Param {
  _pName :: Text
, _pType :: f
} deriving (Eq, Ord, Show)

type PSParam = Param (TypeInfo 'PureScript)

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
  ) => Proxy api -> Proxy bridgeSelector -> [Req (TypeInfo 'PureScript)]
apiToList _ _ = listFromAPI (Proxy :: Proxy (PureScript bridgeSelector)) (Proxy :: Proxy (TypeInfo 'PureScript)) (Proxy :: Proxy api)


apiToDoc :: forall bridgeSelector api.
  ( HasForeign (PureScript bridgeSelector) TypeInfo api
  , GenerateList TypeInfo (Foreign TypeInfo api)
  , HasBridge bridgeSelector
  ) => Proxy api -> Proxy bridgeSelector -> Settings UnBridged -> [Req TypeInfo]

-- | Transform a given identifer to be a valid PureScript variable name (hopefully).
toPSVarName :: Text -> Text
toPSVarName = dropInvalid . unTitle . doPrefix . replaceInvalid
  where
    unTitle = uncurry mappend . first T.toLower . T.splitAt 1
    doPrefix t = let
        s = T.head t
        cond = isAlpha s || s == '_'
      in
        if cond then t else "_" <> t
    replaceInvalid  = T.replace "-" "_"
    dropInvalid = let
        isValid c = isAlphaNum c || c == '_'
      in
        T.filter isValid

makeLenses ''Settings
