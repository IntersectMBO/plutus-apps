{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}

module Servant.PureScript.Internal where

import Control.Lens
import Data.Bifunctor
import Data.Char
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Language.PureScript.Bridge hiding (psTypes)
import qualified Language.PureScript.Bridge.CodeGenSwitches as Switches
import Servant.Foreign
import Servant.Foreign.Internal

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
data PureScript bridgeSelector

instance forall a bridgeSelector. (Typeable a, HasBridge bridgeSelector) => HasForeignType (PureScript bridgeSelector) PSType a where
  typeFor _ _ _ = languageBridge (Proxy :: Proxy bridgeSelector) (mkTypeInfo @a)

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

type ParamName = Text

newtype SumTypeByTypeInfo (lang :: Language) = SumTypeByTypeInfo
  { getSumTypeByTypeInfo :: SumType lang }
  deriving (Show)

instance Eq (SumTypeByTypeInfo lang) where
  SumTypeByTypeInfo (SumType ty1 _  _) == SumTypeByTypeInfo (SumType ty2 _ _) = ty1 == ty2

instance Ord (SumTypeByTypeInfo lang) where
  compare (SumTypeByTypeInfo (SumType ty1 _ _)) (SumTypeByTypeInfo (SumType ty2 _ _)) =
    compare ty1 ty2

data Settings = Settings
  { _apiModuleName :: Text,
    _globalHeaders :: Set ParamName,
    _globalQueryParams :: Set ParamName,
    _psBridgeSwitches :: Switches.Switch,
    _psTypes :: Set (SumTypeByTypeInfo 'Haskell),
    _standardImports :: ImportLines
  }

makeLenses ''Settings

defaultSettings :: Settings
defaultSettings =
  Settings
    { _apiModuleName = "ServerAPI",
      _globalHeaders = Set.empty,
      _globalQueryParams = Set.empty,
      _psBridgeSwitches = mempty,
      _psTypes = mempty,
      _standardImports =
        importsFromList
          [ ImportLine "Affjax.RequestHeader" (Set.fromList ["RequestHeader(..)"]),
            ImportLine "Control.Monad.Except" (Set.fromList ["ExceptT"]),
            ImportLine "Data.Argonaut" (Set.fromList ["Json", "JsonDecodeError"]),
            ImportLine "Data.Argonaut.Decode.Aeson" $ Set.fromList ["(</$\\>)", "(</*\\>)", "(</\\>)"],
            ImportLine "Data.Argonaut.Encode.Aeson" $ Set.fromList ["(>$<)", "(>/\\<)"],
            ImportLine "Data.Array" (Set.fromList ["catMaybes"]),
            ImportLine "Data.Either" (Set.fromList ["Either(..)"]),
            ImportLine "Data.Foldable" (Set.fromList ["fold"]),
            ImportLine "Data.HTTP.Method" (Set.fromList ["Method(..)"]),
            ImportLine "Data.Maybe" (Set.fromList ["Maybe(..)"]),
            ImportLine "Data.Tuple" (Set.fromList ["Tuple(..)"]),
            ImportLine "Servant.PureScript" (Set.fromList ["class MonadAjax", "flagQueryPairs", "paramListQueryPairs", "paramQueryPairs", "request", "toHeader", "toPathSegment"]),
            ImportLine "URI" (Set.fromList ["PathAbsolute(..)", "RelativePart(..)", "RelativeRef(..)"]),
            ImportLine "URI.Path.Segment" (Set.fromList ["segmentNZFromString"])
          ]
    }

-- | Add a header that will not be added to any client function signatures.
--   You are responsible for configuring this header manually in your
--   MonadAjax instance.
addGlobalHeader :: ParamName -> Settings -> Settings
addGlobalHeader name = over globalHeaders (Set.insert name)

-- | Add a query parameter that will not be added to any client function signatures.
--   You are responsible for configuring this query parameter manually in your
--   MonadAjax instance.
addGlobalQueryParam :: ParamName -> Settings -> Settings
addGlobalQueryParam name = over globalQueryParams (Set.insert name)

-- | Add types to generate. Types will automatically be generated for your
-- APIs, but any additional types need to be manually generated.
addTypes :: [SumType 'Haskell] -> Settings -> Settings
addTypes = over psTypes . flip Set.union . Set.fromList . fmap SumTypeByTypeInfo

-- | Add types to generate. Types will automatically be generated for your
-- APIs, but any additional types need to be manually generated.
addSwitch :: Switches.Switch -> Settings -> Settings
addSwitch = over psBridgeSwitches . flip mappend

apiToList ::
  forall bridgeSelector api.
  ( HasForeign (PureScript bridgeSelector) PSType api,
    GenerateList PSType (Foreign PSType api),
    HasBridge bridgeSelector
  ) =>
  Proxy api ->
  Proxy bridgeSelector ->
  [Req PSType]
apiToList _ _ = listFromAPI (Proxy :: Proxy (PureScript bridgeSelector)) (Proxy :: Proxy PSType) (Proxy :: Proxy api)

-- | Transform a given identifer to be a valid PureScript variable name (hopefully).
toPSVarName :: Text -> Text
toPSVarName = dropInvalid . unTitle . doPrefix . replaceInvalid
  where
    unTitle = uncurry mappend . first T.toLower . T.splitAt 1
    doPrefix t =
      let s = T.head t
          cond = isAlpha s || s == '_'
       in if cond then t else "_" <> t
    replaceInvalid = T.replace "-" "_"
    dropInvalid =
      let isValid c = isAlphaNum c || c == '_'
       in T.filter isValid

psTypedToUser :: PSType -> PSType
psTypedToUser response =
  TypeInfo
    { _typePackage = "purescript-subscriber",
      _typeModule = "Servant.Subscriber.Util",
      _typeName = "TypedToUser",
      _typeParameters = [response, psTypeParameterA]
    }

psSubscriptions :: PSType
psSubscriptions =
  TypeInfo
    { _typePackage = "purescript-subscriber",
      _typeModule = "Servant.Subscriber.Subscriptions",
      _typeName = "Subscriptions",
      _typeParameters = [psTypeParameterA]
    }

psTypeParameterA :: PSType
psTypeParameterA =
  TypeInfo
    { _typePackage = "",
      _typeModule = "",
      _typeName = "a",
      _typeParameters = []
    }

-- use servant-foreign's camelCaseL legacy version
jsCamelCaseL :: Getter FunctionName Text
jsCamelCaseL = _FunctionName . to (convert . map (T.replace "-" ""))
  where
    convert [] = ""
    convert (p : ps) = mconcat $ p : map capitalize ps
    capitalize "" = ""
    capitalize name = toUpper (T.head name) `T.cons` T.tail name
