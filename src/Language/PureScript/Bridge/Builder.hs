{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | A bridge builder DSL, powered by 'Monad', 'Alternative' and lens.
--
--   Bridges can be built within the 'BridgeBuilder' monad.
--   You can check properties of the to-be-bridged 'HaskellType' with '^==' or 'doCheck',
--   you have choice ('<|>'), you can fail ('empty') and you can return a translated
--   'PSType' ('return'). The 'HaskellType' can be accessed with:
--
-- > view haskType
--
--   Find usage examples in "Language.PureScript.Bridge.Primitives" and "Language.PureScript.Bridge.PSTypes"
module Language.PureScript.Bridge.Builder
  ( BridgeBuilder
  , BridgePart
  , FixUpBuilder
  , FixUpBridge
  , BridgeData
  , fullBridge
  , (^==)
  , doCheck
  , (<|>)
  , psTypeParameters
  , FullBridge
  , buildBridge
  , clearPackageFixUp
  , errorFixUp
  , buildBridgeWithCustomFixUp
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad (MonadPlus, guard, mplus, mzero)
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (Reader, ReaderT(..), runReader)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Language.PureScript.Bridge.TypeInfo

newtype BridgeBuilder a =
  BridgeBuilder (ReaderT BridgeData Maybe a)
  deriving (Functor, Applicative, Monad, MonadReader BridgeData)

type BridgePart = BridgeBuilder PSType

-- | Bridges to use when a 'BridgePart' returns 'Nothing' (See 'buildBridgeWithCustomFixUp').
--
--   It is similar to BridgeBuilder but does not offer choice or failure. It is used for constructing fallbacks
--   if a 'BridgePart' evaluates to 'Nothing'.
--
--   For type definitions you should use the more generic ('MonadReader' 'BridgeData' m) constraint. This way your code will work
--   in both 'FixUpBuilder' and 'BridgeBuilder':
--
-- > {-# LANGUAGE FlexibleContexts #-}
-- >
-- > import           Control.Monad.Reader.Class
-- > import           Language.PureScript.Bridge.TypeInfo
-- >
-- > psEither :: MonadReader BridgeData m => m PSType
-- > psEither = ....
--
--   instead of:
--
-- > psEither :: BridgePart
-- > psEither = ....
--
--   or
--
-- > psEither :: FixUpBridge
-- > psEither = ....
--
newtype FixUpBuilder a =
  FixUpBuilder (Reader BridgeData a)
  deriving (Functor, Applicative, Monad, MonadReader BridgeData)

type FixUpBridge = FixUpBuilder PSType

type FullBridge = HaskellType -> PSType

data BridgeData =
  BridgeData
    { _haskType :: HaskellType -- ^ The Haskell type to translate.
    , _fullBridge :: FullBridge -- ^ Reference to the bridge itself, needed for translation of type constructors.
    }

-- | By implementing the 'haskType' lens in the HasHaskType class, we are able
--   to use it for both 'BridgeData' and a plain 'HaskellType', therefore
--   you can use it with 'doCheck' and '^==' for checks on the complete 'HaskellType'
--   value.
--
--   Example:
--
-- > stringBridge :: BridgePart
-- > stringBridge = do
-- >   -- Note: we are using the HaskellType instance here:
-- >   haskType ^== mkTypeInfo (Proxy :: Proxy String)
-- >   return psString
instance HasHaskType BridgeData where
  haskType inj (BridgeData iT fB) = flip BridgeData fB <$> inj iT

-- | Lens for access to the complete bridge from within our Reader monad.
--
--   This is used for example for implementing 'psTypeParameters'.
fullBridge :: Lens' BridgeData FullBridge
fullBridge inj (BridgeData iT fB) = BridgeData iT <$> inj fB

-- | Bridge to PureScript by simply clearing out the '_typePackage' field.
--   This bridge is used by default as 'FixUpBridge' by 'buildBridge':
--
-- > buildBridge = buildBridgeWithCustomFixUp clearPackageFixUp
--
--   Thus, if no bridge matches a type, it gets optimistically translated to a PureScript type
--   which is idential to the Haskell type. Only the '_typePackage' field gets cleared,
--   as it is very unlikely that the PureScript package is called the same as the Haskell package.
--
--   Alternatively, if you are not that optimistic, you can use errorFixUp
--   - which simply calls 'error' when used.
--
-- > buildBridgeWithCustomFixUp errorFixUp yourBridge
--
--   Of course you can also write your own 'FixUpBridge'. It works the same
--   as for 'BridgePart', but you can not have choice ('<|>') or failure ('empty').
clearPackageFixUp :: MonadReader BridgeData m => m PSType
clearPackageFixUp = do
  input <- view haskType
  psArgs <- psTypeParameters
  return
    TypeInfo
      { _typePackage = ""
      , _typeModule = input ^. typeModule
      , _typeName = input ^. typeName
      , _typeParameters = psArgs
      }

-- | A 'FixUpBridge' which calles 'error' when used.
--   Usage:
--
-- > buildBridgeWithCustomFixUp errorFixUp yourBridge
errorFixUp :: MonadReader BridgeData m => m PSType
errorFixUp = do
  inType <- view haskType
  let message =
        "No translation supplied for Haskell type: '" <> inType ^. typeName <>
        "', from module: '" <>
        inType ^.
        typeModule <>
        "', from package: '" <>
        inType ^.
        typePackage <>
        "'!"
  return $ error $ T.unpack message

-- | Build a bridge.
--
--   This is a convenience wrapper for 'buildBridgeWithCustomFixUp' and should normally be sufficient.
--
--   Definition:
--
-- > buildBridgeWithCustomFixUp clearPackageFixUp
buildBridge :: BridgePart -> FullBridge
buildBridge = buildBridgeWithCustomFixUp clearPackageFixUp

-- | Takes a constructed BridgePart and makes it a total function ('FullBridge')
--   by using the supplied 'FixUpBridge' when 'BridgePart' returns 'Nothing'.
buildBridgeWithCustomFixUp :: FixUpBridge -> BridgePart -> FullBridge
buildBridgeWithCustomFixUp (FixUpBuilder fixUp) (BridgeBuilder bridgePart) =
  let mayBridge :: HaskellType -> Maybe PSType
      mayBridge inType = runReaderT bridgePart $ BridgeData inType bridge
      fixBridge inType = runReader fixUp $ BridgeData inType bridge
      bridge inType =
        fixTypeParameters $ fromMaybe (fixBridge inType) (mayBridge inType)
   in bridge

-- | Translate types that come from any module named "Something.TypeParameters" to lower case:
--
--   Also drop the 1 at the end if present.
--   This method gets called by 'buildBridge' and buildBridgeWithCustomFixUp for you - you should not need to call it.
--
--   It enables you to even bridge type constructor definitions, see "Language.PureScript.Bridge.TypeParameters" for more details.
fixTypeParameters :: TypeInfo lang -> TypeInfo lang
fixTypeParameters t =
  if "TypeParameters" `T.isSuffixOf` _typeModule t
    then t
           { _typePackage = "" -- Don't suggest any packages
           , _typeModule = "" -- Don't import any modules
           , _typeName = t ^. typeName . to (stripNum . T.toLower)
           }
    else t
  where
    stripNum v = fromMaybe v (T.stripSuffix "1" v)

-- | Alternative instance for BridgeBuilder so you can construct bridges with '<|>',
--   which behaves like a logical 'or' ('||'). If the left-hand side results in Nothing
--   the right-hand side is used, otherwise the left-hand side.
--   For usage examples see "Language.PureScript.Bridge.Primitives".
instance Alternative BridgeBuilder where
  empty = BridgeBuilder . ReaderT $ const Nothing
  BridgeBuilder a <|> BridgeBuilder b =
    BridgeBuilder . ReaderT $ \bridgeData ->
      let ia = runReaderT a bridgeData
          ib = runReaderT b bridgeData
       in ia <|> ib

instance MonadPlus BridgeBuilder where
  mzero = empty
  mplus = (<|>)

-- | Do some check on properties of 'haskType'.
doCheck :: Getter HaskellType a -> (a -> Bool) -> BridgeBuilder ()
doCheck l check = guard =<< views (haskType . l) check

-- | Check parts of 'haskType' for equality:
--
-- > textBridge :: BridgePart
-- > textBridge = do
-- >   typeName ^== "Text"
-- >   typeModule ^== "Data.Text.Internal" <|> typeModule ^== "Data.Text.Internal.Lazy"
-- >   return psString
(^==) :: Eq a => Getter HaskellType a -> a -> BridgeBuilder ()
l ^== a = doCheck l (== a)

infix 4 ^==

-- | Bridge 'haskType' 'typeParameters' over to PureScript types.
--
--   To be used for bridging type constructors.
psTypeParameters :: MonadReader BridgeData m => m [PSType]
psTypeParameters = map <$> view fullBridge <*> view (haskType . typeParameters)
