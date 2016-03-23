{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Bridge (
    bridgeSumType
  , defaultBridge
  , module Bridge
  , writePSTypes
 ) where


import qualified Data.Text as T

import Language.PureScript.Bridge.SumType as Bridge
import Language.PureScript.Bridge.TypeInfo as Bridge
import Language.PureScript.Bridge.Tuple as Bridge
import Language.PureScript.Bridge.Primitives as Bridge
import Language.PureScript.Bridge.Printer as Bridge



import Control.Applicative
import qualified Data.Map as M
import Data.Maybe

-- | Your entry point to this library and quite likely all you will need.
-- | Make sure all your types derive Generic and Typeable.
-- | Typeable is not needed from ghc-7.10 on.
-- | Then call 'writePSTypes' like this:
-- | @
-- |   let myTypes = [
-- |        'toSumType' ('Proxy' :: 'Proxy' MyType1)
-- |      , 'toSumType' ('Proxy' :: 'Proxy' MyType2)
-- |     ]
-- |   'writePSTypes' 'defaultBridge' "path/to/you/purescript/project" myTypes
-- | @
-- | You can add new type mappings, like this:
-- | @
-- |   myBridge = 'defaultBridge' <|> mySpecialTypeBridge
-- | @
-- | Find examples for implementing your own type bridges in: 'Language.PureScript.Bridge.Primitives'
-- | A real world use case of this library can be found <https://github.com/gonimo/gonimo-back/blob/master/src/MkFrontendTypes.hs here>.
-- | Last but not least:
-- | WARNING: This function overwrites files - make backups or use version control!
writePSTypes :: TypeBridge -> FilePath -> [SumType] -> IO ()
writePSTypes br root sts = do
    let bridged = map (bridgeSumType br) sts
    let modules = M.elems $ sumTypesToModules M.empty bridged
    mapM_ (printModule root) modules

bridgeSumType :: TypeBridge -> SumType -> SumType
bridgeSumType br (SumType t cs) = SumType t $ map (bridgeConstructor br) cs

{--
 -- Optimistically and recursively translate types: If the passed TypeBridge returns Nothing,
 -- then the original TypeInfo is returned with the typePackage field cleared.
 -- You don't need to call this function directly, just use bridgeSumType with your TypeBridge
--}
doBridge :: TypeBridge -> TypeInfo -> TypeInfo
doBridge br info = let
    translated = info { typePackage = "" }
    res = fixTypeParameters $ fromMaybe translated (br info)
  in
    res {
      typeParameters = map (doBridge br) . typeParameters $ res
    }

-- | Default bridge for mapping primitive/common types:
-- | You can append your own bridges like this:
-- | defaultBridge <|> myBridge1 <|> myBridge2
defaultBridge :: TypeBridge
defaultBridge t = stringBridge t
  <|> listBridge t
  <|> maybeBridge t
  <|> eitherBridge t
  <|> boolBridge t
  <|> intBridge t
  <|> tupleBridge t

bridgeConstructor :: TypeBridge -> DataConstructor -> DataConstructor
bridgeConstructor br (DataConstructor name (Left infos)) =
    DataConstructor name . Left $ map (doBridge br) infos
bridgeConstructor br (DataConstructor name (Right record)) =
    DataConstructor name . Right $ map (bridgeRecordEntry br) record

bridgeRecordEntry :: TypeBridge -> RecordEntry -> RecordEntry
bridgeRecordEntry br (RecordEntry label value) = RecordEntry label $ doBridge br value

-- | Translate types that come from any module named "Something.TypeParameters" to lower case:
-- | Also drop the 1 at the end if present
fixTypeParameters :: TypeInfo -> TypeInfo
fixTypeParameters t
  | T.isSuffixOf "TypeParameters" (typeModule t) = t {
      typePackage = "" -- Don't suggest any packages
    , typeModule = "" -- Don't import any modules
    , typeName = stripNum . T.toLower $ typeName t
    }
  | otherwise = t
  where
    stripNum v = fromMaybe v (T.stripSuffix "1" v)
