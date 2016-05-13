{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Bridge (
    bridgeSumType
  , defaultBridge
  , doBridge
  , module Bridge
  , writePSTypes
 ) where


import qualified Data.Text                             as T
import qualified Data.Text.IO                          as T


import           Language.PureScript.Bridge.Primitives as Bridge
import           Language.PureScript.Bridge.Printer    as Bridge
import           Language.PureScript.Bridge.SumType    as Bridge
import           Language.PureScript.Bridge.Tuple      as Bridge
import           Language.PureScript.Bridge.TypeInfo   as Bridge



import           Control.Applicative
import qualified Data.Map                              as M
import           Data.Maybe

-- | Your entry point to this library and quite likely all you will need.
--   Make sure all your types derive Generic and Typeable.
--   Typeable is not needed from ghc-7.10 on.
--
--   Then list all your types you want to use in PureScript and call 'writePSTypes':
--
--   >  let myTypes = [
--   >      mkSumType (Proxy :: Proxy MyType1)
--   >    , mkSumType (Proxy :: Proxy MyType2)
--   >   ]
--   >
--   >  writePSTypes defaultBridge "path/to/your/purescript/project" myTypes
--
--   You can define your own type bridges based on 'defaultBridge':
--
--
--  >  myBridge = defaultBridge <|> mySpecialTypeBridge
--
--  and use it with 'writePSTypes':
--
--  >  writePSTypes myBridge "path/to/your/purescript/project" myTypes
--
--   Find examples for implementing your own type bridges in: "Language.PureScript.Bridge.Primitives".
--
--  == Result:
--   'writePSTypes' will write out PureScript modules to the given path, mirroring the hierarchy of the Haskell modules
--   the types came from. In addition a list of needed PS packages is printed to the console.
--
--   The list of needed packages is retrieved from the bridged 'TypeInfo' data, so make sure you set '_typePackage' correctly
--   in your own bridges, in order for this feature to be useful.
--
--  == Real world usage example:
--   A real world use case of this library can be found <https://github.com/gonimo/gonimo-back/blob/master/src/MkFrontendTypes.hs here>.
--
--   With custom bridges defined <https://github.com/gonimo/gonimo-back/blob/master/src/Gonimo/TypeBridges.hs here> and
--   custom PS types defined <https://github.com/gonimo/gonimo-back/blob/master/src/Gonimo/PSTypes.hs here>.
--
--   Parts of the generated output can be found <https://github.com/gonimo/gonimo-front/blob/master/src/Gonimo/Types.purs here>.
--
--   Note how 'Secret' and 'Key'
--   get translated according to our custom rules, with correct imports and everything. Also the formatting is quite nice, I think - would you have guessed that this code was generated?
--
--  == /WARNING/:
--   This function overwrites files - make backups or use version control!
writePSTypes :: TypeBridge -> FilePath -> [SumType] -> IO ()
writePSTypes br root sts = do
    let bridged = map (bridgeSumType br) sts
    let modules = M.elems $ sumTypesToModules M.empty bridged
    mapM_ (printModule root) modules
    T.putStrLn "Successfully created your PureScript modules.\n"
    T.putStrLn "Make sure you have the following PureScript packages installed:\n"
    let packages = sumTypesToNeededPackages bridged
    mapM_ (T.putStrLn . mappend "  - ") packages

-- | Translate leaf types in a sum type to match PureScript types.
bridgeSumType :: TypeBridge -> SumType -> SumType
bridgeSumType br (SumType t cs) = SumType fixedT $ map (bridgeConstructor br) cs
  where
    fixedT= t { _typeParameters = map fixTypeParameters (_typeParameters t)}

-- | Translate types optimistically: If the passed 'TypeBridge' returns 'Nothing',
--   then the original 'TypeInfo' is returned with the '_typePackage' field cleared.
--
--   This function also recurses into all '_typeParameters' of the passed 'TypeInfo'.
--
--   You typically don't need to call this function directly, just use 'bridgeSumType' with your 'TypeBridge'.
doBridge :: TypeBridge -> TypeInfo -> TypeInfo
doBridge br info = let
    translated = info { _typePackage = "" }
    res = fixTypeParameters $ fromMaybe translated (br info)
  in
    res {
      _typeParameters = map (doBridge br) . _typeParameters $ res
    }

-- | Default bridge for mapping primitive/common types:
--   You can append your own bridges like this:
--
-- >  defaultBridge <|> myBridge1 <|> myBridge2
--
--   Find examples for bridge definitions in "Language.PureScript.Bridge.Primitives" and
--   "Language.PureScript.Bridge.Tuple".
defaultBridge :: TypeBridge
defaultBridge t = stringBridge t
  <|> listBridge t
  <|> maybeBridge t
  <|> eitherBridge t
  <|> boolBridge t
  <|> intBridge t
  <|> tupleBridge t

-- | Translate types in a constructor.
bridgeConstructor :: TypeBridge -> DataConstructor -> DataConstructor
bridgeConstructor br (DataConstructor name (Left infos)) =
    DataConstructor name . Left $ map (doBridge br) infos
bridgeConstructor br (DataConstructor name (Right record)) =
    DataConstructor name . Right $ map (bridgeRecordEntry br) record

-- | Translate types in a record entry.
bridgeRecordEntry :: TypeBridge -> RecordEntry -> RecordEntry
bridgeRecordEntry br (RecordEntry label value) = RecordEntry label $ doBridge br value

-- | Translate types that come from any module named "Something.TypeParameters" to lower case:
--   Also drop the 1 at the end if present
fixTypeParameters :: TypeInfo -> TypeInfo
fixTypeParameters t
  | T.isSuffixOf "TypeParameters" (_typeModule t) = t {
      _typePackage = "" -- Don't suggest any packages
    , _typeModule = "" -- Don't import any modules
    , _typeName = stripNum . T.toLower $ _typeName t
    }
  | otherwise = t
  where
    stripNum v = fromMaybe v (T.stripSuffix "1" v)
