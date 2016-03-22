{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Bridge.Printer where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import Control.Monad


import Language.PureScript.Bridge.SumType
import Language.PureScript.Bridge.TypeInfo


data PSModule = PSModule {
  psModuleName :: !Text
, psImportLines :: !(Map Text ImportLine)
, psTypes :: ![SumType]
} deriving Show

data ImportLine = ImportLine {
  importModule :: !Text
, importTypes :: !(Set Text)
} deriving Show

type Modules = Map Text PSModule
type ImportLines = Map Text ImportLine

printModule :: FilePath -> PSModule -> IO ()
printModule root m = do
  unlessM (doesDirectoryExist mDir) $ createDirectoryIfMissing True mDir
  T.writeFile mPath . moduleToText $ m
  where
    mFile = (joinPath . map T.unpack . T.splitOn "." $ psModuleName m) <> ".purs"
    mPath = root </> mFile
    mDir = takeDirectory mPath

moduleToText :: PSModule -> Text
moduleToText m = T.unlines $
  "module " <> psModuleName m <> " where\n"
  : map importLineToText (M.elems (psImportLines m))
  ++ [ "\n\n" ]
  ++ map sumTypeToText (psTypes m)


importLineToText :: ImportLine -> Text
importLineToText l = "import " <> importModule l <> " (" <> typeList <> ")"
  where
    typeList = T.intercalate ", " (S.toList (importTypes l))

sumTypeToText :: SumType -> Text
sumTypeToText (SumType t cs) = T.unlines $
    "data " <> typeName t <> "="
  : [ "    " <> T.intercalate "\n  | " (map (constructorToText 4) cs) ]

constructorToText :: Int -> DataConstructor -> Text
constructorToText _ (DataConstructor n (Left ts))  = n <> " " <> T.intercalate " " (map typeInfoToText ts)
constructorToText indentation (DataConstructor n (Right rs)) = T.unlines $
      n <> " {"
    : [spaces (indentation + 2) <> T.intercalate intercalation (map recordEntryToText rs)]
    ++ [ spaces indentation <> "}" ]
  where
    intercalation = "\n" <> spaces indentation <> "," <> " "
    spaces c = T.replicate c " "

recordEntryToText :: RecordEntry -> Text
recordEntryToText e = recLabel e <> " :: " <> typeInfoToText (recValue e)


typeInfoToText :: TypeInfo -> Text
typeInfoToText t = if length textParameters > 1 then "(" <> inner <> ")" else inner
  where
    inner = typeName t <> T.intercalate " " textParameters
    textParameters = map typeInfoToText (typeParameters t)

sumTypesToModules :: Modules -> [SumType] -> Modules
sumTypesToModules = foldr sumTypeToModule

sumTypeToModule :: SumType -> Modules -> Modules
sumTypeToModule st@(SumType t _) = M.alter (Just. updateModule) (typeModule t)
  where
    updateModule Nothing = PSModule {
          psModuleName = typeModule t
        , psImportLines = dropSelf $ typesToImportLines M.empty (getUsedTypes st)
        , psTypes = [st]
        }
    updateModule (Just m) = m {
        psImportLines = dropSelf $ typesToImportLines (psImportLines m) (getUsedTypes st)
      , psTypes = st : psTypes m
      }
    dropSelf = M.delete (typeModule t)

typesToImportLines :: ImportLines -> [TypeInfo] -> ImportLines
typesToImportLines = foldr typeToImportLines

typeToImportLines :: TypeInfo -> ImportLines -> ImportLines
typeToImportLines t = M.alter (Just . updateLine) (typeModule t)
  where
    updateLine Nothing = ImportLine (typeModule t) (S.singleton (typeName t))
    updateLine (Just (ImportLine m types)) = ImportLine m $ S.insert (typeName t) types

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mbool action = mbool >>= flip unless action
