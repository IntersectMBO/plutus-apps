{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Bridge.Printer where

import           Control.Lens                               (filtered, to,
                                                             traversed, (^.),
                                                             (^..), (^?),
                                                             _Right, _head)
import           Control.Monad                              (unless)
import           Data.Map.Strict                            (Map)
import qualified Data.Map.Strict                            as Map
import           Data.Maybe                                 (isJust)
import           Data.Set                                   (Set)
import qualified Data.Set                                   as Set
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import qualified Data.Text.IO                               as T
import qualified Language.PureScript.Bridge.CodeGenSwitches as Switches
import           Language.PureScript.Bridge.SumType         (DataConstructor (DataConstructor),
                                                             Instance (Eq, Eq1, Functor, Generic, GenericShow, Newtype, Ord, Json),
                                                             RecordEntry (RecordEntry),
                                                             SumType (SumType),
                                                             getUsedTypes,
                                                             nootype, recLabel,
                                                             recValue,
                                                             sigValues,
                                                             sumTypeConstructors,
                                                             sumTypeInfo,
                                                             _recLabel)
import           Language.PureScript.Bridge.TypeInfo        (Language (PureScript),
                                                             PSType, TypeInfo,
                                                             typeParameters,
                                                             _typeModule,
                                                             _typeName,
                                                             _typePackage,
                                                             _typeParameters)

import           System.Directory                           (createDirectoryIfMissing,
                                                             doesDirectoryExist)
import           System.FilePath                            (joinPath,
                                                             takeDirectory,
                                                             (</>))
import           Text.PrettyPrint.Leijen.Text               (Doc, cat,
                                                             comma,
                                                             displayTStrict,
                                                             encloseSep, hcat,
                                                             hsep, indent,
                                                             lbrace, line,
                                                             linebreak, lparen,
                                                             parens, punctuate,
                                                             rbrace,
                                                             renderPretty,
                                                             rparen, space,
                                                             textStrict, vsep,
                                                             (<+>))

renderText :: Doc -> Text
renderText = displayTStrict . renderPretty 0.4 200

data Module (lang :: Language) =
  PSModule
    { psModuleName  :: !Text
    , psImportLines :: !ImportLines
    , psTypes       :: ![SumType lang]
    }
  deriving (Show)

type PSModule = Module 'PureScript

data ImportLine =
  ImportLine
    { importModule :: !Text
    , importTypes  :: !(Set Text)
    }
  deriving (Show)

type Modules = Map Text PSModule

type ImportLines = Map Text ImportLine

printModule :: Switches.Settings -> FilePath -> PSModule -> IO ()
printModule settings root m = do
  unlessM (doesDirectoryExist mDir) $ createDirectoryIfMissing True mDir
  T.writeFile mPath . moduleToText settings $ m
  where
    mFile =
      (joinPath . map T.unpack . T.splitOn "." $ psModuleName m) <> ".purs"
    mPath = root </> mFile
    mDir = takeDirectory mPath

sumTypesToNeededPackages :: [SumType lang] -> Set Text
sumTypesToNeededPackages = Set.unions . map sumTypeToNeededPackages

sumTypeToNeededPackages :: SumType lang -> Set Text
sumTypeToNeededPackages st =
  Set.filter (not . T.null) . Set.map _typePackage $ getUsedTypes st

moduleToText :: Switches.Settings -> Module 'PureScript -> Text
moduleToText settings m =
  T.unlines $ "-- File auto generated by purescript-bridge! --" : "module " <>
  psModuleName m <>
  " where\n" :
  "import Prelude" :
  (importLineToText <$> allImports) <>
  [""] <>
  (renderText . sumTypeToDoc settings <$> psTypes m)
  where
    otherImports =
      importsFromList
        (_lensImports settings <> _genericsImports <> _argonautImports settings)
    allImports = Map.elems $ mergeImportLines otherImports (psImportLines m)

_genericsImports :: [ImportLine]
_genericsImports =
    [ ImportLine "Data.Generic.Rep" $ Set.singleton "class Generic" ]

_lensImports :: Switches.Settings -> [ImportLine]
_lensImports settings
  | Switches.generateLenses settings =
    [ ImportLine "Data.Maybe" $ Set.fromList ["Maybe(..)"]
    , ImportLine "Data.Lens" $
      Set.fromList ["Iso'", "Prism'", "Lens'", "prism'", "lens"]
    , ImportLine "Data.Lens.Record" $ Set.fromList ["prop"]
    , ImportLine "Data.Lens.Iso.Newtype" $ Set.fromList ["_Newtype"]
    , ImportLine "Data.Symbol" $ Set.fromList ["SProxy(SProxy)"]
    ]
  | otherwise =
    [ ImportLine "Data.Maybe" $ Set.fromList ["Maybe(..)"] ]

_argonautImports :: Switches.Settings -> [ImportLine]
_argonautImports settings
  | (isJust . Switches.generateArgonaut) settings =
    [ ImportLine "Data.Argonaut.Decode.Generic" $ Set.singleton "genericDecodeJsonWith"
    , ImportLine "Data.Argonaut.Encode.Generic" $ Set.singleton "genericEncodeJsonWith"
    , ImportLine "Data.Argonaut.Types.Generic" $ Set.singleton "defaultEncoding"
    ]
  | otherwise = []

importLineToText :: ImportLine -> Text
importLineToText l = "import " <> importModule l <> " (" <> typeList <> ")"
  where
    typeList = T.intercalate ", " (Set.toList (importTypes l))

sumTypeToDoc :: Switches.Settings -> SumType 'PureScript -> Doc
sumTypeToDoc settings st = vsep $ punctuate line [sumTypeToTypeDecls settings st, additionalCode]
  where
    additionalCode =
      if Switches.generateLenses settings
        then lenses
        else dashes
    lenses = vsep $ punctuate line [dashes, sumTypeToOptics st, dashes]
    dashes = textStrict (T.replicate 80 "-")

sumTypeToTypeDecls :: Switches.Settings -> SumType 'PureScript -> Doc
sumTypeToTypeDecls settings (SumType t cs is) =
  vsep $ punctuate line $
    (dataOrNewtype <+> typeInfoToDoc True t
    <> line
    <> indent
        2
        (encloseVsep
            ("=" <> space)
            mempty
            ("|" <> space)
            (constructorToDoc <$> cs))
    ) : instances (SumType t cs (filter genArgonaut is))
  where
    dataOrNewtype =
      if isJust (nootype cs)
        then "newtype"
        else "data"
    genArgonaut Json = (isJust . Switches.generateArgonaut) settings
    genArgonaut _      = True

-- | Given a Purescript type, generate instances for typeclass
-- instances it claims to have.
instances :: SumType 'PureScript -> [Doc]
instances st@(SumType t _ is) = go <$> is
  where
    stpLength = length sumTypeParameters
    sumTypeParameters = filter (isTypeParam t) . Set.toList $ getUsedTypes st
    extras instanceConstraints
      | stpLength == 0 = mempty
      | otherwise =
        constraintsInner (instanceConstraints <$> sumTypeParameters) <+> "=>"
    name = textStrict (_typeName t)
    -- isEnum = all isNoArgConstructor cs
    -- isNoArgConstructor c = (c ^. sigValues) == Left []
    go :: Instance -> Doc
    go Json =
      "instance encodeJson" <> name <+> "::" <+> extras encodeJsonInstance <+> "EncodeJson" <+>
      typeInfoToDoc False t <+>
      "where" <>
      linebreak <>
      vsep
        [ indent 2 "encodeJson ="
        , indent 4 "genericEncodeJsonWith"
        , indent 6 "defaultEncoding { valuesKey = \"contents\", unwrapSingleArguments = true }"
        ] <>
      linebreak <>
      "instance decodeJson" <> name <+> "::" <+> extras decodeJsonInstance <+> "DecodeJson" <+>
      typeInfoToDoc False t <+>
      "where" <>
      linebreak <>
      vsep
        [ indent 2 "decodeJson ="
        , indent 4 "genericDecodeJsonWith"
        , indent 6 "defaultEncoding { valuesKey = \"contents\", unwrapSingleArguments = true }"
        ]
    go GenericShow =
      "instance show" <> name <+> "::" <+> extras showInstance <+> "Show" <+>
      typeInfoToDoc False t <+>
      "where" <>
      linebreak <>
      indent 2 "show x = genericShow x"
    go Functor =
      "derive instance functor" <> name <+> "::" <+> "Functor" <+> name
    go Eq =
      "derive instance eq" <> name <+> "::" <+> extras eqInstance <+> "Eq" <+>
      typeInfoToDoc False t
    go Eq1 = "derive instance eq1" <> name <+> "::" <+> "Eq1" <+> name
    go Ord =
      "derive instance ord" <> name <+> "::" <+> extras ordInstance <+> "Ord" <+>
      typeInfoToDoc False t
    go i =
      "derive instance " <> textStrict (T.toLower c) <> name <+> "::" <+>
      textStrict c <+>
      typeInfoToDoc False t <>
      postfix i
      where
        c = T.pack $ show i
        postfix Newtype = " _"
        postfix Generic = " _"
        postfix _ = ""

constraintsInner :: [Doc] -> Doc
constraintsInner = encloseSep lparen rparen ("," <> space)

isTypeParam :: PSType -> PSType -> Bool
isTypeParam t typ = _typeName typ `elem` map _typeName (_typeParameters t)

eqInstance :: PSType -> Doc
eqInstance params = "Eq" <+> typeInfoToDoc False params

ordInstance :: PSType -> Doc
ordInstance params = "Ord" <+> typeInfoToDoc False params

showInstance :: PSType -> Doc
showInstance params = "Show" <+> typeInfoToDoc False params

decodeJsonInstance :: PSType -> Doc
decodeJsonInstance params = "DecodeJson" <+> typeInfoToDoc False params

encodeJsonInstance :: PSType -> Doc
encodeJsonInstance params = "EncodeJson" <+> typeInfoToDoc False params

genericInstance :: PSType -> Doc
genericInstance params =
  "Generic" <+> typeInfoToDoc False params <+> "r" <> mergedTypeInfoToDoc params

sumTypeToOptics :: SumType 'PureScript -> Doc
sumTypeToOptics st =
  vsep $ punctuate line $ constructorOptics st <> recordOptics st

constructorOptics :: SumType 'PureScript -> [Doc]
constructorOptics st =
  case st ^. sumTypeConstructors of
    []  -> [] -- No work required.
    [c] -> [constructorToOptic False typeInfo c]
    cs  -> constructorToOptic True typeInfo <$> cs
  where
    typeInfo = st ^. sumTypeInfo

recordOptics :: SumType 'PureScript -> [Doc]
-- Match on SumTypes with a single DataConstructor (that's a list of a single element)
recordOptics st@(SumType _ [_] _) = recordEntryToLens st <$> dcRecords
  where
    cs = st ^. sumTypeConstructors
    dcRecords =
      lensableConstructor ^.. traversed . sigValues . _Right . traverse .
      filtered hasUnderscore
    hasUnderscore e = e ^. recLabel . to (T.isPrefixOf "_")
    lensableConstructor = filter singleRecordCons cs ^? _head
    singleRecordCons (DataConstructor _ (Right _)) = True
    singleRecordCons _                             = False
recordOptics _ = mempty

constructorToDoc :: DataConstructor 'PureScript -> Doc
constructorToDoc (DataConstructor n (Left [])) = textStrict n
constructorToDoc (DataConstructor n (Left ts)) =
  textStrict n <+> hsep (typeInfoToDoc False <$> ts)
constructorToDoc (DataConstructor n (Right rs)) =
  textStrict n <> line <> indent 4 (recordFields (recordEntryToDoc <$> rs))

recordFields :: [Doc] -> Doc
recordFields = encloseVsep (lbrace <> space) (line <> rbrace) (comma <> space)

encloseVsep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseVsep left right sp ds =
  case ds of
    []  -> left <> right
    [d] -> left <> d <> right
    _   -> vsep (zipWith (<>) (left : repeat sp) ds) <> right

typeNameAndForall :: TypeInfo 'PureScript -> (Doc, Doc)
typeNameAndForall typeInfo = (typName, forAll)
  where
    typName = typeInfoToDoc False typeInfo
    forAllParams =
      typeInfo ^.. typeParameters . traversed . to (typeInfoToDoc False)
    forAll =
      " :: " <>
      case forAllParams of
        [] -> mempty
        cs -> "forall" <+> hsep cs <> ". "

fromEntries :: (RecordEntry a -> Doc) -> [RecordEntry a] -> Doc
fromEntries mkElem rs =
  encloseSep (lbrace <> space) (space <> rbrace) ("," <> space) (mkElem <$> rs)

mkFnArgs :: [RecordEntry 'PureScript] -> Doc
mkFnArgs [r] = textStrict $ r ^. recLabel
mkFnArgs rs =
  fromEntries
    (\recE ->
       textStrict (recE ^. recLabel) <> ":" <+> textStrict (recE ^. recLabel))
    rs

mkTypeSig :: [RecordEntry 'PureScript] -> Doc
mkTypeSig []  = "Unit"
mkTypeSig [r] = typeInfoToDoc False $ r ^. recValue
mkTypeSig rs  = fromEntries recordEntryToDoc rs

constructorToOptic ::
     Bool -> TypeInfo 'PureScript -> DataConstructor 'PureScript -> Doc
constructorToOptic hasOtherConstructors typeInfo (DataConstructor n args) =
  case (args, hasOtherConstructors) of
    (Left [c], False) ->
      vsep
        [ pName <> forAll <> "Iso'" <+> typName <+>
          mkTypeSig (constructorTypes [c])
        , pName <+> "= _Newtype"
        ]
    (Left cs, _) ->
      vsep
        [ pName <> forAll <> "Prism'" <+> typName <+> mkTypeSig types
        , pName <+> "= prism'" <+> getter <+> "f" <> line <>
          indent
            2
            ("where" <> linebreak <>
             indent 2 (vsep ["f" <+> mkF cs, otherConstructorFallThrough]))
        ]
      where mkF [] = textStrict n <+> "= Just unit"
            mkF _ =
              parens
                (textStrict n <> space <>
                 textStrict (T.unwords (_recLabel <$> types))) <+>
              "= Just $" <+>
              mkFnArgs types
            getter :: Doc
            getter
              | null cs = parens ("\\_ ->" <+> textStrict n)
              | length cs == 1 = textStrict n
              | otherwise =
                parens
                  ("\\{" <+> cat (punctuate ", " cArgs) <+> "} ->" <+>
                   textStrict n <+>
                   cat (punctuate space cArgs))
              where
                cArgs = textStrict . T.singleton . fst <$> zip ['a' ..] cs
            types = constructorTypes cs
    (Right rs, False) ->
      vsep
        [ pName <> forAll <> "Iso'" <+> typName <+>
          fromEntries recordEntryToDoc rs
        , pName <+> "= _Newtype"
        ]
    (Right rs, True) ->
      vsep
        [ pName <> forAll <> "Prism'" <+> typName <+>
          fromEntries recordEntryToDoc rs
        , pName <+> "= prism'" <+> textStrict n <+> "f" <> line <>
          indent
            2
            ("where" <> linebreak <>
             indent
               2
               ("f (" <> textStrict n <+> "r) = Just r" <> line <>
                otherConstructorFallThrough))
        ]
  where
    constructorTypes cs =
      [RecordEntry (T.singleton label) t | (label, t) <- zip ['a' ..] cs]
    (typName, forAll) = typeNameAndForall typeInfo
    pName = "_" <> textStrict n
    otherConstructorFallThrough
      | hasOtherConstructors = "f _ = Nothing"
      | otherwise = mempty

recordEntryToLens :: SumType 'PureScript -> RecordEntry 'PureScript -> Doc
recordEntryToLens st e =
  if hasUnderscore
    then vsep
           [ textStrict lensName <> forAll <> "Lens'" <+> typName <+> recType
           , textStrict lensName <+> "= _Newtype <<< prop" <+>
             parens ("SProxy :: SProxy \"" <> textStrict recName <> "\"")
           ]
    else mempty
  where
    (typName, forAll) = typeNameAndForall (st ^. sumTypeInfo)
    recName = e ^. recLabel
    lensName = T.drop 1 recName
    recType = typeInfoToDoc False (e ^. recValue)
    hasUnderscore = e ^. recLabel . to (T.isPrefixOf "_")

recordEntryToDoc :: RecordEntry 'PureScript -> Doc
recordEntryToDoc e =
  textStrict (_recLabel e) <+> "::" <+> typeInfoToDoc True (e ^. recValue)

typeInfoToText :: Bool -> PSType -> Text
typeInfoToText topLevel = renderText . typeInfoToDoc topLevel

typeInfoToDoc :: Bool -> PSType -> Doc
typeInfoToDoc topLevel t =
  if needParens
    then parens inner
    else inner
  where
    inner =
      if pLength > 0
        then textStrict (_typeName t) <+> hsep textParameters
        else textStrict (_typeName t)
    params = _typeParameters t
    pLength = length params
    needParens = not topLevel && pLength > 0
    textParameters = typeInfoToDoc False <$> params

mergedTypeInfoToDoc :: PSType -> Doc
mergedTypeInfoToDoc t = textStrict (_typeName t) <> hcat textParameters
  where
    params = _typeParameters t
    textParameters = mergedTypeInfoToDoc <$> params

sumTypesToModules :: [SumType 'PureScript] -> Modules
sumTypesToModules = foldr (Map.unionWith unionModules) Map.empty . fmap sumTypeToModule

unionModules :: PSModule -> PSModule -> PSModule
unionModules m1 m2 =
    m1
      { psImportLines = unionImportLines (psImportLines m1) (psImportLines m2)
      , psTypes = psTypes m1 <> psTypes m2
      }

sumTypeToModule :: SumType 'PureScript -> Modules
sumTypeToModule st@(SumType t _ is) =
  Map.singleton
    (_typeModule t)
    $ PSModule
      { psModuleName = _typeModule t
      , psImportLines =
          dropEmpty $
            dropPrelude $
              dropPrim $
                dropSelf $
                  unionImportLines
                    (typesToImportLines (getUsedTypes st))
                    (instancesToImportLines is)
      , psTypes = [st]
      }
  where
    dropEmpty = Map.delete ""
    dropPrelude = Map.delete "Prelude"
    dropPrim = Map.delete "Prim"
    dropSelf = Map.delete (_typeModule t)

unionImportLines :: ImportLines -> ImportLines -> ImportLines
unionImportLines = Map.unionWith unionImportLine

unionImportLine :: ImportLine -> ImportLine -> ImportLine
unionImportLine l1 l2 =
    l1 { importTypes = Set.union (importTypes l1) (importTypes l2) }

typesToImportLines :: Set PSType -> ImportLines
typesToImportLines =
  foldr unionImportLines Map.empty . fmap typeToImportLines . Set.toList

typeToImportLines :: PSType -> ImportLines
typeToImportLines t =
  unionImportLines (typesToImportLines $ Set.fromList (_typeParameters t)) $
    importsFromList [ImportLine (_typeModule t) (Set.singleton (_typeName t))]

instancesToImportLines :: [Instance] -> ImportLines
instancesToImportLines =
  foldr unionImportLines Map.empty . fmap instanceToImportLines

instanceToImportLines :: Instance -> ImportLines
instanceToImportLines GenericShow =
  importsFromList [ ImportLine "Data.Show.Generic" $ Set.singleton "genericShow" ]
instanceToImportLines Json =
  importsFromList
    [ ImportLine "Data.Argonaut.Decode.Generic" $ Set.singleton "genericDecodeJsonWith"
    , ImportLine "Data.Argonaut.Encode.Generic" $ Set.singleton "genericEncodeJsonWith"
    , ImportLine "Data.Argonaut.Types.Generic" $ Set.singleton "defaultEncoding"
    ]
instanceToImportLines _ = Map.empty

importsFromList :: [ImportLine] -> Map Text ImportLine
importsFromList ls =
  let pairs = zip (importModule <$> ls) ls
      merge a b =
        ImportLine (importModule a) (importTypes a `Set.union` importTypes b)
   in Map.fromListWith merge pairs

mergeImportLines :: ImportLines -> ImportLines -> ImportLines
mergeImportLines = Map.unionWith mergeLines
  where
    mergeLines a b =
      ImportLine (importModule a) (importTypes a `Set.union` importTypes b)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mbool action = mbool >>= flip unless action
