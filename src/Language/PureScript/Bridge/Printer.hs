{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Language.PureScript.Bridge.Printer where

import           Control.Lens                               (to, traversed,(^.), (^..))
import           Control.Monad                              (unless)
import           Data.Map.Strict                            (Map)
import qualified Data.Map.Strict                            as Map
import           Data.Maybe                                 (isJust, catMaybes)
import           Data.Set                                   (Set)
import qualified Data.Set                                   as Set
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import qualified Data.Text.IO                               as T
import qualified Language.PureScript.Bridge.CodeGenSwitches as Switches
import           Language.PureScript.Bridge.SumType         (DataConstructor (..),
                                                             Instance (..),
                                                             RecordEntry (RecordEntry),
                                                             DataConstructorArgs (..),
                                                             SumType (SumType),
                                                             getUsedTypes,
                                                             nootype, recLabel,
                                                             recValue,
                                                             sumTypeConstructors,
                                                             sumTypeInfo,
                                                             _recLabel, sigConstructor)
import           Language.PureScript.Bridge.TypeInfo        (Language (PureScript),
                                                             PSType, TypeInfo (TypeInfo),
                                                             typeParameters,
                                                             _typeModule,
                                                             _typeName,
                                                             _typePackage,
                                                             _typeParameters, typeName, flattenTypeInfo)

import           System.Directory                           (createDirectoryIfMissing,
                                                             doesDirectoryExist)
import           System.FilePath                            (joinPath,
                                                             takeDirectory,
                                                             (</>))
import           Text.PrettyPrint.Leijen.Text               (Doc,
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
                                                             (<+>), hang, dquotes, braces, char, backslash)
import Data.List (unfoldr)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Char (isLower)
import Language.PureScript.Bridge.PSTypes (psUnit)

renderText :: Doc -> Text
renderText = displayTStrict . renderPretty 0.4 200

data Module (lang :: Language) =
  PSModule
    { psModuleName  :: !Text
    , psImportLines :: !ImportLines
    , psQualifiedImports :: !(Map Text Text)
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
  (uncurry qualifiedImportToText <$> Map.toList (psQualifiedImports m)) <>
  [""] <>
  (renderText . sumTypeToDoc settings <$> psTypes m)
  where
    otherImports =
      importsFromList
        (_lensImports settings <> _genericsImports)
    allImports = Map.elems $ mergeImportLines otherImports (psImportLines m)

_genericsImports :: [ImportLine]
_genericsImports =
    [ ImportLine "Data.Generic.Rep" $ Set.singleton "class Generic" ]

_lensImports :: Switches.Settings -> [ImportLine]
_lensImports settings
  | Switches.generateLenses settings =
    [ ImportLine "Data.Maybe" $ Set.fromList ["Maybe(..)"]
    , ImportLine "Data.Lens" $
      Set.fromList ["Iso'", "Prism'", "Lens'", "prism'", "lens", "iso"]
    , ImportLine "Data.Lens.Record" $ Set.fromList ["prop"]
    , ImportLine "Data.Lens.Iso.Newtype" $ Set.fromList ["_Newtype"]
    , ImportLine "Type.Proxy" $ Set.fromList ["Proxy(Proxy)"]
    ]
  | otherwise =
    [ ImportLine "Data.Maybe" $ Set.fromList ["Maybe(..)"] ]

qualifiedImportToText :: Text -> Text -> Text
qualifiedImportToText m q = "import " <> m <> " as " <> q

importLineToText :: ImportLine -> Text
importLineToText l = "import " <> importModule l <> " (" <> typeList <> ")"
  where
    typeList = T.intercalate ", " (Set.toList (importTypes l))

sumTypeToDoc :: Switches.Settings -> SumType 'PureScript -> Doc
sumTypeToDoc settings st = vsep $ punctuate line [sumTypeToTypeDecls st, additionalCode]
  where
    additionalCode =
      if Switches.generateLenses settings
        then lenses
        else dashes
    lenses = vsep $ punctuate line [dashes, sumTypeToOptics st, dashes]
    dashes = textStrict (T.replicate 80 "-")

sumTypeToTypeDecls :: SumType 'PureScript -> Doc
sumTypeToTypeDecls (SumType t cs is) =
  vsep $ punctuate line $
    (dataOrNewtype <+> typeInfoToDecl t
    <> line
    <> indent
        2
        (encloseVsep
            ("=" <> space)
            mempty
            ("|" <> space)
            (constructorToDoc <$> cs))
    ) : instances (SumType t cs is)
  where
    dataOrNewtype =
      if isJust (nootype cs)
        then "newtype"
        else "data"

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
    go :: Instance -> Doc
    go Bounded =
      hang 2 $ vsep
        [ "instance bounded" <> name <+> "::" <+> "Bounded" <+> typeInfoToDoc t <+> "where"
        , "bottom = genericBottom"
        , "top = genericTop"
        ]
    go Enum =
      hang 2 $ vsep
        [ "instance enum" <> name <+> "::" <+> "Enum" <+> typeInfoToDoc t <+> "where"
        , "succ = genericSucc"
        , "pred = genericPred"
        ]
    go Json =
      vsep
        [ hang 2 $ vsep
          [ "instance encodeJson" <> name <+> "::" <+> extras encodeJsonInstance <+> "EncodeJson" <+> typeInfoToDoc t <+> "where"
          , hang 2 ("encodeJson = E.encode" <+> sumTypeToEncode st)
          ]
        , linebreak
        , hang 2 $ vsep
          [ "instance decodeJson" <> name <+> "::" <+> extras decodeJsonInstance <+> "DecodeJson" <+> typeInfoToDoc t <+> "where"
          , hang 2 ("decodeJson = D.decode" <+> sumTypeToDecode st)
          ]
        ]
    go GenericShow =
      "instance show" <> name <+> "::" <+> extras showInstance <+> "Show" <+>
      typeInfoToDoc t <+>
      "where" <>
      linebreak <>
      indent 2 "show x = genericShow x"
    go Functor =
      "derive instance functor" <> name <+> "::" <+> "Functor" <+> name
    go Eq =
      "derive instance eq" <> name <+> "::" <+> extras eqInstance <+> "Eq" <+>
      typeInfoToDoc t
    go Eq1 = "derive instance eq1" <> name <+> "::" <+> "Eq1" <+> name
    go Ord =
      "derive instance ord" <> name <+> "::" <+> extras ordInstance <+> "Ord" <+>
      typeInfoToDoc t
    go i =
      "derive instance " <> textStrict (T.toLower c) <> name <+> "::" <+>
      textStrict c <+>
      typeInfoToDoc t <>
      postfix i
      where
        c = T.pack $ show i
        postfix Newtype = " _"
        postfix Generic = " _"
        postfix _ = ""

isEnum :: [DataConstructor lang] -> Bool
isEnum = all $ (== Nullary) . _sigValues

sumTypeToEncode :: SumType 'PureScript -> Doc
sumTypeToEncode (SumType _ cs _)
  | isEnum cs = "E.enum"
  | otherwise =
    line <> "$" <+> case cs of
      [dc@(DataConstructor _ args)] ->
        vsep
          [ if isJust (nootype [dc])
               then "unwrap"
               else parens $ "case _ of" <+> branch (constructorPattern dc) (constructorExpr args)
          , ">$<" <+> hang 2 (argsToEncode args)
          ]
      _ -> 
        vsep
          [ "E.sumType"
          , "$ toEither"
          , indent 4 $ ">$<" <+> hsep (punctuate (line <> ">|<") (constructorToTagged <$> cs))
          , "where"
          , "toEither =" <+> case_of (unfoldr toEither ("", cs))
          ]
  where
    toEither (_, []) = Nothing
    toEither (prefix, dc@(DataConstructor _ args) : rest) =
      Just
        ( ( constructorPattern dc
          , prefix <+> eitherCase rest <+> "$" <+> constructorExpr args
          )
        , (nextPrefix rest, rest)
        )
      where
        eitherCase [] = "Right"
        eitherCase _ = "Left"
        nextPrefix [_] = prefix
        nextPrefix _ = prefix <+> "Right $"
    constructorToTagged (DataConstructor name args) =
      "E.tagged" <+> dquotes (textStrict name) <+> argsToEncode args
    argsToEncode Nullary = "E.null"
    argsToEncode (Normal (t :| [])) = typeToEncode t
    argsToEncode (Normal ts) =
      parens $ "E.tuple" <+> parens (hsep $ punctuate " >/\\<" $ typeToEncode <$> NE.toList ts)
    argsToEncode (Record fields) = parens $ vsep
      [ "E.record"
      , braces $ vsep $ punctuate comma $ fieldToEncode <$> NE.toList fields
      ]
    fieldToEncode (RecordEntry name t) =
      textStrict name <> ":" <+> typeToEncode t <+> ":: Encoder" <+> typeInfoToDoc t

typeToEncode :: PSType -> Doc
typeToEncode (TypeInfo "purescript-prelude" "Prelude" "Unit" []) = "E.unit"
typeToEncode (TypeInfo "purescript-maybe" "Data.Maybe" "Maybe" [t]) = parens $
  "E.maybe" <+> typeToEncode t
typeToEncode (TypeInfo "purescript-either" "Data.Either" "Either" [l, r]) = parens $
  "E.either" <+> typeToEncode l <+> typeToEncode r
typeToEncode (TypeInfo "purescript-tuples" "Data.Tuple" "Tuple" ts) = parens $
  "E.tuple" <+> parens (hsep $ punctuate " >/\\<" $ typeToEncode <$> flattenTuple ts)
    where
      flattenTuple [] = []
      flattenTuple [a] = [a]
      flattenTuple [a, TypeInfo "purescript-tuples" "Data.Tuple" "Tuple" ts'] = a : flattenTuple ts'
      flattenTuple (h : t) = h : flattenTuple t
typeToEncode _ = "E.value"


sumTypeToDecode :: SumType 'PureScript -> Doc
sumTypeToDecode (SumType _ cs _)
  | isEnum cs = "D.enum"
sumTypeToDecode (SumType _ [c] _) = "$" <+> constructorToDecode c
sumTypeToDecode (SumType t cs _) = line <>
  vsep
    [ "$ D.sumType" <+> dquotes (t ^. typeName . to textStrict)
    , "$" <+> hang 2 (hsep $ punctuate (line <> "<|>") $ constructorToTagged <$> cs)
    ]
  where
    constructorToTagged dc =
      "D.tagged"
        <+> dc ^. sigConstructor . to textStrict . to dquotes 
        <+> dc ^. to constructorToDecode . to parens


constructorToDecode :: DataConstructor 'PureScript -> Doc
constructorToDecode (DataConstructor name Nullary) =
  textStrict name <+> "<$" <+> "D.null"
constructorToDecode (DataConstructor name (Normal (a :| []))) =
  textStrict name <+> "<$>" <+> typeToDecode a
constructorToDecode (DataConstructor name (Normal as)) =
  "D.tuple"
    <+> "$"
    <+> textStrict name
    <+> "</$\\>"
    <+> hsep (punctuate " </*\\>" $ typeToDecode <$> NE.toList as)
constructorToDecode (DataConstructor name (Record fields)) =
  vsep
  [ textStrict name <+> "<$> D.record" <+> dquotes (textStrict name)
  , braces $ vsep $ punctuate comma $ fieldToDecode <$> NE.toList fields
  ]
  where
    fieldToDecode (RecordEntry n t) =
      textStrict n <> ":" <+> typeToDecode t <+> ":: Decoder" <+> typeInfoToDoc t

typeToDecode :: PSType -> Doc
typeToDecode (TypeInfo "purescript-prelude" "Prelude" "Unit" []) = "D.unit"
typeToDecode (TypeInfo "purescript-maybe" "Data.Maybe" "Maybe" [t]) = parens $
  "D.maybe" <+> typeToDecode t
typeToDecode (TypeInfo "purescript-either" "Data.Either" "Either" [l, r]) = parens $
  "D.either" <+> typeToDecode l <+> typeToDecode r
typeToDecode (TypeInfo "purescript-tuples" "Data.Tuple" "Tuple" ts) = parens $
  "D.tuple" <+> parens (hsep $ punctuate " </\\>" $ typeToDecode <$> flattenTuple ts)
    where
      flattenTuple [] = []
      flattenTuple [a] = [a]
      flattenTuple [a, TypeInfo "purescript-tuples" "Data.Tuple" "Tuple" ts'] = a : flattenTuple ts'
      flattenTuple (h : t) = h : flattenTuple t
typeToDecode _ = "D.value"

constraintsInner :: [Doc] -> Doc
constraintsInner = encloseSep lparen rparen ("," <> space)

isTypeParam :: PSType -> PSType -> Bool
isTypeParam t typ = _typeName typ `elem` map _typeName (_typeParameters t)

eqInstance :: PSType -> Doc
eqInstance params = "Eq" <+> typeInfoToDoc params

ordInstance :: PSType -> Doc
ordInstance params = "Ord" <+> typeInfoToDoc params

showInstance :: PSType -> Doc
showInstance params = "Show" <+> typeInfoToDoc params

decodeJsonInstance :: PSType -> Doc
decodeJsonInstance params = "DecodeJson" <+> typeInfoToDoc params

encodeJsonInstance :: PSType -> Doc
encodeJsonInstance params = "EncodeJson" <+> typeInfoToDoc params

genericInstance :: PSType -> Doc
genericInstance params =
  "Generic" <+> typeInfoToDoc params <+> "r" <> mergedTypeInfoToDoc params

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
recordOptics st@(SumType _ [DataConstructor _ (Record fields)] _) =
  recordEntryToLens st <$> filter hasUnderscore (NE.toList fields)
recordOptics _ = mempty

hasUnderscore :: RecordEntry lang -> Bool
hasUnderscore (RecordEntry name _) = "_" `T.isPrefixOf` name

constructorToDoc :: DataConstructor 'PureScript -> Doc
constructorToDoc (DataConstructor n Nullary) = textStrict n
constructorToDoc (DataConstructor n (Normal ts)) =
  textStrict n <+> hsep (typeInfoToDoc <$> NE.toList ts)
constructorToDoc (DataConstructor n (Record rs)) =
  textStrict n <> line <> indent 4 (recordFields (recordEntryToDoc <$> NE.toList rs))

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
    typName = typeInfoToDoc typeInfo
    forAllParams =
      typeInfo ^.. typeParameters . traversed . to typeInfoToDoc
    forAll =
      " :: " <>
      case forAllParams of
        [] -> mempty
        cs -> "forall" <+> hsep cs <> ". "

constructorToOptic ::
     Bool -> TypeInfo 'PureScript -> DataConstructor 'PureScript -> Doc
constructorToOptic hasOtherConstructors typeInfo (DataConstructor n args) =
  case (args, hasOtherConstructors) of
    (Nullary, False) -> iso pName typeInfo psUnit "(const unit)" $ parens ("const" <+> cName)
    (Nullary, True) -> prism pName typeInfo psUnit cName "unit" $ parens ("const" <+> cName)
    (Normal (t :| []), False) -> newtypeIso pName typeInfo t
    (Normal (t :| []), True) -> prism pName typeInfo t (normalPattern n [t]) "a" cName
    (Normal ts, False) ->
      iso
        pName
        typeInfo
        (mkType (renderText $ recordType rec) [])
        (parens (lambda (normalPattern n ts) (recordExpr rec)))
        (parens (lambda (recordExpr rec) (normalPattern n ts)))
      where
        rec = argsToRecord ts
    (Normal ts, True) ->
      prism
        pName
        typeInfo
        (mkType (renderText $ recordType rec) [])
        (normalPattern n ts)
        (recordExpr rec)
        (parens (lambda (recordExpr rec) (normalPattern n ts)))
      where
        rec = argsToRecord ts
    (Record rs, False) -> newtypeIso pName typeInfo (mkType (renderText $ recordType rs) [])
    (Record rs, True) -> newtypeIso pName typeInfo (mkType (renderText $ recordType rs) [])
  where
    cName = textStrict n
    pName = "_" <> textStrict n
    recordType = braces . hsep . punctuate ", " . map recordFieldSig . NE.toList
    recordFieldSig (RecordEntry name t) = signature False (textStrict name) [] [] t

argsToRecord :: NonEmpty PSType -> NonEmpty (RecordEntry 'PureScript)
argsToRecord = fmap (uncurry RecordEntry) . NE.zip (T.singleton <$> ['a'..])

iso :: Doc -> PSType -> PSType -> Doc -> Doc -> Doc
iso name fromType toType fromMorph toMorph =
  def
    name
    []
    []
    (mkType "Iso'" [fromType, toType])
    ("iso" <+> fromMorph <+> toMorph)

prism :: Doc -> PSType -> PSType -> Doc -> Doc -> Doc -> Doc
prism name fromType toType previewPattern previewExpr inject =
  def
    name
    []
    []
    (mkType "Prism'" [fromType, toType])
    ( "prism'" <+> inject <+> case_of
        [ (previewPattern, "Just" <+> previewExpr)
        , ("_", "Nothing")
        ]
    )

newtypeIso :: Doc -> PSType -> PSType -> Doc
newtypeIso name fromType toType =
  def
    name
    []
    []
    (mkType "Iso'" [fromType, toType])
    "_Newtype"

recordEntryToLens :: SumType 'PureScript -> RecordEntry 'PureScript -> Doc
recordEntryToLens st e =
  if hasUnderscore e
    then vsep
           [ textStrict lensName <> forAll <> "Lens'" <+> typName <+> recType
           , textStrict lensName <+> "= _Newtype <<< prop" <+>
             parens ("Proxy :: _ \"" <> textStrict recName <> "\"")
           ]
    else mempty
  where
    (typName, forAll) = typeNameAndForall (st ^. sumTypeInfo)
    recName = e ^. recLabel
    lensName = T.drop 1 recName
    recType = typeInfoToDoc (e ^. recValue)

recordEntryToDoc :: RecordEntry 'PureScript -> Doc
recordEntryToDoc e =
  textStrict (_recLabel e) <+> "::" <+> typeInfoToDoc (e ^. recValue)

typeInfoToText :: PSType -> Text
typeInfoToText = renderText . typeInfoToDoc

typeInfoToDecl :: PSType -> Doc
typeInfoToDecl (TypeInfo  _ _ name params) =
  hsep $ textStrict name : (typeInfoToDoc <$> params)

typeInfoToDoc :: PSType -> Doc
typeInfoToDoc t@(TypeInfo  _ _ _ params) =
  (if null params then id else parens) $ typeInfoToDecl t

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
      , psQualifiedImports = instancesToQualifiedImports is
      , psTypes = [st]
      }
  where
    dropEmpty = Map.delete ""
    dropPrelude = Map.delete "Prelude"
    dropPrim = Map.delete "Prim"
    dropSelf = Map.delete (_typeModule t)

unionQualifiedImports :: Map Text Text -> Map Text Text -> Map Text Text
unionQualifiedImports = Map.unionWith const

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

instancesToQualifiedImports :: [Instance] -> Map Text Text
instancesToQualifiedImports =
  foldr unionQualifiedImports Map.empty . fmap instanceToQualifiedImports

instancesToImportLines :: [Instance] -> ImportLines
instancesToImportLines =
  foldr unionImportLines Map.empty . fmap instanceToImportLines

instanceToImportLines :: Instance -> ImportLines
instanceToImportLines GenericShow =
  importsFromList [ ImportLine "Data.Show.Generic" $ Set.singleton "genericShow" ]
instanceToImportLines Json =
  importsFromList
    [ ImportLine "Control.Alt" $ Set.singleton "(<|>)"
    , ImportLine "Data.Array" $ Set.singleton "index"
    , ImportLine "Data.Bifunctor" $ Set.singleton "lmap"
    , ImportLine "Data.Argonaut.Core" $ Set.fromList ["jsonEmptyArray", "jsonEmptyObject", "jsonNull", "fromArray", "fromString"]
    , ImportLine "Data.Argonaut.Decode" $ Set.fromList ["JsonDecodeError(..)", "(.:)", "(.:?)", "(.!=)", "decodeJson"]
    , ImportLine "Data.Argonaut.Decode.Aeson" $ Set.fromList ["Decoder", "(</$\\>)", "(</*\\>)", "(</\\>)"]
    , ImportLine "Data.Argonaut.Decode.Decoders" $ Set.fromList ["decodeJArray", "decodeJObject", "decodeArray", "decodeNull"]
    , ImportLine "Data.Argonaut.Encode" $ Set.fromList ["(:=)", "(~>)", "encodeJson"]
    , ImportLine "Data.Argonaut.Encode.Aeson" $ Set.fromList ["Encoder", "(>$<)", "(>*<)", "(>/\\<)", "(>|<)"]
    , ImportLine "Data.Either" $ Set.singleton "Either(..)"
    , ImportLine "Data.Maybe" $ Set.fromList ["Maybe(..)", "maybe"]
    , ImportLine "Data.Newtype" $ Set.singleton "unwrap"
    , ImportLine "Data.Tuple.Nested" $ Set.singleton "(/\\)"
    ]
instanceToImportLines Enum =
  importsFromList
    [ ImportLine "Data.Enum.Generic" $ Set.fromList ["genericPred", "genericSucc"]
    ]
instanceToImportLines Bounded =
  importsFromList
    [ ImportLine "Data.Bounded.Generic" $ Set.fromList ["genericBottom", "genericTop"]
    ]
instanceToImportLines _ = Map.empty

instanceToQualifiedImports :: Instance -> Map Text Text
instanceToQualifiedImports Json =
  Map.fromList
    [ ("Data.Argonaut.Decode.Aeson", "D")
    , ("Data.Argonaut.Encode.Aeson", "E")
    ]
instanceToQualifiedImports _ = Map.empty

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

constructorPattern :: DataConstructor 'PureScript -> Doc
constructorPattern (DataConstructor name Nullary) = nullaryPattern name
constructorPattern (DataConstructor name (Normal ts)) = normalPattern name ts
constructorPattern (DataConstructor name (Record rs)) = recordPattern name rs

constructorExpr :: DataConstructorArgs 'PureScript -> Doc
constructorExpr Nullary = nullaryExpr
constructorExpr (Normal ts) = normalExpr ts
constructorExpr (Record rs) = recordExpr rs

nullaryPattern :: Text -> Doc
nullaryPattern = textStrict

nullaryExpr :: Doc
nullaryExpr = "unit"

normalPattern :: Text -> NonEmpty PSType -> Doc
normalPattern name = parens . (textStrict name <+>) . hsep . normalLabels

normalExpr :: NonEmpty PSType -> Doc
normalExpr = parens . hsep . punctuate " /\\" . normalLabels

normalLabels :: NonEmpty PSType -> [Doc]
normalLabels = fmap char . zipWith const ['a'..] . NE.toList

recordPattern :: Text -> NonEmpty (RecordEntry 'PureScript) -> Doc
recordPattern name = parens . (textStrict name <+>) . recordExpr

recordExpr :: NonEmpty (RecordEntry 'PureScript) -> Doc
recordExpr = braces . hsep . punctuate ", " . recordLabels

recordLabels :: NonEmpty (RecordEntry 'PureScript) -> [Doc]
recordLabels = fmap recordLabel . NE.toList

recordLabel :: RecordEntry 'PureScript -> Doc
recordLabel = textStrict . _recLabel

case_of :: [(Doc, Doc)] -> Doc
case_of = caseOf "_"

caseOf :: Doc -> [(Doc, Doc)] -> Doc
caseOf scrutinee branches =
  vsep $ hsep ["case", scrutinee, "of"] : (indent 2 . uncurry branch <$> branches)

branch :: Doc -> Doc -> Doc
branch pattern body = hsep [pattern, "->", body]

lambda :: Doc -> Doc -> Doc
lambda variables body = backslash <> branch variables body

signature :: Bool -> Doc -> [PSType] -> [PSType] -> PSType-> Doc
signature topLevel name constraints params ret =
  hsep $ catMaybes [Just name, Just "::", forAll, constraintsDoc, paramsDoc, Just $ typeInfoToDoc ret]
    where
      forAll = case (topLevel, typeParams) of
        (False, _) -> Nothing
        (_, []) -> Nothing
        (_, ps) -> Just $ "forall" <+> hsep (textStrict <$> ps) <> "."
      typeParams = filter (isLower . T.head) $ _typeName  <$> allTypes
      allTypes = concatMap flattenTypeInfo $ constraints <> params <> [ret]
      constraintsDoc = case constraints of
        [] -> Nothing
        cs -> Just $ hsep ((<+> "=>") . typeInfoToDecl  <$> cs) 
      paramsDoc = case params of
        [] -> Nothing
        ps -> Just $ hsep ((<+> "->") . typeInfoToDecl  <$> ps) 

def :: Doc -> [PSType] -> [(Doc, PSType)] -> PSType -> Doc -> Doc
def name constraints params ret body = vsep
  [ signature True name constraints (snd <$> params) ret
  , hsep $ name : (fst <$> params) <> ["=", body]
  ]

mkType :: Text -> [PSType] -> PSType
mkType = TypeInfo "" ""
