{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BlockArguments #-}

module Language.PureScript.Bridge.Printer where

import           Control.Lens                               (to,(^.), (%~), (<>~))
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
                                                             RecordEntry (..),
                                                             DataConstructorArgs (..),
                                                             SumType (SumType),
                                                             getUsedTypes,
                                                             nootype, recLabel,
                                                             recValue,
                                                             _recLabel, sigConstructor)
import           Language.PureScript.Bridge.TypeInfo        (Language (PureScript),
                                                             PSType, TypeInfo (TypeInfo),
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
                                                             hsep, indent,
                                                             line, lparen,
                                                             parens, punctuate,
                                                             renderPretty,
                                                             rparen,
                                                             textStrict, vsep,
                                                             (<+>), hang, dquotes, char, backslash, nest, linebreak, lbrace, rbrace, softline, lbracket, rbracket)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Char (isLower, toLower)
import Language.PureScript.Bridge.PSTypes (psUnit)
import Control.Arrow ((&&&))
import Data.Function ((&), on)
import Data.List (nubBy)

renderText :: Doc -> Text
renderText = T.replace " \n" "\n" . displayTStrict . renderPretty 0.4 200

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
    , ImportLine "Data.Argonaut.Encode.Aeson" $ Set.fromList ["Encoder", "(>$<)", "(>*<)", "(>/\\<)"]
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
    , ("Data.Map", "Map")
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
  renderText $ vsep $
    [ "-- File auto generated by purescript-bridge! --"
    , "module" <+> textStrict (psModuleName m) <+> "where" <> linebreak
    , "import Prelude" <> linebreak
    , vsep
        ( (importLineToText <$> allImports)
            <> (uncurry qualifiedImportToText <$> Map.toList (psQualifiedImports m))
        )
          <> linebreak
    ]
    <> punctuate (line <> line <> dashes <> line) (sumTypeToDocs settings =<< psTypes m)
  where
    otherImports =
      importsFromList
        (lensImports settings <> genericsImports)
    allImports = Map.elems $ mergeImportLines otherImports (psImportLines m)
    dashes = textStrict (T.replicate 80 "-")

genericsImports :: [ImportLine]
genericsImports =
    [ ImportLine "Data.Generic.Rep" $ Set.singleton "class Generic" ]

lensImports :: Switches.Settings -> [ImportLine]
lensImports settings
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

qualifiedImportToText :: Text -> Text -> Doc
qualifiedImportToText m q = hsep ["import", textStrict m, "as", textStrict q]

importLineToText :: ImportLine -> Doc
importLineToText l =
  hsep ["import", textStrict $ importModule l, encloseHsep lparen rparen comma typeList]
    where
      typeList = textStrict <$> Set.toList (importTypes l)

sumTypeToDocs :: Switches.Settings -> SumType 'PureScript -> [Doc]
sumTypeToDocs settings st
  | Switches.generateLenses settings = [sumTypeToTypeDecls st, sumTypeToOptics st]
  | otherwise = [sumTypeToTypeDecls st]

sumTypeToTypeDecls :: SumType 'PureScript -> Doc
sumTypeToTypeDecls st@(SumType t cs _) =
  vsep $ punctuate line $ typeDecl : instances st
  where
    typeDecl
      | isJust (nootype cs) = mkTypeDecl "newtype"
      | otherwise = mkTypeDecl "data"
    mkTypeDecl keyword =
      keyword <+> typeInfoToDecl t <+> encloseVsep "=" mempty "|" (constructorToDoc <$> cs)

typeInfoToDecl :: PSType -> Doc
typeInfoToDecl (TypeInfo  _ _ name params) =
  hsep $ textStrict name : (typeInfoToDoc <$> params)

typeInfoToDoc :: PSType -> Doc
typeInfoToDoc t@(TypeInfo  _ _ _ params) =
  (if null params then id else parens) $ typeInfoToDecl t

constructorToDoc :: DataConstructor 'PureScript -> Doc
constructorToDoc (DataConstructor n args) =
  hsep $ textStrict n : case args of
    Nullary -> []
    Normal ts -> NE.toList $ typeInfoToDoc <$> ts
    Record rs -> [vrecord $ fieldSignatures rs]

-- | Given a Purescript type, generate instances for typeclass
-- instances it claims to have.
instances :: SumType 'PureScript -> [Doc]
instances st@(SumType t _ is) = go <$> is
  where
    mkConstraints :: (PSType -> [PSType]) -> [Doc]
    mkConstraints getConstraints = case getConstraints t of
      [] -> []
      constraints -> [encloseHsep lparen rparen comma (typeInfoToDecl <$> constraints), "=>"]
    mkInstance name getConstraints ty methods =
      vsep
        [ hsep
            [ "instance"
            , textStrict $ T.cons (toLower $ T.head name) (T.tail name) <> _typeName ty
            , "::"
            , hsep $ mkConstraints getConstraints <> [typeInfoToDecl $ mkType name [ty]]
            , "where"
            ]
        , indent 2 $ vsep methods
        ]
    mkDerivedInstance name getConstraints params ty =
      hsep $
        [ "derive instance"
        , textStrict $ T.cons (toLower $ T.head name) (T.tail name) <> _typeName ty
        , "::"
        , hsep $ mkConstraints getConstraints <> [typeInfoToDecl $ mkType name [ty]]
        ]
        <> params
    toKind1 (TypeInfo p m n []) = TypeInfo p m n []
    toKind1 (TypeInfo p m n ps) = TypeInfo p m n $ init ps
    go :: Instance -> Doc
    go Bounded = mkInstance "Bounded" (const []) t
      [ "bottom = genericBottom"
      , "top = genericTop"
      ]
    go Enum = mkInstance "Enum" (const []) t
      [ "succ = genericSucc"
      , "pred = genericPred"
      ]
    go Json = vsep $ punctuate line
      [ mkInstance "EncodeJson" encodeJsonConstraints t
        [ "encodeJson =" <+> nest 2 (sumTypeToEncode st) ]
      , mkInstance "DecodeJson" decodeJsonConstraints t
        [ "decodeJson = D.decode" <+> nest 2 (sumTypeToDecode st) ]
      ]
    go GenericShow = mkInstance "Show" showConstraints t [ "show = genericShow" ]
    go Functor = mkDerivedInstance "Functor" (const []) [] $ toKind1 t
    go Eq = mkDerivedInstance "Eq" eqConstraints [] t
    go Eq1 = mkDerivedInstance "Eq1" (const []) [] $ toKind1 t
    go Ord = mkDerivedInstance "Ord" ordConstraints [] t
    go Generic = mkDerivedInstance "Generic" (const []) ["_"] t
    go Newtype = mkDerivedInstance "Newtype" (const []) ["_"] t

constrainWith :: Text -> PSType -> [PSType]
constrainWith name = map (mkType name . pure) . typeParams

eqConstraints :: PSType -> [PSType]
eqConstraints = constrainWith "Eq"

ordConstraints :: PSType -> [PSType]
ordConstraints = constrainWith "Ord"

showConstraints :: PSType -> [PSType]
showConstraints = constrainWith "Show"

decodeJsonConstraints :: PSType -> [PSType]
decodeJsonConstraints = constrainWith "DecodeJson"

encodeJsonConstraints :: PSType -> [PSType]
encodeJsonConstraints = constrainWith "EncodeJson"

isEnum :: [DataConstructor lang] -> Bool
isEnum = all $ (== Nullary) . _sigValues

sumTypeToEncode :: SumType 'PureScript -> Doc
sumTypeToEncode (SumType _ cs _)
  | isEnum cs = "E.encode E.enum"
  | otherwise =
    linebreak <> case cs of
      [dc@(DataConstructor _ args)] ->
        "E.encode $"
          <+> vsep
            [ if isJust (nootype [dc])
                then "unwrap"
                else parens $ case_of [(constructorPattern dc, constructor args)]
            , ">$<" <+> nest 2 (argsToEncode args)
            ]
      _ -> case_of (constructorToEncode <$> cs)
  where
    constructorToEncode c@(DataConstructor name args) =
      ( constructorPattern c
      , "E.encodeTagged" <+> dquotes (textStrict name) <+> constructor args <+> argsToEncode args
      )
    argsToEncode Nullary = "E.null"
    argsToEncode (Normal (t :| [])) = typeToEncode t
    argsToEncode (Normal ts) =
      parens $ "E.tuple" <+> encloseHsep lparen rparen " >/\\<" (typeToEncode <$> NE.toList ts)
    argsToEncode (Record rs) =
      parens $ "E.record" <> softline <> vrecord (fieldSignatures $ fieldEncoder <$> rs)
        where
          fieldEncoder r =
            r
              & recValue %~ mkType "_" . pure
              & recLabel <>~ renderText (":" <+> typeToEncode (_recValue r))

flattenTuple :: [PSType] -> [PSType]
flattenTuple [] = []
flattenTuple [a] = [a]
flattenTuple [a, TypeInfo "purescript-tuples" "Data.Tuple" "Tuple" ts'] = a : flattenTuple ts'
flattenTuple (h : t) = h : flattenTuple t

typeToEncode :: PSType -> Doc
typeToEncode (TypeInfo "purescript-prelude" "Prelude" "Unit" []) = "E.unit"
typeToEncode (TypeInfo "purescript-maybe" "Data.Maybe" "Maybe" [t]) = parens $
  "E.maybe" <+> typeToEncode t
typeToEncode (TypeInfo "purescript-either" "Data.Either" "Either" [l, r]) = parens $
  "E.either" <+> typeToEncode l <+> typeToEncode r
typeToEncode (TypeInfo "purescript-tuples" "Data.Tuple" "Tuple" ts) = parens $
  "E.tuple" <+> parens (hsep $ punctuate " >/\\<" $ typeToEncode <$> flattenTuple ts)
typeToEncode _ = "E.value"


sumTypeToDecode :: SumType 'PureScript -> Doc
sumTypeToDecode (SumType _ cs _)
  | isEnum cs = "D.enum"
sumTypeToDecode (SumType _ [c] _) = "$" <+> constructorToDecode c
sumTypeToDecode (SumType t cs _) = line <>
  vsep
    [ "$ D.sumType" <+> t ^. typeName . to textStrict . to dquotes
    , hang 2
        $ "$ Map.fromFoldable"
        <+> encloseVsep lbracket rbracket comma (constructorToTagged <$> cs)
    ]
  where
    constructorToTagged dc = hsep
      [ dc ^. sigConstructor . to textStrict . to dquotes
      , "/\\"
      , constructorToDecode dc
      ]


constructorToDecode :: DataConstructor 'PureScript -> Doc
constructorToDecode (DataConstructor name Nullary) =
  parens $ textStrict name <+> "<$" <+> "D.null"
constructorToDecode (DataConstructor name (Normal (a :| []))) =
  parens $ textStrict name <+> "<$>" <+> typeToDecode a
constructorToDecode (DataConstructor name (Normal as)) =
  parens $ "D.tuple"
    <+> "$"
    <+> textStrict name
    <+> encloseHsep "</$\\>" mempty " </*\\>" (typeToDecode <$> NE.toList as)
constructorToDecode (DataConstructor name (Record rs)) =
  parens $ textStrict name
    <+> "<$> D.record"
    <+> dquotes (textStrict name)
    <+> vrecord (fieldSignatures $ fieldDecoder <$> rs)
    where
      fieldDecoder r =
        r
          & recValue %~ mkType "_" . pure
          & recLabel <>~ renderText (":" <+> typeToDecode (_recValue r))

typeToDecode :: PSType -> Doc
typeToDecode (TypeInfo "purescript-prelude" "Prelude" "Unit" []) = "D.unit"
typeToDecode (TypeInfo "purescript-maybe" "Data.Maybe" "Maybe" [t]) = parens $
  "D.maybe" <+> typeToDecode t
typeToDecode (TypeInfo "purescript-either" "Data.Either" "Either" [l, r]) = parens $
  "D.either" <+> typeToDecode l <+> typeToDecode r
typeToDecode (TypeInfo "purescript-tuples" "Data.Tuple" "Tuple" ts) = parens $
  "D.tuple" <+> encloseHsep lparen rparen " </\\>" (typeToDecode <$> flattenTuple ts)
typeToDecode _ = "D.value"


sumTypeToOptics :: SumType 'PureScript -> Doc
sumTypeToOptics st =
  vsep $ punctuate line $ constructorOptics st <> recordOptics st

constructorOptics :: SumType 'PureScript -> [Doc]
constructorOptics (SumType t cs _) = constructorToOptic (length cs > 1) t <$> cs

recordOptics :: SumType 'PureScript -> [Doc]
recordOptics st@(SumType _ [DataConstructor _ (Record rs)] _) =
  recordEntryToLens st <$> filter hasUnderscore (NE.toList rs)
recordOptics _ = mempty

hasUnderscore :: RecordEntry lang -> Bool
hasUnderscore (RecordEntry name _) = "_" `T.isPrefixOf` name

constructorToOptic ::
     Bool -> TypeInfo 'PureScript -> DataConstructor 'PureScript -> Doc
constructorToOptic hasOtherConstructors typeInfo (DataConstructor n args) =
  case (args, hasOtherConstructors) of
    (Nullary, False) -> iso pName typeInfo psUnit "(const unit)" $ parens ("const" <+> cName)
    (Nullary, True) -> prism pName typeInfo psUnit cName "unit" $ parens ("const" <+> cName)
    (Normal (t :| []), False) -> newtypeIso pName typeInfo t
    (Normal (t :| []), True) -> prism pName typeInfo t (parens $ normalPattern n [t]) "a" cName
    (Normal ts, _)
      | hasOtherConstructors -> prism pName typeInfo toType fromExpr toExpr toMorph
      | otherwise -> iso pName typeInfo toType fromMorph toMorph
      where
        fields' = fields $ typesToRecord ts
        toType = recordType $ typesToRecord ts
        fromExpr = parens $ normalPattern n ts
        toExpr = hrecord fields'
        fromMorph = parens $ lambda fromExpr toExpr
        toMorph = parens $ lambda toExpr fromExpr
    (Record rs, False) -> newtypeIso pName typeInfo $ recordType rs
    (Record rs, True) ->
      prism pName typeInfo (recordType rs) fromExpr toExpr cName
        where
          fromExpr = parens $ pattern n toExpr
          toExpr = "a"
  where
    cName = textStrict n
    pName = "_" <> textStrict n
    recordType = (`mkType` []) . renderText . hrecord . fieldSignatures

typesToRecord :: NonEmpty PSType -> NonEmpty (RecordEntry 'PureScript)
typesToRecord = fmap (uncurry RecordEntry) . NE.zip (T.singleton <$> ['a'..])

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
recordEntryToLens (SumType t _ _) e =
  if hasUnderscore e
    then vsep
      [ signature True lensName [] [] $ mkType "Lens'" [t, e ^. recValue]
      , lensName <+> "= _Newtype <<< prop" <+> parens ("Proxy :: _" <> dquotes recName)
      ]
    else mempty
  where
    recName = e ^. recLabel . to textStrict
    lensName = e ^. recLabel . to (T.drop 1) . to textStrict

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mbool action = mbool >>= flip unless action

constructorPattern :: DataConstructor 'PureScript -> Doc
constructorPattern (DataConstructor name Nullary) = nullaryPattern name
constructorPattern (DataConstructor name (Normal ts)) = normalPattern name ts
constructorPattern (DataConstructor name (Record rs)) = recordPattern name rs

constructor :: DataConstructorArgs 'PureScript -> Doc
constructor Nullary = nullaryExpr
constructor (Normal ts) = normalExpr ts
constructor (Record rs) = hrecord $ fields rs

nullaryPattern :: Text -> Doc
nullaryPattern = textStrict

nullaryExpr :: Doc
nullaryExpr = "unit"

normalPattern :: Text -> NonEmpty PSType -> Doc
normalPattern name = pattern name . hsep . normalLabels

normalExpr :: NonEmpty PSType -> Doc
normalExpr (_ :| []) = "a"
normalExpr ts = parens . hsep . punctuate " /\\" $ normalLabels ts

normalLabels :: NonEmpty PSType -> [Doc]
normalLabels = fmap char . zipWith const ['a'..] . NE.toList

recordPattern :: Text -> NonEmpty (RecordEntry 'PureScript) -> Doc
recordPattern name = pattern name . hrecord . fields

vrecord :: [Doc] -> Doc
vrecord = encloseVsep lbrace rbrace comma

hrecord :: [Doc] -> Doc
hrecord = encloseHsep lbrace rbrace comma

fields :: NonEmpty (RecordEntry 'PureScript) -> [Doc]
fields = fmap field . NE.toList

field :: RecordEntry 'PureScript -> Doc
field = textStrict . _recLabel

fieldSignatures :: NonEmpty (RecordEntry 'PureScript) -> [Doc]
fieldSignatures = fmap fieldSignature . NE.toList

fieldSignature :: RecordEntry 'PureScript -> Doc
fieldSignature = uncurry signature' . (field &&& _recValue)

pattern :: Text -> Doc -> Doc
pattern name = (textStrict name <+>)

case_of :: [(Doc, Doc)] -> Doc
case_of = caseOf "_"

caseOf :: Doc -> [(Doc, Doc)] -> Doc
caseOf scrutinee [(p, b)] =
  hsep ["case", scrutinee, "of", branch p b]
caseOf scrutinee branches =
  vsep $ hsep ["case", scrutinee, "of"] : (indent 2 . uncurry branch <$> branches)

branch :: Doc -> Doc -> Doc
branch p body = hsep [p, "->", body]

lambda :: Doc -> Doc -> Doc
lambda variables body = backslash <> branch variables body

signature' :: Doc -> PSType-> Doc
signature' name = signature False name [] []

signature :: Bool -> Doc -> [PSType] -> [PSType] -> PSType-> Doc
signature topLevel name constraints params ret =
  hsep $ catMaybes [Just name, Just "::", forAll, constraintsDoc, paramsDoc, Just $ typeInfoToDecl ret]
    where
      forAll = case (topLevel, allTypes >>= typeParams) of
        (False, _) -> Nothing
        (_, []) -> Nothing
        (_, ps) -> Just $ "forall" <+> hsep (typeInfoToDoc <$> nubBy (on (==) _typeName) ps) <> "."
      allTypes = ret : constraints <> params
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

typeParams :: PSType -> [PSType]
typeParams = filter (isLower . T.head . _typeName) . flattenTypeInfo

encloseHsep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseHsep left right sp ds =
  case ds of
    []  -> left <> right
    _   -> left <> hsep (punctuate sp ds) <> right

encloseVsep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseVsep left right sp ds =
  case ds of
    []  -> left <> right
    [d] -> left <+> d <+> right
    _   -> nest 2 $ linebreak <> vsep (zipWith (<+>) (left : repeat (hang 2 sp)) ds <> [right])

