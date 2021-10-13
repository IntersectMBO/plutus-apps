-- File auto generated by purescript-bridge! --
module RoundTrip.Types where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut.Core (fromArray, fromString, jsonEmptyArray, jsonEmptyObject, jsonNull)
import Data.Argonaut.Decode ((.!=), (.:), (.:?), JsonDecodeError(..), class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Decoders (decodeArray, decodeJArray, decodeJObject, decodeNull)
import Data.Argonaut.Encode ((:=), (~>), class EncodeJson, encodeJson)
import Data.Array (index)
import Data.Bifunctor (lmap)
import Data.Either (Either, Either(..))
import Data.Functor (class Functor)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\), Tuple3, Tuple4)
import Type.Proxy (Proxy(Proxy))

data TestData
  = Maybe (Maybe TestSum)
  | Either (Either String TestSum)

derive instance eqTestData :: Eq TestData

instance showTestData :: Show TestData where
  show x = genericShow x

derive instance ordTestData :: Ord TestData

instance encodeJsonTestData :: EncodeJson TestData where
  encodeJson =
    case _ of
      Maybe v0 ->
        "tag" := "Maybe" ~>
        "contents" :=
          ( (let a = v0 in case a of
                Nothing -> jsonNull
                Just a -> encodeJson a)
          ) ~>
        jsonEmptyObject
      Either v0 ->
        "tag" := "Either" ~>
        "contents" :=
          ( (let a = v0 in case a of
                Left a -> "Left" := (encodeJson a) ~> jsonEmptyObject
                Right a -> "Right" := (encodeJson a) ~> jsonEmptyObject)
          ) ~>
        jsonEmptyObject


instance decodeJsonTestData :: DecodeJson TestData where
  decodeJson json =
    do
      obj <- decodeJObject json
      tag <- obj .: "tag"
      json <- obj .:? "contents" .!= jsonNull
      case tag of
        "Maybe" -> lmap (AtKey "contents") $ Maybe <$>
          ( Nothing <$ decodeNull json <|>
            Just <$> decodeJson json
          )
        "Either" -> lmap (AtKey "contents") $ Either <$>
          ( decodeJson json >>= \obj ->
              Left <$> (obj .: "Left" >>= \json -> decodeJson json) <|>
              Right <$> (obj .: "Right" >>= \json -> decodeJson json)
          )
        _ -> Left $ AtKey "tag" (UnexpectedValue json)

derive instance genericTestData :: Generic TestData _

--------------------------------------------------------------------------------

_Maybe :: Prism' TestData (Maybe TestSum)
_Maybe = prism' Maybe f
  where
    f (Maybe a) = Just $ a
    f _ = Nothing

_Either :: Prism' TestData (Either String TestSum)
_Either = prism' Either f
  where
    f (Either a) = Just $ a
    f _ = Nothing

--------------------------------------------------------------------------------
data TestSum
  = Nullary
  | Bool Boolean
  | Int Int
  | Number Number
  | String String
  | Array (Array String)
  | Record (TestRecord Int)
  | NestedRecord (TestRecord (TestRecord Int))
  | NT TestNewtype
  | NTRecord TestNewtypeRecord
  | Unit Unit
  | MyUnit MyUnit
  | Pair (Tuple Int String)
  | Triple (Tuple3 Int String Boolean)
  | Quad (Tuple4 Int String Boolean Number)
  | QuadSimple Int String Boolean Number
  | NestedSum TestNestedSum
  | Enum TestEnum

derive instance eqTestSum :: Eq TestSum

instance showTestSum :: Show TestSum where
  show x = genericShow x

derive instance ordTestSum :: Ord TestSum

instance encodeJsonTestSum :: EncodeJson TestSum where
  encodeJson =
    case _ of
      Nullary ->
        "tag" := "Nullary" ~>
        jsonEmptyObject
      Bool v0 ->
        "tag" := "Bool" ~>
        "contents" :=
          ( (let a = v0 in encodeJson a)
          ) ~>
        jsonEmptyObject
      Int v0 ->
        "tag" := "Int" ~>
        "contents" :=
          ( (let a = v0 in encodeJson a)
          ) ~>
        jsonEmptyObject
      Number v0 ->
        "tag" := "Number" ~>
        "contents" :=
          ( (let a = v0 in encodeJson a)
          ) ~>
        jsonEmptyObject
      String v0 ->
        "tag" := "String" ~>
        "contents" :=
          ( (let a = v0 in encodeJson a)
          ) ~>
        jsonEmptyObject
      Array v0 ->
        "tag" := "Array" ~>
        "contents" :=
          ( (let a = v0 in encodeJson a)
          ) ~>
        jsonEmptyObject
      Record v0 ->
        "tag" := "Record" ~>
        "contents" :=
          ( (let a = v0 in encodeJson a)
          ) ~>
        jsonEmptyObject
      NestedRecord v0 ->
        "tag" := "NestedRecord" ~>
        "contents" :=
          ( (let a = v0 in encodeJson a)
          ) ~>
        jsonEmptyObject
      NT v0 ->
        "tag" := "NT" ~>
        "contents" :=
          ( (let a = v0 in encodeJson a)
          ) ~>
        jsonEmptyObject
      NTRecord v0 ->
        "tag" := "NTRecord" ~>
        "contents" :=
          ( (let a = v0 in encodeJson a)
          ) ~>
        jsonEmptyObject
      Unit v0 ->
        "tag" := "Unit" ~>
        "contents" :=
          ( (let a = v0 in jsonEmptyArray)
          ) ~>
        jsonEmptyObject
      MyUnit v0 ->
        "tag" := "MyUnit" ~>
        "contents" :=
          ( (let a = v0 in encodeJson a)
          ) ~>
        jsonEmptyObject
      Pair v0 ->
        "tag" := "Pair" ~>
        "contents" :=
          ( (let a = v0 in encodeJson a)
          ) ~>
        jsonEmptyObject
      Triple v0 ->
        "tag" := "Triple" ~>
        "contents" :=
          ( (let a = v0 in case a of v0 /\ v1 /\ v2 /\ unit ->
                  [ (let a = v0 in encodeJson a)
                  , (let a = v1 in encodeJson a)
                  , (let a = v2 in encodeJson a)
                  ])
          ) ~>
        jsonEmptyObject
      Quad v0 ->
        "tag" := "Quad" ~>
        "contents" :=
          ( (let a = v0 in case a of v0 /\ v1 /\ v2 /\ v3 /\ unit ->
                  [ (let a = v0 in encodeJson a)
                  , (let a = v1 in encodeJson a)
                  , (let a = v2 in encodeJson a)
                  , (let a = v3 in encodeJson a)
                  ])
          ) ~>
        jsonEmptyObject
      QuadSimple v0 v1 v2 v3 ->
        "tag" := "QuadSimple" ~>
        "contents" :=
          ( fromArray
              [ (let a = v0 in encodeJson a)
              , (let a = v1 in encodeJson a)
              , (let a = v2 in encodeJson a)
              , (let a = v3 in encodeJson a)
              ]
          ) ~>
        jsonEmptyObject
      NestedSum v0 ->
        "tag" := "NestedSum" ~>
        "contents" :=
          ( (let a = v0 in encodeJson a)
          ) ~>
        jsonEmptyObject
      Enum v0 ->
        "tag" := "Enum" ~>
        "contents" :=
          ( (let a = v0 in encodeJson a)
          ) ~>
        jsonEmptyObject


instance decodeJsonTestSum :: DecodeJson TestSum where
  decodeJson json =
    do
      obj <- decodeJObject json
      tag <- obj .: "tag"
      json <- obj .:? "contents" .!= jsonNull
      case tag of
        "Nullary" -> pure Nullary
        "Bool" -> lmap (AtKey "contents") $ Bool <$>
          ( decodeJson json
          )
        "Int" -> lmap (AtKey "contents") $ Int <$>
          ( decodeJson json
          )
        "Number" -> lmap (AtKey "contents") $ Number <$>
          ( decodeJson json
          )
        "String" -> lmap (AtKey "contents") $ String <$>
          ( decodeJson json
          )
        "Array" -> lmap (AtKey "contents") $ Array <$>
          ( decodeJson json
          )
        "Record" -> lmap (AtKey "contents") $ Record <$>
          ( decodeJson json
          )
        "NestedRecord" -> lmap (AtKey "contents") $ NestedRecord <$>
          ( decodeJson json
          )
        "NT" -> lmap (AtKey "contents") $ NT <$>
          ( decodeJson json
          )
        "NTRecord" -> lmap (AtKey "contents") $ NTRecord <$>
          ( decodeJson json
          )
        "Unit" -> lmap (AtKey "contents") $ Unit <$>
          ( unit <$ decodeArray (Left <<< UnexpectedValue) json
          )
        "MyUnit" -> lmap (AtKey "contents") $ MyUnit <$>
          ( decodeJson json
          )
        "Pair" -> lmap (AtKey "contents") $ Pair <$>
          ( decodeJson json
          )
        "Triple" -> lmap (AtKey "contents") $ Triple <$>
          ( do
              arr <- decodeJArray json
              v0 <-
                maybe
                  (Left $ AtIndex 0 $ MissingValue)
                  (\json ->
                    decodeJson json
                  )
                  $ index arr 0
              v1 <-
                maybe
                  (Left $ AtIndex 1 $ MissingValue)
                  (\json ->
                    decodeJson json
                  )
                  $ index arr 1
              v2 <-
                maybe
                  (Left $ AtIndex 2 $ MissingValue)
                  (\json ->
                    decodeJson json
                  )
                  $ index arr 2
              pure $ v0 /\ v1 /\ v2 /\ unit
          )
        "Quad" -> lmap (AtKey "contents") $ Quad <$>
          ( do
              arr <- decodeJArray json
              v0 <-
                maybe
                  (Left $ AtIndex 0 $ MissingValue)
                  (\json ->
                    decodeJson json
                  )
                  $ index arr 0
              v1 <-
                maybe
                  (Left $ AtIndex 1 $ MissingValue)
                  (\json ->
                    decodeJson json
                  )
                  $ index arr 1
              v2 <-
                maybe
                  (Left $ AtIndex 2 $ MissingValue)
                  (\json ->
                    decodeJson json
                  )
                  $ index arr 2
              v3 <-
                maybe
                  (Left $ AtIndex 3 $ MissingValue)
                  (\json ->
                    decodeJson json
                  )
                  $ index arr 3
              pure $ v0 /\ v1 /\ v2 /\ v3 /\ unit
          )
        "QuadSimple" -> do
          arr <- decodeJArray json
          lmap (AtKey "contents") $ QuadSimple <$>
            ( do
                json <- maybe (Left $ AtIndex 0 $ MissingValue) Right $ index arr 0
                decodeJson json
            ) <*>
            ( do
                json <- maybe (Left $ AtIndex 1 $ MissingValue) Right $ index arr 1
                decodeJson json
            ) <*>
            ( do
                json <- maybe (Left $ AtIndex 2 $ MissingValue) Right $ index arr 2
                decodeJson json
            ) <*>
            ( do
                json <- maybe (Left $ AtIndex 3 $ MissingValue) Right $ index arr 3
                decodeJson json
            )
        "NestedSum" -> lmap (AtKey "contents") $ NestedSum <$>
          ( decodeJson json
          )
        "Enum" -> lmap (AtKey "contents") $ Enum <$>
          ( decodeJson json
          )
        _ -> Left $ AtKey "tag" (UnexpectedValue json)

derive instance genericTestSum :: Generic TestSum _

--------------------------------------------------------------------------------

_Nullary :: Prism' TestSum Unit
_Nullary = prism' (\_ -> Nullary) f
  where
    f Nullary = Just unit
    f _ = Nothing

_Bool :: Prism' TestSum Boolean
_Bool = prism' Bool f
  where
    f (Bool a) = Just $ a
    f _ = Nothing

_Int :: Prism' TestSum Int
_Int = prism' Int f
  where
    f (Int a) = Just $ a
    f _ = Nothing

_Number :: Prism' TestSum Number
_Number = prism' Number f
  where
    f (Number a) = Just $ a
    f _ = Nothing

_String :: Prism' TestSum String
_String = prism' String f
  where
    f (String a) = Just $ a
    f _ = Nothing

_Array :: Prism' TestSum (Array String)
_Array = prism' Array f
  where
    f (Array a) = Just $ a
    f _ = Nothing

_Record :: Prism' TestSum (TestRecord Int)
_Record = prism' Record f
  where
    f (Record a) = Just $ a
    f _ = Nothing

_NestedRecord :: Prism' TestSum (TestRecord (TestRecord Int))
_NestedRecord = prism' NestedRecord f
  where
    f (NestedRecord a) = Just $ a
    f _ = Nothing

_NT :: Prism' TestSum TestNewtype
_NT = prism' NT f
  where
    f (NT a) = Just $ a
    f _ = Nothing

_NTRecord :: Prism' TestSum TestNewtypeRecord
_NTRecord = prism' NTRecord f
  where
    f (NTRecord a) = Just $ a
    f _ = Nothing

_Unit :: Prism' TestSum Unit
_Unit = prism' Unit f
  where
    f (Unit a) = Just $ a
    f _ = Nothing

_MyUnit :: Prism' TestSum MyUnit
_MyUnit = prism' MyUnit f
  where
    f (MyUnit a) = Just $ a
    f _ = Nothing

_Pair :: Prism' TestSum (Tuple Int String)
_Pair = prism' Pair f
  where
    f (Pair a) = Just $ a
    f _ = Nothing

_Triple :: Prism' TestSum (Tuple3 Int String Boolean)
_Triple = prism' Triple f
  where
    f (Triple a) = Just $ a
    f _ = Nothing

_Quad :: Prism' TestSum (Tuple4 Int String Boolean Number)
_Quad = prism' Quad f
  where
    f (Quad a) = Just $ a
    f _ = Nothing

_QuadSimple :: Prism' TestSum { a :: Int
                              , b :: String
                              , c :: Boolean
                              , d :: Number }
_QuadSimple = prism' (\{ a, b, c, d } -> QuadSimple a b c d) f
  where
    f (QuadSimple a b c d) = Just $ { a: a, b: b, c: c, d: d }
    f _ = Nothing

_NestedSum :: Prism' TestSum TestNestedSum
_NestedSum = prism' NestedSum f
  where
    f (NestedSum a) = Just $ a
    f _ = Nothing

_Enum :: Prism' TestSum TestEnum
_Enum = prism' Enum f
  where
    f (Enum a) = Just $ a
    f _ = Nothing

--------------------------------------------------------------------------------
newtype TestRecord a
  = TestRecord
      { _field1 :: String
      , _field2 :: a
      }

derive instance functorTestRecord :: Functor TestRecord

derive instance eqTestRecord :: (Eq a) => Eq (TestRecord a)

instance showTestRecord :: (Show a) => Show (TestRecord a) where
  show x = genericShow x

derive instance ordTestRecord :: (Ord a) => Ord (TestRecord a)

instance encodeJsonTestRecord :: (EncodeJson a) => EncodeJson (TestRecord a) where
  encodeJson =
    case _ of
      TestRecord {_field1,  _field2} ->
        "_field1" := (let a = _field1 in encodeJson a) ~>
        "_field2" := (let a = _field2 in encodeJson a) ~>
        jsonEmptyObject


instance decodeJsonTestRecord :: (DecodeJson a) => DecodeJson (TestRecord a) where
  decodeJson json =
    do
      x <- decodeJson json
      _field1 <- x .: "_field1" >>= \json -> decodeJson json
      _field2 <- x .: "_field2" >>= \json -> decodeJson json
      pure $ TestRecord {_field1,  _field2}

derive instance genericTestRecord :: Generic (TestRecord a) _

derive instance newtypeTestRecord :: Newtype (TestRecord a) _

--------------------------------------------------------------------------------

_TestRecord :: forall a. Iso' (TestRecord a) { _field1 :: String, _field2 :: a }
_TestRecord = _Newtype

field1 :: forall a. Lens' (TestRecord a) String
field1 = _Newtype <<< prop (Proxy :: _ "_field1")

field2 :: forall a. Lens' (TestRecord a) a
field2 = _Newtype <<< prop (Proxy :: _ "_field2")

--------------------------------------------------------------------------------
newtype TestNewtype
  = TestNewtype (TestRecord String)

derive instance eqTestNewtype :: Eq TestNewtype

instance showTestNewtype :: Show TestNewtype where
  show x = genericShow x

derive instance ordTestNewtype :: Ord TestNewtype

instance encodeJsonTestNewtype :: EncodeJson TestNewtype where
  encodeJson =
    case _ of
      TestNewtype v0 ->
        (let a = v0 in encodeJson a)


instance decodeJsonTestNewtype :: DecodeJson TestNewtype where
  decodeJson json =
    lmap (AtKey "contents") $ TestNewtype <$>
    ( decodeJson json
    )

derive instance genericTestNewtype :: Generic TestNewtype _

derive instance newtypeTestNewtype :: Newtype TestNewtype _

--------------------------------------------------------------------------------

_TestNewtype :: Iso' TestNewtype (TestRecord String)
_TestNewtype = _Newtype

--------------------------------------------------------------------------------
newtype TestNewtypeRecord
  = TestNewtypeRecord
      { unTestNewtypeRecord :: TestNewtype
      }

derive instance eqTestNewtypeRecord :: Eq TestNewtypeRecord

instance showTestNewtypeRecord :: Show TestNewtypeRecord where
  show x = genericShow x

derive instance ordTestNewtypeRecord :: Ord TestNewtypeRecord

instance encodeJsonTestNewtypeRecord :: EncodeJson TestNewtypeRecord where
  encodeJson =
    case _ of
      TestNewtypeRecord {unTestNewtypeRecord} ->
        "unTestNewtypeRecord" := (let a = unTestNewtypeRecord in encodeJson a) ~>
        jsonEmptyObject


instance decodeJsonTestNewtypeRecord :: DecodeJson TestNewtypeRecord where
  decodeJson json =
    do
      x <- decodeJson json
      unTestNewtypeRecord <- x .: "unTestNewtypeRecord" >>= \json -> decodeJson json
      pure $ TestNewtypeRecord {unTestNewtypeRecord}

derive instance genericTestNewtypeRecord :: Generic TestNewtypeRecord _

derive instance newtypeTestNewtypeRecord :: Newtype TestNewtypeRecord _

--------------------------------------------------------------------------------

_TestNewtypeRecord :: Iso' TestNewtypeRecord { unTestNewtypeRecord :: TestNewtype }
_TestNewtypeRecord = _Newtype

--------------------------------------------------------------------------------
data TestNestedSum
  = Case1 String
  | Case2 Int
  | Case3 (TestRecord Int)

derive instance eqTestNestedSum :: Eq TestNestedSum

instance showTestNestedSum :: Show TestNestedSum where
  show x = genericShow x

derive instance ordTestNestedSum :: Ord TestNestedSum

instance encodeJsonTestNestedSum :: EncodeJson TestNestedSum where
  encodeJson =
    case _ of
      Case1 v0 ->
        "tag" := "Case1" ~>
        "contents" :=
          ( (let a = v0 in encodeJson a)
          ) ~>
        jsonEmptyObject
      Case2 v0 ->
        "tag" := "Case2" ~>
        "contents" :=
          ( (let a = v0 in encodeJson a)
          ) ~>
        jsonEmptyObject
      Case3 v0 ->
        "tag" := "Case3" ~>
        "contents" :=
          ( (let a = v0 in encodeJson a)
          ) ~>
        jsonEmptyObject


instance decodeJsonTestNestedSum :: DecodeJson TestNestedSum where
  decodeJson json =
    do
      obj <- decodeJObject json
      tag <- obj .: "tag"
      json <- obj .:? "contents" .!= jsonNull
      case tag of
        "Case1" -> lmap (AtKey "contents") $ Case1 <$>
          ( decodeJson json
          )
        "Case2" -> lmap (AtKey "contents") $ Case2 <$>
          ( decodeJson json
          )
        "Case3" -> lmap (AtKey "contents") $ Case3 <$>
          ( decodeJson json
          )
        _ -> Left $ AtKey "tag" (UnexpectedValue json)

derive instance genericTestNestedSum :: Generic TestNestedSum _

--------------------------------------------------------------------------------

_Case1 :: Prism' TestNestedSum String
_Case1 = prism' Case1 f
  where
    f (Case1 a) = Just $ a
    f _ = Nothing

_Case2 :: Prism' TestNestedSum Int
_Case2 = prism' Case2 f
  where
    f (Case2 a) = Just $ a
    f _ = Nothing

_Case3 :: Prism' TestNestedSum (TestRecord Int)
_Case3 = prism' Case3 f
  where
    f (Case3 a) = Just $ a
    f _ = Nothing

--------------------------------------------------------------------------------
data TestEnum
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun

derive instance eqTestEnum :: Eq TestEnum

instance showTestEnum :: Show TestEnum where
  show x = genericShow x

derive instance ordTestEnum :: Ord TestEnum

instance encodeJsonTestEnum :: EncodeJson TestEnum where
  encodeJson =
    fromString <<< show


instance decodeJsonTestEnum :: DecodeJson TestEnum where
  decodeJson json =
    decodeJson json >>= case _ of
      "Mon" -> pure Mon
      "Tue" -> pure Tue
      "Wed" -> pure Wed
      "Thu" -> pure Thu
      "Fri" -> pure Fri
      "Sat" -> pure Sat
      "Sun" -> pure Sun
      _ -> Left (UnexpectedValue json)

derive instance genericTestEnum :: Generic TestEnum _

--------------------------------------------------------------------------------

_Mon :: Prism' TestEnum Unit
_Mon = prism' (\_ -> Mon) f
  where
    f Mon = Just unit
    f _ = Nothing

_Tue :: Prism' TestEnum Unit
_Tue = prism' (\_ -> Tue) f
  where
    f Tue = Just unit
    f _ = Nothing

_Wed :: Prism' TestEnum Unit
_Wed = prism' (\_ -> Wed) f
  where
    f Wed = Just unit
    f _ = Nothing

_Thu :: Prism' TestEnum Unit
_Thu = prism' (\_ -> Thu) f
  where
    f Thu = Just unit
    f _ = Nothing

_Fri :: Prism' TestEnum Unit
_Fri = prism' (\_ -> Fri) f
  where
    f Fri = Just unit
    f _ = Nothing

_Sat :: Prism' TestEnum Unit
_Sat = prism' (\_ -> Sat) f
  where
    f Sat = Just unit
    f _ = Nothing

_Sun :: Prism' TestEnum Unit
_Sun = prism' (\_ -> Sun) f
  where
    f Sun = Just unit
    f _ = Nothing

--------------------------------------------------------------------------------
data MyUnit
  = U

derive instance eqMyUnit :: Eq MyUnit

instance showMyUnit :: Show MyUnit where
  show x = genericShow x

derive instance ordMyUnit :: Ord MyUnit

instance encodeJsonMyUnit :: EncodeJson MyUnit where
  encodeJson =
    fromString <<< show


instance decodeJsonMyUnit :: DecodeJson MyUnit where
  decodeJson json =
    decodeJson json >>= case _ of
      "U" -> pure U
      _ -> Left (UnexpectedValue json)

derive instance genericMyUnit :: Generic MyUnit _

--------------------------------------------------------------------------------

_U :: Prism' MyUnit Unit
_U = prism' (\_ -> U) f
  where
    f U = Just unit

--------------------------------------------------------------------------------
