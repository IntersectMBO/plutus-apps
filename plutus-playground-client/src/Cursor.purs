-- | A cursor is an Array with a pointer to the 'current' item, plus
-- some guarantees* that you cannot get into an invalid state.
--
-- * Mostly guaranteed by using smart constructors and judicious exports.
module Cursor
  ( Cursor
  , current
  , first
  , last
  , empty
  , singleton
  , snoc
  , deleteAt
  , fromArray
  , toArray
  , mapWithIndex
  , null
  , length
  , _current
  , setIndex
  , getIndex
  , left
  , right
  ) where

import Prologue hiding (clamp)
import Control.Monad.Gen.Class (chooseInt)
import Data.Array as Array
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Lens.AffineTraversal (AffineTraversal', affineTraversal)
import Data.Lens.Index (class Index)
import Data.Maybe (fromMaybe, maybe)
import Data.Ord as Ord
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (uncurry)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (arrayOf)

data Cursor a = Cursor Int (Array a)

derive instance eqCursor :: Eq a => Eq (Cursor a)

derive instance ordCursor :: Ord a => Ord (Cursor a)

derive instance functorCursor :: Functor Cursor

instance foldableCursor :: Foldable Cursor where
  foldr f acc (Cursor _ xs) = foldr f acc xs
  foldl f acc (Cursor _ xs) = foldl f acc xs
  foldMap f (Cursor _ xs) = foldMap f xs

instance traversableCursor :: Traversable Cursor where
  traverse f (Cursor n xs) = Cursor n <$> traverse f xs
  sequence = sequenceDefault

instance showCursor :: Show a => Show (Cursor a) where
  show (Cursor index xs) = "Cursor " <> show index <> " " <> show xs

instance arbitraryCursor :: Arbitrary a => Arbitrary (Cursor a) where
  arbitrary = do
    xs <- arrayOf arbitrary
    index <- chooseInt 0 (Array.length xs - 1)
    pure $ Cursor index xs

instance indexCursor :: Index (Cursor a) Int a where
  ix n = affineTraversal set pre
    where
    set c@(Cursor index xs) a = fromMaybe c $ Cursor index <$> Array.updateAt n a xs

    pre c@(Cursor _ xs) = maybe (Left c) Right $ Array.index xs n

instance encodeCursor :: EncodeJson a => EncodeJson (Cursor a) where
  encodeJson (Cursor n xs) = encodeJson [ encodeJson n, encodeJson xs ]

instance decodeCursor :: DecodeJson a => DecodeJson (Cursor a) where
  decodeJson value = uncurry Cursor <$> decodeJson value

_current :: forall a. AffineTraversal' (Cursor a) a
_current = affineTraversal set pre
  where
  set (Cursor index xs) a = Cursor index $ fromMaybe xs $ Array.updateAt index a xs

  pre c@(Cursor index xs) = maybe (Left c) Right $ Array.index xs index

clamp :: forall a. Cursor a -> Cursor a
clamp (Cursor index xs) =
  Cursor
    (Ord.clamp 0 (Array.length xs - 1) index)
    xs

empty :: forall a. Cursor a
empty = fromArray []

singleton :: forall a. a -> Cursor a
singleton = fromArray <<< Array.singleton

snoc :: forall a. Cursor a -> a -> Cursor a
snoc (Cursor index xs) x = last $ Cursor index (Array.snoc xs x)

deleteAt :: forall a. Int -> Cursor a -> Cursor a
deleteAt n cursor@(Cursor index xs) =
  fromMaybe cursor do
    let
      newIndex
        | n >= index = index
        | otherwise = index - 1
    newXs <- Array.deleteAt n xs
    pure $ clamp $ Cursor newIndex newXs

fromArray :: forall a. Array a -> Cursor a
fromArray xs = Cursor 0 xs

toArray :: forall a. Cursor a -> Array a
toArray (Cursor _ xs) = xs

mapWithIndex :: forall b a. (Int -> a -> b) -> Cursor a -> Cursor b
mapWithIndex f (Cursor index xs) = Cursor index (Array.mapWithIndex f xs)

null :: forall a. Cursor a -> Boolean
null (Cursor _ xs) = Array.null xs

length :: forall a. Cursor a -> Int
length (Cursor _ xs) = Array.length xs

current :: forall a. Cursor a -> Maybe a
current (Cursor index xs) = Array.index xs index

getIndex :: forall a. Cursor a -> Int
getIndex (Cursor index _) = index

setIndex :: forall a. Int -> Cursor a -> Cursor a
setIndex newIndex (Cursor _ xs) = clamp $ Cursor newIndex xs

left :: forall a. Cursor a -> Cursor a
left (Cursor index xs) = clamp $ Cursor (index - 1) xs

right :: forall a. Cursor a -> Cursor a
right (Cursor index xs) = clamp $ Cursor (index + 1) xs

first :: forall a. Cursor a -> Cursor a
first (Cursor _ xs) = Cursor 0 xs

last :: forall a. Cursor a -> Cursor a
last (Cursor _ xs) = Cursor (Array.length xs - 1) xs
