module Main where

import           Data.List            (intercalate)


import           Control.Applicative  hiding (many, some)
import           Control.Monad
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

{- What do we want?

So we want some form of automatic generation of js bindings to emcc generated
code, such that we can trivially interface ghcjs with c code.

The key is marshalling to and from emcc. We'll still require to write some
signatures for now, and will derive the binding frmo them.  Maybe in the future
we'll be able to extract that from haskell.

Signatures look like the following:

f :: arg1:type1 -> arg2:type2 -> ...

arg1, arg2, ... are the names to use (for descriptive purposes).

the argument types type1, type2, ... can be of the following form:
- [u]int[8|16|32|64]
- cstr (a \0 terminated string)
- buf[N], where N denotes the size of a buffer. N can be a function or any
  sensible expression. If N is omitted, we assume a pointer, which defaults to
  4 bytes (32bit). If an argument with the same name and the `_len` suffix is
  given, we assume that to contain the length info about the buffer.
- dbl, flt, for dobule and floats.

an optional `out` can be prefixed to an argument to indicate that we need
to copy the values back into the argument.

Example
in the c file we find:

#define CHACHA_KEY_SIZE 32
...
int foundation_rngV1_generate(uint8_t newkey[CHACHA_KEY_SIZE], uint8_t *dst, uint8_t key[CHACHA_KEY_SIZE], FsCountOf bytes)

The corresponding haskell import:
foreign import ccall unsafe "foundation_rngV1_generate"
   c_rngv1_generate :: Ptr Word8 -- new key
                    -> Ptr Word8 -- destination
                    -> Ptr Word8 -- current key
                    -> CountOf Word8 -- number of bytes to generate
                    -> IO Word32

CountOf is just facy way of specifying an Int (number of word8s aka bytes).

So our signature now looks like this

foundation_rngV1_generate :: newkey:uint8[CHACHA_KEY_SIZE] -> out dst:uint8[] -> key:uint8[CHACHA_KEY_SIZE] -> dst_len:int -> int

which will generate the following binding:
function h$foundation_rngV1_generate(newkey_d, newkey_o, dst_d, dst_o, key_d, key_o, dst_len) {
    return h$withCBuffer(newkey_d, newkey_o, CHACHA_KEY_SIZE, function(newkey) {
        return h$withOutBuffer(dst_d, dst_o, dst_len, function(dst) {
            return h$withCBuffer(key_d, key_o, CHACHA_KEY_SIZE, function(key) {
                return _foundation_rngV1_generate(newkey, dst, key, dst_len);
            });
        });
    });
}
-}

data Direction = In | Out deriving (Eq, Show)
type BufferSize = Maybe String

data Type
    = UInt8 | UInt16 | UInt32 | UInt64 | UInt
    | Int8  | Int16  | Int32  | Int64  | Int
    | Flt | Dbl
    | Buffer Direction Type BufferSize
    deriving (Eq, Show)

data Argument
    = Argument String Type
    deriving (Eq, Show)

data Function
    = Function String [Argument] Type
    deriving (Eq, Show)

type Parser = Parsec Void String

pType :: Parser Type
pType = choice
    [ UInt8  <$ string "uint8"
    , UInt16 <$ string "uint16"
    , UInt32 <$ string "uint32"
    , UInt64 <$ string "uint64"
    , UInt   <$ string "uint"
    , Int8   <$ string "int8"
    , Int16  <$ string "int16"
    , Int32  <$ string "int32"
    , Int64  <$ string "int64"
    , Int    <$ string "int"
    , Flt    <$ string "float"
    , Dbl    <$ string "double"
    ]

pBuffer :: Parser Type
pBuffer = Buffer In <$> pType <*> (Nothing <$ string "[]" <|> Just <$> (char '[' *> some (alphaNumChar <|> char '_') <* char ']'))

pArgument :: Parser Argument
pArgument = Argument <$> (some (alphaNumChar <|> char '_') <* char ':') <*> (try pBuffer <|> pType)

pFun :: Parser Function
pFun = do
    name <- some (alphaNumChar <|> char '_')
    void space1
    void (string "::")
    void space1
    args <- many (try $ pArgument <* space1 <* string "->" <* space1)
    -- void space1
    -- void (string "->")
    -- void space1
    rty <- (try pBuffer <|> pType)
    return $ Function name args rty

{-
Our example function would then end up in an AST like this:

Function "foundation_rngV1_generate"
         [ Argument "newkey" (Buffer In (Just "CHACHA_KEY_SIZE"))
         , Argument "dst" (Buffer Out (Just "dst_len"))
         , Argument "key" (Buffer In (Just "CHACHA_KEY_SIZE"))
         , Argument "dst_len" Int ]
         Int
}
-}
--
-- For 64bit values and ptrs we need to
-- legalize the values into a pair of high and low 32bits.
-- For buffers we need to have this as well with the offset and HEAP8.
is64bit :: Type -> Bool
is64bit UInt64 = True
is64bit Int64  = True
is64bit _      = False

isPtr :: Type -> Bool
isPtr (Buffer _ _ _) = True
isPtr _              = False

needLegalization :: Type -> Bool
needLegalization ty = is64bit ty || isPtr ty

renderArg :: Argument -> [String]
renderArg (Argument aname ty) | not (needLegalization ty) = [aname]
renderArg (Argument aname UInt64)         = [aname <> "_msw", aname <> "_lsw"]
renderArg (Argument aname Int64)          = [aname <> "_msw", aname <> "_lsw"]
renderArg (Argument aname (Buffer _ _ _)) = [aname <> "_d", aname <> "_o"]

renderBinding :: Function -> String
renderBinding f@(Function fname args rty) = intercalate "\n" [ prefix f, go padding [] args, postfix f ]
    where go :: String -> [String] -> [Argument] -> String
          go pad params [] | is64bit rty = intercalate "\n"
            [ pad <> "h$ret1 = _" <> fname <> "(" <> intercalate ", " (reverse params) <> ");"
            , pad <> "return getTempRet0();" ]
          go pad params [] | isPtr rty = intercalate "\n"
            [ pad <> "h$ret1 = _" <> fname <> "(" <> intercalate ", " (reverse params) <> ");"
            , pad <> "return { dv: new DataView(HEAPU8.buffer), u8: HEAPU8 };" ]
          go pad params [] = pad <> "return _" <> fname <> "(" <> intercalate ", " (reverse params) <> ");"
          go pad params (Argument aname ty:as) | needLegalization ty == False
            = go pad (aname:params) as
          -- not the swapping of most and least significant words when passing legalized
          -- 64bit values to emcc.  Remember @params@ is being reversed!
          go pad params (Argument aname UInt64:as)
            = go pad (aname <> "_msw":aname <> "_lsw":params) as
          go pad params (Argument aname Int64:as)
            = go pad (aname <> "_msw":aname <> "_lsw":params) as

          go pad params (Argument aname (Buffer In _ (Just sz)):as)
            = intercalate "\n"
                [ pad <> "return h$withCBuffer(" <> aname <> "_d, " <> aname <> "_o, " <> sz <> ", function(" <> aname <> ") {"
                , go (padding <> pad) (aname:params) as
                , pad <> ")};" ]
          go pad params (Argument aname (Buffer Out _ (Just sz)):as)
            = intercalate "\n"
                [ pad <> "return h$withOutBuffer(" <> aname <> "_d, " <> aname <> "_o, " <> sz <> ", function(" <> aname <> ") {"
                , go (padding <> pad) (aname:params) as
                , pad <> ")};" ]
          go pad params f = error $ "boom; " <> show f

          padding :: String
          padding = "  "

          prefix :: Function -> String
          prefix (Function fname args _) = "function h$" <> fname <> "(" ++ intercalate ", " (concatMap renderArg args) ++ "){"
          postfix :: Function -> String
          postfix _ = "}"

parseSig :: String -> String
parseSig sig = case parse pFun "Signature" sig of
    Right fun -> renderBinding (fixupSig fun)
    Left e    -> errorBundlePretty e

fixupSig :: Function -> Function
fixupSig (Function fname args rty)
    = Function fname (go args) rty
    where go :: [Argument] -> [Argument]
          go [] = []
          go (Argument aname (Buffer dir bty Nothing):as) =
              if (aname <> "_len" `notElem` argnames) then error $ "no implicit length given for argument " <> aname
              else (Argument aname (Buffer dir bty (Just $ aname <> "_len"))):go as
          go (a:as) = a:go as
          argnames :: [String]
          argnames = [aname | (Argument aname _) <- args]

main :: IO ()
main = putStrLn "Hello, Haskell!"
