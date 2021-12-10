-- |
-- Module      : Gauge.CSV
-- Copyright   : (c) 2017 Vincent Hanquez
--
-- a very simple CSV printer
-- 
-- import qualified for best result
--
module Gauge.CSV
    ( Row(..)
    , outputRow
    , Field
    , float
    , integral
    , string
    , write
    ) where

import Data.List (intercalate)

-- | a CSV Field (numerical or string)
--
-- The content inside is properly escaped
newtype Field = Field { unField :: String }
    deriving (Show,Eq)

-- | A Row of fields
newtype Row = Row [Field]
    deriving (Show,Eq)

-- | Create a field from Double
float :: Double -> Field
float d = Field $ show d

-- | Create a field for numerical integral
integral :: Integral a => a -> Field
integral i = Field $ show (toInteger i)

-- | Create a field from String
string :: String -> Field
string s =
    -- potentially a random string need to be escape,
    -- first find out how it need to escaped, then
    -- escape the data properly.
    case needEscape NoEscape s of
        NoEscape       -> Field s
        Escape         -> Field ('"' : (s ++ "\""))
        EscapeDoubling -> Field ('"' : doubleQuotes s)
  where
    needEscape EscapeDoubling _      = EscapeDoubling
    needEscape e              []     = e
    needEscape e              (x:xs)
        | x == '"'                   = EscapeDoubling
        | x `elem` toEscape          = needEscape (max e Escape) xs
        | otherwise                  = needEscape e              xs

    toEscape = ",\r\n"

    doubleQuotes [] = ['"']
    doubleQuotes (x:xs)
        | x == '"'  = '"':'"':doubleQuotes xs
        | otherwise = x : doubleQuotes xs

-- | Output a row to a String
outputRow :: Row -> String
outputRow (Row fields) = intercalate "," $ map unField fields

-- | 3 Possible modes of escaping:
-- * none
-- * normal quotes escapes
-- * content need doubling because of double quote in content
data Escaping = NoEscape | Escape | EscapeDoubling
    deriving (Show,Eq,Ord)

write :: Maybe FilePath
      -> Row
      -> IO ()
write Nothing _   = return ()
write (Just fp) r = appendFile fp (outputRow r ++ "\r\n")
