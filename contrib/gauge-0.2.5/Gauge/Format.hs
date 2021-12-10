-- |
-- Module      : Gauge.Format
-- Copyright   : (c) 2017 Vincent Hanquez
-- 
-- Formatting helpers
--
-- shame there's no leftPad package to use. /s
--
module Gauge.Format
    ( printNanoseconds
    , printSubNanoseconds
    , tableMarkdown
    , reset
    , green
    , red
    , yellow
    ) where

import Gauge.Time
import Data.List
import Data.Word
import Text.Printf
import qualified Basement.Terminal.ANSI as ANSI
import           Basement.Bounded (zn64)
import           GHC.Exts (toList)

-- | Print a NanoSeconds quantity with a human friendly format
-- that make it easy to compare different values
--
-- Given a separator Char of '_':
--
-- 0           -> "             0"
-- 1000        -> "         1_000"
-- 1234567     -> "     1_234_567"
-- 10200300400 -> "10_200_300_400"
--
-- Note that the seconds parameters is aligned considered
-- maximum of 2 characters (i.e. 99 seconds).
-- 
printNanoseconds :: Maybe Char -> NanoSeconds -> String
printNanoseconds thousandSeparator (NanoSeconds absNs) =
    case divSub1000 0 absNs of
        [ns]         -> padLeft maxLength $ printSpace ns
        [ns,us]      -> padLeft maxLength $ addSeparators1000 [printSpace us,print3 ns]
        [ns,us,ms]   -> padLeft maxLength $ addSeparators1000 [printSpace ms,print3 us,print3 ns]
        [ns,us,ms,s] -> padLeft maxLength $ addSeparators1000 [printSpace s,print3 ms,print3 us,print3 ns]
        _            -> error "printNanoSeconds: internal error: invalid format"
  where
    maxLength = 3 + 3 + 3 + 2 + (sepLength * 3)
  
    (addSeparators1000, sepLength) =
        case thousandSeparator of
            Nothing -> (concat, 0)
            Just c  -> (intercalate [c], 1)

    printSpace :: Word64 -> String
    printSpace n = printf "%3d" n
    print3 :: Word64 -> String
    print3 n = printf "%03d" n

    divSub1000 :: Int -> Word64 -> [Word64]
    divSub1000 n i
        | n == 3    = [i]
        | otherwise =
            let (d,m) = i `divMod` 1000
             in if d == 0 then [m] else m : divSub1000 (n+1) d

printSubNanoseconds :: Maybe Char -> PicoSeconds100 -> String
printSubNanoseconds ts p =
    printNanoseconds ts ns ++ "." ++ show fragment
  where
    (ns, fragment) = picosecondsToNanoSeconds p


-- | Produce a table in markdown
--
-- This is handy when wanting to copy paste to a markdown flavor destination.
tableMarkdown :: String     -- ^ top left corner label
              -> [String]   -- ^ columns labels
              -> [[String]] -- ^ a list of row labels followed by content rows
              -> String     -- ^ the resulting string
tableMarkdown name cols rows =
    let hdr = "| " ++ intercalate " | " (padList (name : cols)) ++ " |\n"
        sep = "|-" ++ intercalate "-|-" (map (map (const '-')) (padList (name : cols))) ++ "-|\n"
     in hdr ++ sep ++ concatMap printRow (map padList rows)
  where
    printRow :: [String] -> String
    printRow l = "| " ++ intercalate " | " l ++ " |\n"

    getColN n = map (flip (!!) n) rows

    sizeCols :: [Int]
    sizeCols = map (\(i, c) -> maximum $ map length (c : getColN i)) $ zip [0..] (name : cols)

    padList l = zipWith padCenter sizeCols l

padLeft :: Int -> String -> String
padLeft sz s
    | sz <= len = s
    | otherwise = replicate leftPad ' ' ++ s
  where
    len = length s
    leftPad = (sz - len)

padCenter :: Int -> String -> String
padCenter sz s
    | sz <= len = s
    | otherwise = replicate leftPad ' ' ++ s ++ replicate rightPad ' '
  where
    len = length s
    (leftPad, r) = (sz - len) `divMod` 2
    rightPad = leftPad + r

-- | reset, green, red, yellow ANSI escape
reset, green, red, yellow :: String
reset = toList ANSI.sgrReset
green = toList $ ANSI.sgrForeground (zn64 2) True
red = toList $ ANSI.sgrForeground (zn64 1) True
yellow = toList $ ANSI.sgrForeground (zn64 3) True
