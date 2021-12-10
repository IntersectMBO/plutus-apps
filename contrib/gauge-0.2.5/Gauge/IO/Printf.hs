-- |
-- Module      : Gauge.IO.Printf
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Input and output actions.

{-# LANGUAGE CPP, FlexibleInstances, Rank2Types, TypeSynonymInstances #-}
module Gauge.IO.Printf
    (
      CritHPrintfType
    , note
    , printError
    , prolix
    , rewindClearLine
    ) where

import Control.Monad (when)
import Gauge.Monad (Gauge, askConfig, gaugeIO)
import Gauge.Main.Options (Config(verbosity), Verbosity(..))
import System.IO (Handle, hFlush, stderr, stdout)
import Text.Printf (PrintfArg)
import qualified Text.Printf (HPrintfType, hPrintf)

-- First item is the action to print now, given all the arguments
-- gathered together so far.  The second item is the function that
-- will take a further argument and give back a new PrintfCont.
data PrintfCont = PrintfCont (IO ()) (forall a . PrintfArg a => a -> PrintfCont)

-- | An internal class that acts like Printf/HPrintf.
--
-- The implementation is visible to the rest of the program, but the
-- details of the class are not.
class CritHPrintfType a where
  chPrintfImpl :: (Config -> Bool) -> PrintfCont -> a


instance CritHPrintfType (Gauge a) where
  chPrintfImpl check (PrintfCont final _)
    = do x <- askConfig
         when (check x) (gaugeIO (final >> hFlush stderr >> hFlush stdout))
         return undefined

instance CritHPrintfType (IO a) where
  chPrintfImpl _ (PrintfCont final _)
    = final >> hFlush stderr >> hFlush stdout >> return undefined

instance (CritHPrintfType r, PrintfArg a) => CritHPrintfType (a -> r) where
  chPrintfImpl check (PrintfCont _ anotherArg) x
    = chPrintfImpl check (anotherArg x)

chPrintf :: CritHPrintfType r => (Config -> Bool) -> Handle -> String -> r
chPrintf shouldPrint h s
  = chPrintfImpl shouldPrint (make (Text.Printf.hPrintf h s)
                                   (Text.Printf.hPrintf h s))
  where
    make :: IO () -> (forall a r. (PrintfArg a, Text.Printf.HPrintfType r) =>
                      a -> r) -> PrintfCont
    make curCall curCall' = PrintfCont curCall (\x -> make (curCall' x)
                                                      (curCall' x))

{- A demonstration of how to write printf in this style, in case it is
ever needed
  in fututre:

cPrintf :: CritHPrintfType r => (Config -> Bool) -> String -> r
cPrintf shouldPrint s
  = chPrintfImpl shouldPrint (make (Text.Printf.printf s)
  (Text.Printf.printf s))
  where
    make :: IO () -> (forall a r. (PrintfArg a, Text.Printf.PrintfType r) => a -> r) -> PrintfCont
    make curCall curCall' = PrintfCont curCall (\x -> make (curCall' x) (curCall' x))
-}

-- | Print a \"normal\" note.
note :: (CritHPrintfType r) => String -> r
note = chPrintf ((> Quiet) . verbosity) stdout

-- | Print verbose output.
prolix :: (CritHPrintfType r) => String -> r
prolix = chPrintf ((== Verbose) . verbosity) stdout

-- | Print an error message.
printError :: (CritHPrintfType r) => String -> r
printError = chPrintf (const True) stderr

-- | ansi escape on unix to rewind and clear the line to the end
rewindClearLine :: String
#ifdef mingw32_HOST_OS
rewindClearLine = "\n"
#else
rewindClearLine = "\r\ESC[0K"
#endif

