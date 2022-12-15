{-# LANGUAGE OverloadedStrings #-}

module Spec.Cli where

import Control.Monad (forM_)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative (ParserInfo, ParserResult (CompletionInvoked, Failure, Success), defaultPrefs,
                            execParserPure, renderFailure, (<**>))
import Options.Applicative qualified as OptA
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

import Marconi.CLI (Options, optionsParser)

tests :: TestTree
tests = testGroup "marconi-cli all tests" [
    genTest commands | commands <- [
            []
            , ["--help"]
            , ["--socket"]]]


opts :: OptA.ParserInfo Options
opts = OptA.info (optionsParser <**> OptA.helper) OptA.idm

run :: ParserInfo a -> [String] -> ParserResult a
run = execParserPure defaultPrefs

-- | Test generate golden tests from the list of commands
genTest :: [T.Text] -> TestTree
genTest commands = do
  let goldenFile = T.unpack $
        "test/Spec/Golden/Cli/"
          <> T.intercalate "_" ("marconi" : (  T.replace "-" "_"  <$> commands))
          <> ".help"

  goldenVsStringDiff
    ( T.unpack $ T.unwords commands)
    (\expected actual -> ["diff", "--color=always", expected, actual])
    goldenFile
    (generateHelpScreen commands)

-- | Test generate  cli tests and parse the help screen.
-- All generated tests are bad cli invocations that will result in
--   - printng the cli produced error
--   - help screen
generateHelpScreen :: [T.Text] -> IO BSL.ByteString
generateHelpScreen commands = do
  let
      commandLine = commands
      text = case execParserPure defaultPrefs opts  (T.unpack <$>commandLine) of
        Failure failure     -> failure
        Success _           -> error "Parser expected to fail"
        CompletionInvoked _ -> error "Parser expected to fail"
  pure $ BSL.fromStrict ( encodeUtf8 . T.pack  <$> fst $ renderFailure text "marconi")
