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
    testGroup "marconi-cli invalid commands"
        [testCase "marconi-cli invalid command" testInValidClis
        , testCase "marconi-cli valid command" testValidClis
        ]
    , testGroup "marconi-cli generated golden tests"
        [genTest commands | commands <- [
                                         []
                                         , ["--help"]
                                         , ["--socket"]]]
        ]

-- | input files for this testGroup. The test group executes the tests in `batch`
validBatchFile, inValidBatchFile :: FilePath
validBatchFile      = "test/Spec/validClis.txt"
inValidBatchFile    = "test/Spec/invalidClis.txt"

-- | Parse the Cli commands from file
--
clisFromFile :: FilePath -> IO [(String, OptA.ParserResult Options)]
clisFromFile cliFile =
  mapM asOpt . lines =<< readFile cliFile
  where
    asOpt' =
      run opts
        . words
    asOpt line = pure (line, asOpt' line)


opts :: OptA.ParserInfo Options
opts = OptA.info (optionsParser <**> OptA.helper) OptA.idm

run :: ParserInfo a -> [String] -> ParserResult a
run = execParserPure defaultPrefs

-- | Test inValid CLIs from input file.
testInValidClis :: Assertion
testInValidClis = do
    ps <- clisFromFile inValidBatchFile
    forM_ ps $ \(aLine, input) ->
        case input of
            Success a ->
                assertBool
                ("Result `" <> show a <>  "` should've been invalid. CLI parser for: " <> show aLine <> " should fail!")
                False
            Failure pf ->
                assertBool
                ("CLI parser for: `" <> show aLine <> "` failed with: " <> show pf )
                True
            _ -> assertBool ("some other failure for the `invalid` cliInput of: `" <> aLine <> "`") False

-- | Test inValid CLIs from input file.
testValidClis :: Assertion
testValidClis = do
    ps <- clisFromFile validBatchFile
    print ps
    forM_ ps $ \(aLine, input) ->
        case input of
            Success a -> assertBool ("Result `" <> show a <>  "` should've been invalid. CLI parser for: " <> show aLine <> " should fail!") True
            Failure pf -> assertBool ("failure for the `valid` cliInput of: "
                                      <> show aLine <> " " <> show pf ) False
            _ ->  assertBool ("some other failure for the `invalid` cliInput of: `" <> aLine <> "`") False

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
    (goldenFile)
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
  -- pure $ BSL.fromStrict $ encodeUtf8 <$> fst $ renderFailure text "marconi"
  pure $ BSL.fromStrict ( encodeUtf8 . T.pack  <$> fst $ renderFailure text "marconi")
