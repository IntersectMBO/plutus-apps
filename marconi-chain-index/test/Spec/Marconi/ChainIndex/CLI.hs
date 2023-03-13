{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.ChainIndex.CLI (tests) where

import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative (ParserResult (CompletionInvoked, Failure, Success), defaultPrefs, execParserPure,
                            renderFailure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)

import Marconi.ChainIndex.CLI (programParser)

tests :: TestTree
tests = testGroup "marconi-chain-index CLI Specs" [
    genTest commands | commands <- [
        ["--disable-address-data"]  -- invalid command
        , ["--help"]                -- display help
        ]]

-- | Test generate golden tests from the list of commands
genTest :: [T.Text] -> TestTree
genTest commands = do
  let goldenFile = T.unpack $
        "test/Spec/Golden/Cli/"
          <> T.intercalate "_" ("marconi-chain-index" : (  T.replace "-" "_"  <$> commands))
          <> ".help"

  goldenVsStringDiff
    ( T.unpack $ T.unwords commands)
    (\expected actual -> ["diff", "--color=always", expected, actual])
    goldenFile
    (generateHelpScreen commands)

-- | Generate CLI tests and parse the help screen.
--
-- Generated tests are incomplete CLI invocations that will result in
--   * printing the CLI produced error
--   * help screen
generateHelpScreen :: [T.Text] -> IO ByteString
generateHelpScreen commands = do
  let parser =  programParser "fake-sha" -- parameter is ignored in this test
      text = case execParserPure defaultPrefs parser  (T.unpack <$>commands) of
        Failure failure     -> failure
        Success _           -> error "Parser expected to fail"
        CompletionInvoked _ -> error "Parser expected to fail"
  pure $ fromStrict ( encodeUtf8 . T.pack  <$> fst $ renderFailure text "marconi-chain-index")
