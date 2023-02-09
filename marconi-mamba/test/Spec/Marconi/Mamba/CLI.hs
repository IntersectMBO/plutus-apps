{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.Mamba.CLI where

import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative (ParserResult (CompletionInvoked, Failure, Success), defaultPrefs, execParserPure, info,
                            renderFailure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)

import Marconi.Mamba.CLI (parserCliArgs)

tests :: TestTree
tests = testGroup "marconi-mamba CLI Specs" [
    genTest commands | commands <- [
            []
            , ["--help"]
            , ["--socket"]
            , ["--addresses-to-index"]
            , ["--http-port"]
            , ["--utxo-db"]]]

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
-- Generated tests are incomplete CLI invocations that will result in
--   - printng the cli produced error
--   - help screen
generateHelpScreen :: [T.Text] -> IO ByteString
generateHelpScreen commands = do
  let parser = info parserCliArgs mempty
      text = case execParserPure defaultPrefs parser  (T.unpack <$>commands) of
        Failure failure     -> failure
        Success _           -> error "Parser expected to fail"
        CompletionInvoked _ -> error "Parser expected to fail"
  pure $ fromStrict ( encodeUtf8 . T.pack  <$> fst $ renderFailure text "marconi-mamba")
