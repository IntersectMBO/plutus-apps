module Main where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Node.Process (exit)
import Node.ReadLine (createConsoleInterface, noCompletion, question)
import RoundTrip.Types (TestData)

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  interface # question "" \input ->
    let
      parsed :: Either JsonDecodeError TestData
      parsed = decodeJson =<< parseJson input
    in
      case parsed of
        Left err -> do
          log $ "got" <> input
          log $ printJsonDecodeError err
          exit 1
        Right testData -> do
          log $ stringify $ encodeJson testData
          exit 0
