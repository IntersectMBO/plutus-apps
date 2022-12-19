module Main where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (error, log)
import Node.ReadLine (createConsoleInterface, noCompletion, question)
import RoundTrip.Types (TestData)

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  log "ready"
  go interface
  where
  go interface = 
    interface # question "" \input -> do
      let
        parsed :: Either JsonDecodeError TestData
        parsed = decodeJson =<< parseJson input
      case parsed of
        Left err -> do
          error $ "got " <> input
          error $ printJsonDecodeError err
          log ""
        Right testData -> do
          error ""
          log $ stringify $ encodeJson testData
      go interface
