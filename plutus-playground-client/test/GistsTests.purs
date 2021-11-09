module GistsTests
  ( all
  ) where

import Prologue
import Gist (GistId(..))
import Gists.Types (parseGistUrl)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

all :: Spec Unit
all =
  describe "Gists" do
    parseGistUrlTests

parseGistUrlTests :: Spec Unit
parseGistUrlTests =
  describe "parseGistUrlTests" do
    let
      gistId = GistId "9d8feacacd8c4b553f870c4448483938"
    it "Ref" do
      parseGistUrl "9d8feacacd8c4b553f870c4448483938"
        `shouldEqual`
          Right gistId
    it "Direct link" do
      parseGistUrl "https://gist.github.com/9d8feacacd8c4b553f870c4448483938"
        `shouldEqual`
          Right gistId
    it "User link" do
      parseGistUrl "https://gist.github.com/krisajenkins/9d8feacacd8c4b553f870c4448483938"
        `shouldEqual`
          Right gistId
    it "No ID" do
      parseGistUrl "https://gist.github.com/"
        `shouldEqual`
          Left "Could not parse Gist Url"
    it "Too long" do
      parseGistUrl "aaaabbbbccccddddeeeeffff000011112"
        `shouldEqual`
          Left "Could not parse Gist Url"
