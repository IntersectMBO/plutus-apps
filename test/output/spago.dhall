{ name = "output"
, dependencies =
  [ "affjax"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "control"
  , "either"
  , "foldable-traversable"
  , "http-methods"
  , "json-helpers"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "servant-support"
  , "strings"
  , "transformers"
  , "tuples"
  , "uri"
  ]
, packages = ./packages.dhall
, sources = ["./src/**/*.purs"]
}
