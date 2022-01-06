{ name = "output"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "control"
  , "either"
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
  ]
, packages = ./packages.dhall
, sources = [ "./ServerAPI.purs", "./ServerTypes.purs" ]
}
