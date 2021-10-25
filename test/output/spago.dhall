{ name = "output"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut-codecs"
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
