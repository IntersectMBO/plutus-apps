{ name = "output"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "control"
  , "either"
  , "http-methods"
  , "json-helpers"
  , "maybe"
  , "newtype"
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
