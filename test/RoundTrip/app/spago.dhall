{ name = "my-project"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  , "console"
  , "effect"
  , "either"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
