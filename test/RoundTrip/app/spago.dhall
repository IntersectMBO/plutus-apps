{ name = "my-project"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "control"
  , "effect"
  , "either"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-process"
  , "node-streams"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
