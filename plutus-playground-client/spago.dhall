{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "plutus-playground-client"
, dependencies =
  [ "aff"
  , "aff-coroutines"
  , "aff-promise"
  , "argonaut-codecs"
  , "bigints"
  , "concurrent-queues"
  , "console"
  , "coroutines"
  , "debug"
  , "effect"
  , "foreign-generic"
  , "halogen"
  , "json-helpers"
  , "matryoshka"
  , "node-fs"
  , "numerics"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "remotedata"
  , "servant-support"
  , "test-unit"
  , "undefinable"
  , "uuid"
  , "web-common"
  , "web-socket"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , "test/**/*.purs"
  , "generated/**/*.purs"
  , "web-common-plutus/**/*.purs"
  , "web-common-playground/**/*.purs"
  ]
}
