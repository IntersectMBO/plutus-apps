{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "plutus-pab-client"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut-codecs"
  , "avar"
  , "bigints"
  , "concurrent-queues"
  , "console"
  , "debug"
  , "effect"
  , "foreign-generic"
  , "halogen"
  , "json-helpers"
  , "matryoshka"
  , "newtype"
  , "node-fs"
  , "numerics"
  , "prelude"
  , "psci-support"
  , "remotedata"
  , "servant-support"
  , "test-unit"
  , "transformers"
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
  , "web-common/**/*.purs"
  , "web-common-plutus/**/*.purs"
  ]
}
