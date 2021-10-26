{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "plutus-pab-client"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "coroutines"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign-generic"
  , "foreign-object"
  , "gen"
  , "halogen"
  , "halogen-subscriptions"
  , "http-methods"
  , "integers"
  , "json-helpers"
  , "lists"
  , "matryoshka"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "ordered-collections"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "quickcheck"
  , "remotedata"
  , "servant-support"
  , "strings"
  , "transformers"
  , "tuples"
  , "web-common"
  , "web-socket"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , "generated/**/*.purs"
  , "web-common-plutus/**/*.purs"
  ]
}
