{ name = "output"
, dependencies =
  [ "aff"
  , "affjax"
  , "console"
  , "effect"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "./ServerAPI.purs", "./ServerTypes.purs" ]
}
