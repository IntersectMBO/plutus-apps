{ name = "nami-demo-app"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "arraybuffer"
  , "console"
  , "effect"
  , "halogen"
  , "halogen-formless"
  , "node-buffer"
  , "node-fs"
  , "node-process"
  , "prelude"
  , "psci-support"
  , "web-common"
  ]
, packages = ../../../../packages.dhall
, sources =
  [ "src/**/*.purs"
  , "test/**/*.purs"
  , "generated/**/*.purs"
  ]
}
