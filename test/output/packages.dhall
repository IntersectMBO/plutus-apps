let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211005/packages.dhall sha256:2ec351f17be14b3f6421fbba36f4f01d1681e5c7f46e0c981465c4cf222de5be

let additions =
      { servant-support =
          { dependencies =
            [ "console"
            , "prelude"
            , "either"
            , "foldable-traversable"
            , "effect"
            , "aff"
            , "affjax"
            , "exceptions"
            , "web-xhr"
            ]
          , repo = "https://github.com/input-output-hk/purescript-servant-support"
          , version = "1805f896560751c48a04d3e29f9c109df850d8d3"
          }
      }

in  upstream // additions
