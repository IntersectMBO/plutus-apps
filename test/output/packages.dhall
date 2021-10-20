let upstream =
    https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211005/packages.dhall sha256:2ec351f17be14b3f6421fbba36f4f01d1681e5c7f46e0c981465c4cf222de5be

let additions =
    { servant-support =
        { dependencies =
            [ "affjax"
            , "argonaut-codecs"
            , "argonaut-core"
            , "prelude"
            ]
        , repo = "https://github.com/input-output-hk/purescript-servant-support"
        , version = "93ea0fa97d0ba04e8d408bbba51749a92d6477f5"
        }
    }

in  upstream // additions
