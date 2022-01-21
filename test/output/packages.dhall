let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211109/packages.dhall sha256:e8d8d5b339f6d46d950da90037c6c38e8809f7e34f727373089ab82c080fc709

let additions =
      { servant-support =
        { dependencies =
          [ "aff"
          , "affjax"
          , "argonaut"
          , "arrays"
          , "bifunctors"
          , "either"
          , "newtype"
          , "prelude"
          , "psci-support"
          , "strings"
          , "transformers"
          , "uri"
          ]
        , repo = "https://github.com/input-output-hk/purescript-servant-support"
        , version = "1fcf4e93723f7f5d36476735b76888938ced871e"
        }
      , json-helpers =
        { dependencies =
          [ "argonaut-codecs"
          , "argonaut-core"
          , "arrays"
          , "bifunctors"
          , "contravariant"
          , "control"
          , "either"
          , "enums"
          , "foreign-object"
          , "maybe"
          , "newtype"
          , "ordered-collections"
          , "prelude"
          , "profunctor"
          , "psci-support"
          , "record"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          ]
        , repo =
            "https://github.com/input-output-hk/purescript-bridge-json-helpers.git"
        , version = "68265aaacc1a56c00a7625d424ff13d619681e5e"
        }
      }

in  upstream // additions
