let upstream =
  https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211005/packages.dhall sha256:2ec351f17be14b3f6421fbba36f4f01d1681e5c7f46e0c981465c4cf222de5be

let overrides = {=}

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
    , repo = "https://github.com/input-output-hk/purescript-bridge-json-helpers.git"
    , version = "68265aaacc1a56c00a7625d424ff13d619681e5e"
    }
  , web-common =
      { dependencies =
          [ "aff"
          , "aff-promise"
          , "affjax"
          , "argonaut-codecs"
          , "argonaut-core"
          , "arrays"
          , "avar"
          , "bifunctors"
          , "bigints"
          , "concurrent-queues"
          , "console"
          , "control"
          , "coroutines"
          , "datetime"
          , "debug"
          , "dom-indexed"
          , "effect"
          , "either"
          , "enums"
          , "exceptions"
          , "filterable"
          , "foldable-traversable"
          , "foreign"
          , "foreign-object"
          , "free"
          , "freeap"
          , "freet"
          , "functions"
          , "halogen"
          , "halogen-subscriptions"
          , "identity"
          , "integers"
          , "json-helpers"
          , "lists"
          , "markdown"
          , "maybe"
          , "newtype"
          , "nullable"
          , "ordered-collections"
          , "parallel"
          , "prelude"
          , "profunctor"
          , "profunctor-lenses"
          , "rationals"
          , "servant-support"
          , "strings"
          , "tailrec"
          , "transformers"
          , "tuples"
          , "undefinable"
          , "unfoldable"
          , "unsafe-coerce"
          , "uuid"
          , "web-dom"
          , "web-events"
          , "web-html"
          , "web-socket"
          , "web-uievents"
          ]
    , repo = "https://github.com/input-output-hk/web-common.git"
    , version = "048b6423f926a84b3587827ac89442e465fcdbf9"
    }
  , matryoshka =
    { dependencies =
        [ "prelude", "fixed-points", "free", "transformers", "profunctor" ]
    , repo = "https://github.com/slamdata/purescript-matryoshka.git"
    , version = "v0.4.0"
    }
  , numerics =
    { dependencies =
        [ "prelude", "integers", "rationals", "uint", "bigints" ]
    , repo = "https://github.com/Proclivis/purescript-numerics"
    , version = "v0.1.2"
    }
  }

in  upstream // overrides // additions
