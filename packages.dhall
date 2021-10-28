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
    , version = "78a0693a9409bce31bf538a8f2aa54adb8200733"
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
    , version = "895db00f2fe97ee56b866bf1582b303d029c216a"
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
    , version = "2f02fa30f7dcd06568aabaa8921b9beed26923ff"
    }
  , matryoshka =
    { dependencies =
        [ "prelude", "fixed-points", "free", "transformers", "profunctor" ]
    , repo = "https://github.com/slamdata/purescript-matryoshka.git"
    , version = "v0.4.0"
    }
  , markdown =
    { dependencies =
        [ "const", "datetime", "functors", "lists", "ordered-collections", "parsing", "partial", "precise", "prelude", "strings", "unicode", "validation" ]
    , repo = "https://github.com/jhbertra/purescript-markdown"
    , version = "a9fbc4c42acf7b4be908832698b69ed558912496"
    }
  , numerics =
    { dependencies =
        [ "prelude", "integers", "rationals", "uint", "bigints" ]
    , repo = "https://github.com/Proclivis/purescript-numerics"
    , version = "v0.1.2"
    }
  }

in  upstream // overrides // additions
