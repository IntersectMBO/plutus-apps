let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211005/packages.dhall sha256:2ec351f17be14b3f6421fbba36f4f01d1681e5c7f46e0c981465c4cf222de5be

in  upstream // {
  json-helpers =
    { dependencies =
      [ "aff"
      , "argonaut-codecs"
      , "argonaut-core"
      , "arrays"
      , "bifunctors"
      , "contravariant"
      , "control"
      , "effect"
      , "either"
      , "enums"
      , "foreign-object"
      , "maybe"
      , "newtype"
      , "ordered-collections"
      , "prelude"
      , "profunctor"
      , "psci-support"
      , "quickcheck"
      , "record"
      , "spec"
      , "spec-quickcheck"
      , "transformers"
      , "tuples"
      , "typelevel-prelude"
      ]
    , repo = "https://github.com/input-output-hk/purescript-bridge-json-helpers.git"
    , version = "0556c336c06fea6e388946271ae8965e775e3ed0"
    }
}
