let upstream =
      https://github.com/input-output-hk/purescript-web-common/releases/download/v1.0.1/packages.dhall sha256:f4ca6c359c375f3267b72696d9ed429a663d0a5a9adbe540d58e1451455df368

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
