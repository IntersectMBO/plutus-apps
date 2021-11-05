let upstream =
      https://github.com/input-output-hk/purescript-web-common/releases/download/v1.1.4/packages.dhall sha256:ab905184ee034c35867e73ace0475ce29d1a0b975581d355af6f35e680449749

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
