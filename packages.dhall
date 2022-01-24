let upstream =
      https://github.com/input-output-hk/purescript-web-common/releases/download/v2.0.1/packages.dhall sha256:5b49e522293790f08e07e5ed4b59a5e91d213521e68e20148eabf81164c38664

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
