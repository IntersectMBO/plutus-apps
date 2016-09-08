# purescript-bridge


[![Build Status](https://travis-ci.org/eskimor/purescript-bridge.svg?branch=master)](https://travis-ci.org/eskimor/purescript-bridge)



Translate your Haskell types to PureScript types. It should in theory work for almost all Haskell types, including type constructors!
You just have to instantiate it with dummy parameters from e.g. "Language.PureScript.Bridge.TypeParameters".

Data type translation is fully and easily customizable by providing your own `BridgePart` instances!

## JSON encoding / decoding

For compatible JSON representations you should be using [aeson](http://hackage.haskell.org/package/aeson)'s generic encoding/decoding with default options
and `encodeJson` and `decodeJson` from "Data.Argonaut.Generic.Aeson" in [purescript-argonaut-generic-codecs](https://github.com/eskimor/purescript-argonaut-generic-codecs).


## Documentation

Usage of this library is documented in `Language.Purescript.Bridge`, with `writePSTypes` you should have everything to get started. Documentation can be found [here](https://www.stackage.org/nightly/package/purescript-bridge).

## Status

It works for my use case and is used in production. PRs for more `PSType`s definitions and bridges are very welcome! 
