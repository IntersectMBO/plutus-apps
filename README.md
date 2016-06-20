# purescript-bridge


[![Build Status](https://travis-ci.org/eskimor/purescript-bridge.svg?branch=master)](https://travis-ci.org/eskimor/purescript-bridge)



Translate your Haskell types to PureScript types. It should in theory work for almost all Haskell types, including type constructors!
You just have to instantiate it with dummy parameters from e.g. "Language.PureScript.Bridge.TypeParameters".

Data type translation is fully and easily customizable by providing your own `BridgePart` instances!

## JSON encoding / decoding

For compatible JSON representations you should be using [aeson](http://hackage.haskell.org/package/aeson)'s generic encoding/decoding with default options
and `encodeJson` and `decodeJson` from "Data.Argonaut.Generic.Aeson" in [purescript-argonaut-generic-codecs](https://github.com/purescript-contrib/purescript-argonaut-codecs).


## Documentation

Usage of this library is documented in [`Language.Purescript.Bridge`](http://hackage.haskell.org/package/purescript-bridge/docs/Language-PureScript-Bridge.html).

All you should need to get started is: [`writePSTypes`](http://hackage.haskell.org/package/purescript-bridge/docs/Language-PureScript-Bridge.html#writePSTypes).

## Status

This library is at a quite early stage. It works for my use case at the moment and I will fix bugs when they come along. Also PRs for more `PSType`s definitions and bridges are very welcome!

Expect bugs - especially for more advanced use cases. Although I have tested the most advanced one already with no issues, bugs always creep in.
