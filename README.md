# purescript-bridge

Translate your Haskell types to PureScript types.

For compatible JSON representations you should be using [aeson](http://hackage.haskell.org/package/aeson)'s generic encoding/decoding with default options
and `gAesonEncodeJson` and `gAesonDecodeJson` from the [purescript-argonaut-codecs](https://github.com/purescript-contrib/purescript-argonaut-codecs)
package, (Data.Argonaut.Aeson).

At the time of this writing the PR providing `Data.Argonaut.Aeson` was not yet merged.
In the meantime, you can find the PR
[here](https://github.com/purescript-contrib/purescript-argonaut-codecs/pull/12).

Usage of this library is documented in [`Language.Purescript.Bridge`](http://hackage.haskell.org/package/purescript-bridge/docs/Language-PureScript-Bridge.html).

All you should need to get started is: [`writePSTypes`](http://hackage.haskell.org/package/purescript-bridge/docs/Language-PureScript-Bridge.html#writePSTypes).

You can customize data type translation by providing your own `TypeBridge`.

This library is at a really early stage. It works for my use case at the moment and I will fix bugs when they come along.

Expect bugs - especially for more advanced use cases!
