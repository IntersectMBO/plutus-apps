# purescript-bridge

Translate your Haskell types to PureScript types.

You get compatible JSON representations by using aeson's generic encoding/decoding with default options
and by using `gAesonEncodeJson` and `gAesonDecodeJson` from the [purescript-argonaut-codecs](https://github.com/purescript-contrib/purescript-argonaut-codecs)
package, (Data.Argonaut.Aeson).

At the time of this writing the PR providing `Data.Argonaut.Aeson` was not yet merged, you can find it
[here](https://github.com/purescript-contrib/purescript-argonaut-codecs/pull/12).

Usage of this library is documented in [`Language.Purescript.Bridge`](http://hackage.haskell.org/package/purescript-bridge/docs/Language-PureScript-Bridge.html).
All you should need to get started is: [`writePSTypes`](http://hackage.haskell.org/package/purescript-bridge-0.1.0.0/docs/Language-PureScript-Bridge.html#writePSTypes)

You can customize data type translation by providing your own `TypeBridge`.

This library is at a really early stage, it works for my use case at the moment and I will fix bugs when they come along.

Expect bugs - especially for more advanced use cases!
