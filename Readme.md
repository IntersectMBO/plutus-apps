Servant Purescript
==================

[![Build Status](https://travis-ci.org/eskimor/servant-purescript.svg?branch=master)](https://travis-ci.org/eskimor/servant-purescript)

Generate typed PureScript query functions for your servant-api. Find an example, including
the generated code in [examples/central-counter](https://github.com/eskimor/servant-purescript/tree/master/examples/central-counter).

## Features

 - Typed serialization/deserialization taken care of by Haskell's aeson and PureScript's argonaut.
   Generic encoding/decoding of both made compatible by not yet merged
   [pull request](https://github.com/purescript-contrib/purescript-argonaut-codecs/pull/12).
 - You can put common parameters like Auth tokens and the base URL in a reader monad so you don't
   have to pass them explicitly. This is configurable in the code generator with `readerParams` in `Settings`.
   
## Status

It works!

Documentation is yet to come, but there is a usage example in examples/central-counter
which also employs a very early version of servant-subscriber
used for counter live updates.
The PureScript side of servant-subscriber is very low-level and will
change significantly in the future.

For type translations purescript-bridge is used.

I now have to take care of my actual project, which makes use of servant-purescript,
improvements and documentation will follow on the way!
