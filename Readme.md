Servant Purescript
==================

[![Build Status](https://travis-ci.org/eskimor/servant-purescript.svg?branch=master)](https://travis-ci.org/eskimor/servant-purescript)

Generate typed PureScript query functions for your servant-api. Find an example, including
the generated code in [examples/central-counter](https://github.com/eskimor/servant-purescript/tree/master/examples/central-counter).

## Features

 - Typed serialization/deserialization, taken care of by Haskell's aeson and PureScript's argonaut.
   Generic encoding/decoding of both made compatible by 
   [purescript-argonaut-generic-codecs](https://github.com/eskimor/purescript-argonaut-generic-codecs/blob/purescript-argonaut-generic-codec/src/Data/Argonaut/Generic/Aeson.purs).
 - You can put common parameters like Auth tokens and the base URL in a reader monad, so you don't
   have to pass them explicitly. This is configurable in the code generator with `readerParams` in `Settings`.
   
## Usage

Apart from the above basic usage example, the documentation is still lacking. For
the purescript side make sure you have
[purescript-servant-support](https://github.com/eskimor/purescript-servant-support)
and
[purescript-argonaut-generic-codecs](https://github.com/eskimor/purescript-argonaut-generic-codecs)
installed, otherwise the generated code will not compile.

## Status

It works!

Documentation is yet to come, but there is a usage example in
examples/central-counter which also uses servant-subscriber for counter
live updates. Generated code is only partly tested, especially Query parameters
are still completely untested.

For type translations purescript-bridge is used.
