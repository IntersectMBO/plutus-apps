Servant Purescript
==================

Generate typed PureScript query functions for your servant-api. To see how the
resulting client code looks, look at the golden tests.

## Features

 - Typed serialization/deserialization, taken care of by Haskell's aeson and PureScript's argonaut.
 - You can put common parameters like Auth tokens and the base URL in a reader monad, so you don't
   have to pass them explicitly. This is configurable in the code generator with `readerParams` in `Settings`.
   
## Usage

Apart from the above basic usage example, the documentation is still lacking.

## Status

It works!

For type translations purescript-bridge is used.
