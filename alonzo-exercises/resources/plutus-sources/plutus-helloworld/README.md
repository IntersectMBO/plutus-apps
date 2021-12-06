### plutus-helloworld

This directory contains a simple "Hello World" script.  There are two versions: one using an integer literal (needed because the Plutus interpreter doesn't currently accept byte string literals) and a slighly more complicated one using a bytestring parameter.

``plutus-helloworld`` -- very simple numeric version

``plutus-helloworld-bytestring`` -- more compex typed version using a paramaterised validator and bytestring constant

``plutus-helloworld-literal-bytestring`` -- an untyped example using literal builtin bytestring (significantly smaller when serialised than typed validator)

``plutus-sum`` -- a typed validator where datum and redeemer are both integers. The validator succeed if `sum [1..datum] == redeemer`
