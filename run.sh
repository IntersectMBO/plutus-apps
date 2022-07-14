#!/usr/bin/env sh
until ! LANG=C.UTF-8 cabal test plutus-pab-executables:test:plutus-pab-test-full --test-options="-p \"can wait for tx\""; do
echo ...;
done
