#!/usr/bin/env bash

# build the JavaScript distribution after building pab-mktx-lib with GHCJS

# run the test/example program using nodejs: node mktx-lib-test.js


DIST=dist/build/pab-mktx-lib/pab-mktx-lib.jsexe/
JSBITS=mktx/jsbits

echo "building mktx-lib distribution"

# build the library  
cat "$DIST/rts.js" "$DIST/lib.js" "$DIST/out.js" "$JSBITS/bits.js" "$JSBITS/init.js" > mktx-lib.js

# TODO: minify?

# build the test program"
cat "mktx-lib.js" "$JSBITS/test.js" > "mktx-lib-test.js"
