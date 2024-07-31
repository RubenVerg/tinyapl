#!/bin/bash

if [[ $PWD == */js ]]; then
	echo "build.sh must be ran in the root tinyapl directory"
	exit 1
fi

rm -rf js/dist
mkdir js/dist

wasm32-wasi-cabal build tinyapl-js

out_path=$(find . -name "*-js.wasm")

echo "Found $out_path"

"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs --input "$out_path" --output js/dist/ghc_wasm_jsffi.js

cp $out_path js/dist/tinyapl-js.wasm

cp js/tinyapl.js js/dist
cp js/*.html js/dist