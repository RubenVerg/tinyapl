#!/bin/bash

set -e

if [[ $PWD == */app ]]; then
	echo "build.sh must be ran in the root tinyapl directory"
	exit 1
fi

rm -rf dist
mkdir dist

echo "Compiling executable"

wasm32-wasi-cabal build exe:tinyapl

# out_path=$(find dist-newstyle -name "tinyapl.wasm")
out_path=$(wasm32-wasi-cabal list-bin tinyapl | tail -n1)

echo "Compiled, embedding standard library"

wizer --allow-wasi --wasm-bulk-memory true $out_path -o dist/tinyapl.wasm --mapdir /std::./std
