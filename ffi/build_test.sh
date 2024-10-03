#!/bin/bash

set -e

rm -rf ffi/dist
mkdir ffi/dist

gcc -fPIC -rdynamic -shared -o ffi/dist/test.so ffi/test.c