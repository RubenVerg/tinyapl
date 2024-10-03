@echo off

rmdir /S /Q ffi\dist
mkdir ffi\dist

cd ffi\dist

cl /LD ..\test.c /link /out:test.dll