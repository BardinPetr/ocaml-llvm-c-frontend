#!/bin/bash

rm demo demo.o demo.s
# clang -S -emit-llvm q.c
llc-17 demo.ll
clang-17 -o demo demo.s
./demo
