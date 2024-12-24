#!/bin/bash

rm q q.s
clang -S -emit-llvm q.c
llc q.ll
clang -o q q.s
./q
