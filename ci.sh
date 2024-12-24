#!/bin/bash

cd /root/app
eval $(opam env)
opam install . --deps-only --with-test
opam exec -- dune build
opam exec -- dune runtest
