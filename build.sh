#!/bin/sh
set -e
version="4.04.0"
opam init --compiler=$version --no-setup --yes
opam switch $version
eval $(SHELL=bash opam config env)
opam pin add sparrow . -n --yes
opam depext apron
opam install sparrow --yes --deps-only
opam pin remove sparrow
./configure
make
