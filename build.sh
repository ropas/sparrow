#!/bin/sh
set -e
NCPU="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)"
OCAML_VERSION="4.04.0"
SPARROW_OPAM_SWITCH=sparrow-"$OCAML_VERSION"
opam init --compiler=$OCAML_VERSION -j $NCPU --no-setup --yes
opam switch set -j $NCPU $SPARROW_OPAM_SWITCH --alias-of $OCAML_VERSION
eval $(SHELL=bash opam config env --switch=$SPARROW_OPAM_SWITCH)
opam install depext
opam pin add sparrow . -n --yes
opam depext apron
opam install -j $NCPU sparrow --yes --deps-only
opam pin remove sparrow
./configure --enable-tests
make
