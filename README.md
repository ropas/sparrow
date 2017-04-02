# Sparrow

Sparrow is a state-of-the-art static analyzer that aims to verify the absence
of fatal bugs in C source. Sparrow is designed based on the Abstract Interpretation 
framework and the analysis is sound in design. Sparrow adopts a number of well-founded 
static analysis techniques for scalability, precision, and user convenience.
This is an academic version of Sparrow that is different from the [commercial version][fasoo].

## Sparrow Dependencies
To build Sparrow, you need
-   [OCaml][] >= 4.02.3
-   [OPAM][] >= 1.2.2
-   [Batteries][] >= 2.3.1
-   [Cil][] >= 1.7.3
-   [Ocamlgraph][] >= 1.8.7
-   [Apron][] >= 0.9.10
-   [Yojson][] >= 1.2.3
-   [Lymp][] >= 0.1.3
-   [Ppx_compare][] >= 113.33

Optionally, you need the following prerequisites to use machine-learning features
-   [Python][] >= 2.7
-   [Scikit-learn][] >= 0.18

[Ocaml]: http://caml.inria.fr
[OPam]: https://opam.ocaml.org
[Batteries]: http://batteries.forge.ocamlcore.org
[Cil]: https://github.com/cil-project/cil
[Ocamlgraph]: http://ocamlgraph.lri.fr/index.en.html
[Apron]: http://apron.cri.ensmp.fr/library
[Yojson]: http://mjambon.com/yojson.html
[Lymp]: https://github.com/dbousque/lymp
[Ppx_compare]: https://github.com/janestreet/ppx_compare
[Python]: https://www.python.org
[Scikit-learn]: http://scikit-learn.org
[OPAM]: http://opam.ocaml.org
[fasoo]: http://en.fasoo.com/sparrow

## Install Sparrow with OPAM
The easiest way to install Sparrow is to use [OPAM][]. For example:
```sh
opam depext --install sparrow
```

## Install Sparrow from source with OPAM
You can install the prerequisites using [OPAM][]. For example:
```sh
$ opam depext --install batteries cil apron ocamlgraph yojson lymp ppx_compare
```
Once you have installed all the prerequisites, clone the source codes and run the build scripts:
```sh
$ git clone https://github.com/ropas/sparrow.git
$ cd sparrow
$ ./configure
$ make
$ make install
```
Optionally, you need to set up environment variables to use machine-learning features
depending on the installation prefix.
```sh
$ export SPARROW_BIN_PATH= # PREFIX/bin
$ export SPARROW_DATA_PATH= # PREFIX/etc
```
For example, if you install Sparrow using OPAM:
```sh
$ export SPARROW_BIN_PATH=`opam config var sparrow:bin`
$ export SPARROW_DATA_PATH=`opam config var sparrow:etc`
```
## Run the analysis
You can run Sparrow for buffer overflow detection on pre-processed C files. For example:
```sh
$ ./bin/sparrow test.i
# partially flow-sensitive analysis with degree [0-100]
$ ./bin/sparrow -pfs 10 test.i
# selectively unsound analysis with bugfinder level [0-2]
$ ./bin/sparrow -bugfinder 2 test.i
```
