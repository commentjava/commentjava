The minijavac compiler
======================

[![pipeline status](https://gitlab.com/commentjava/commentjava/badges/master/pipeline.svg)](https://gitlab.com/commentjava/commentjava/commits/master)

A compilation project for Third year students of Telecom Bretagne.

> 'ocamlbuild Main.byte' (or native) to build the compiler. The main file
> is Main/Main.ml, it should not be modified. It opens the given file,
> creates a lexing buffer, initializes the location and call the compile
> function of the module Main/compile.ml. It is this function that you
> should modify to call your parser.
> 
> 'ocamlbuild Main.byte -- <filename>' (or native) to build and then execute
> the compiler on the file given. By default, the program searches for
> file with the extension .java and append it to the given filename if
> it does not end with it.
> 
> If you want to reuse an existing ocaml library. Start by installing it
> with opam. For example, to use colored terminal output you
> use 'opam install ANSITerminal'.
> Then you must inform ocamlbuild to use the ocamlfind tool :
> 'ocamlbuild -use-ocamlfind Main.byte -- tests/UnFichierDeTest.java'
> et vous devez ajouter au fichier `_tags` la bibliothèque en question par exemple :
> true: package(ANSITerminal)

## Quickstart

Building is done using the given Makfile:

Create the main binary:
```bash
make

# print the Ast tree:
./Main.native -v <java file>
```

Run the lovely hand crafted suite:
```bash
make test-all
```

### More commands

Run the provided JDK tests:
```bash
make test-jdk
```

List existing tests receipes:
```bash
make test-list
```

Create the `_build/errors.txt` file which list the possible error states:
```bash
make list-errors
```

See Makefile for every make receipes.

## Test suite

In order to verify the validity of the parsing process, we have design a simple
test suite that helps us track down issues and regressions introduced down the
line. These tests can be launched with the `make test-all` command.

The `Test/` folder contains multiple subfolders dedicated to each test suite:
- `block_statements_files`: Files to test the block language
- `classes_files`: Files to test the classes language
- `expression_files`: Files to test the expressions parsing
- `must_fail_files`: Contains files that **must not** compile

Tests can be arbitrarly disabled by appending `.disabled` to the file name. In
which case the test suite will show them as `skipped`.

For every commit, in order to quickly catch regressions, the test suite is
automatically runned by the Gitlab Continuous Integration suite (CI). The
builds can be found at the following address:
https://gitlab.com/commentjava/commentjava/pipelines

We noticed that some methods and syntaxes were not always working on every
Ocaml versions(e.g. commit 9d8ad5560641329ffb42583166cc2f38ab40d96e). We
created a script based on Docker to test multiple versions of Ocaml without
installing them. See script `docker_test.sh` for more info.

Quick bash script to spot failures locations in tests:
```bash
make test-jdk 2>&1 |\
ag "\Test.*error" |\
awk '{print $2 " " $4}' |\
while read x y read; do;
   echo "> $x Line $y:" ;
   awk "{ if(NR==$y) print \$0 }" $(echo $x | tr -d '"');
   echo "";
done;
```

## Java Specification Language compliance and divergence

TODO:
- Annotation of annotations
- Limits on types
- Limits on cast expressions

## Miscellaneous notes

Some jdk tests fail intentionnaly, thoses tests don't follow the JSL for Java
SE 6. We specifically decided to follow the specification instead of doing 100%
parsing success rate on the provided JDK source code.

Purposefully failing test files:
- Invalid `for` constructor:
  - Test/jdk/src/java/text/MergeCollation.java L107
- Invalid variable name `enum`:
  - Test/jdk/src/com/sun/corba/se/internal/Activation/RepositoryImpl.java L111
  - Test/jdk/src/com/sun/corba/se/internal/core/ServiceContextRegistry.java L64
  - Test/jdk/src/com/sun/security/auth/PolicyFile.java L472
  - Test/jdk/src/com/sun/security/auth/PolicyParser.java 247
  - Test/jdk/src/java/awt/datatransfer/MimeTypeParameterList.java L45
  - Test/jdk/src/java/awt/geom/Area.java L193
  - Test/jdk/src/java/awt/image/MemoryImageSource.java L332
  - Test/jdk/src/java/security/PermissionCollection.java L161
  - Test/jdk/src/java/security/Permissions.java L507
  - Test/jdk/src/java/security/Security.java L510
  - Test/jdk/src/java/text/RuleBasedBreakIterator.java L1375
  - Test/jdk/src/java/util/jar/JarFile.java L209
