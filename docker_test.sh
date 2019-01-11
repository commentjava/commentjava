#!/usr/bin/env bash

if [ $# -lt 1 ]; then
        echo "OVERVIEW: Test the project with different versions of ocaml"
        echo ""
        echo "USAGE: $0 <ocaml_version> [command]"
        echo ""
        echo "The ocaml_versions must be found on https://hub.docker.com/r/ocaml/opam/"
        echo "Examples:"
        echo "Use Ocaml 4.02.3:"
        echo "$0 4.02.3"
        echo ""
        echo "Change the test case:"
        echo "$0 4.02.3 test-ClassesTest.native"
        exit 2;
fi
TAG="ocaml/menhir:$1"
docker build --build-arg OCAML_VERSION=$1 --tag $TAG .
docker run --rm -v "$PWD":/home/opam/j $TAG bash -c "cd j && ${2-make test-all}"

