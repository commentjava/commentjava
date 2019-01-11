#!/usr/bin/env bash

if [ $# -lt 1 ]; then
        echo "Usage: $0 <ocaml_version>"
        echo ""
        echo "The versions must be found on https://hub.docker.com/r/ocaml/opam/"
        echo "For Ocaml 4.02.3:"
        echo "$0 4.02.3"
        exit 2;
fi
TAG="ocaml/menhir:$1"
docker build --build-arg OCAML_VERSION=$1 --tag $TAG .
docker run --rm -v "$PWD":/home/opam/j $TAG bash -c "cd j && make"

