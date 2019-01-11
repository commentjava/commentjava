ARG OCAML_VERSION
FROM ocaml/opam:debian-stable_ocaml-$OCAML_VERSION

RUN opam update
RUN opam install -q -y menhir
