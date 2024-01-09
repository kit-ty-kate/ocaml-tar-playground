#!/bin/sh

#ocamlfind ocamlopt -package checkseum.ocaml -package tar.gz -package unix -linkpkg test.ml
ocamlfind ocamlopt -package checkseum.c -package tar.gz -package unix -linkpkg test.ml
