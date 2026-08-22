# Vendored lean codec snapshot

This directory contains only `web3-codec-basen`, `web3-codec-base58`, and
`web3-codec-borsh` from private repository `reuna-labs/ocaml-web3-codec`, commit
`13be69c3071a1e66996cc7c64e52366b9bf4ce0e` (ISC).

It makes clean CI and source builds independent of cross-repository GitHub
credentials. The canonical development source remains `ocaml-web3-codec`; when
updating, copy the three package directories and generated opam files together,
then rerun both repositories' package tests.
