# ocaml-solana

Pure OCaml Solana transaction and JSON-RPC libraries for Unix and future
MirageOS/Solo5 consumers. Keep signed-data libraries deterministic and free of
Unix, environment, clock, RNG and transport dependencies. Network tests must
remain opt-in; ordinary `dune runtest` is hermetic.
