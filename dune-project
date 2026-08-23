(lang dune 3.6)

(name solana)
(version 0.1.0~alpha1)
(generate_opam_files true)
(implicit_transitive_deps false)
(source (github reuna-labs/ocaml-solana))
(license ISC)
(authors "Gil Bahat <bahat.gil@gmail.com>")
(maintainers "Gil Bahat <bahat.gil@gmail.com>")

(package
 (name solana-types)
 (synopsis "Validated primitive types for Solana")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.6))
  (zarith (>= 1.12))
  (web3-codec-base58 (>= 0.1.0~alpha1))))

(package
 (name solana-crypto)
 (synopsis "Ed25519 signing backend for Solana")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.6))
  (solana-types (= :version))
  (digestif (>= 1.2.0))
  mirage-crypto-ec))

(package
 (name solana-transaction)
 (synopsis "Canonical legacy and v0 Solana transactions")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.6))
  (solana-types (= :version))
  (solana-crypto (= :version))
  (web3-codec-borsh (>= 0.1.0~alpha1))
  (digestif (>= 1.2.0))
  (zarith (>= 1.12))))

(package
 (name solana-rpc)
 (synopsis "Typed transport-independent Solana JSON-RPC client")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.6))
  (solana-types (= :version))
  (solana-transaction (= :version))
  (yojson (>= 2.0.0))
  (base64 (>= 3.5.0))
  (zarith (>= 1.12))))

(package
 (name solana-rpc-cohttp)
 (synopsis "Cohttp/Lwt transport shared by Solana RPC adapters")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.6))
  (solana-rpc (= :version))
  (lwt (>= 5.6.0))
  (uri (>= 4.2.0))
  (http (>= 6.1.0))
  (cohttp-lwt (>= 6.1.0))
  (alcotest :with-test)))

(package
 (name solana-rpc-unix)
 (synopsis "Unix HTTP transport for Solana JSON-RPC")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.6))
  (solana-types (= :version))
  (solana-crypto (= :version))
  (solana-transaction (= :version))
  (solana-rpc (= :version))
  (solana-rpc-cohttp (= :version))
  (lwt (>= 5.6.0))
  (uri (>= 4.2.0))
  (http (>= 6.1.0))
  (cohttp (>= 6.1.0))
  (cohttp-lwt (>= 6.1.0))
  (cohttp-lwt-unix (>= 6.1.0))
  (base64 (>= 3.3.0))
  (conduit (>= 8.0.0))
  conduit-lwt
  conduit-lwt-unix
  (mirage-crypto-rng (>= 1.0.0))
  (websocket (>= 2.17))))

(package
 (name solana-rpc-mirage)
 (synopsis "MirageOS HTTP/TLS transport for Solana JSON-RPC")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.6))
  (solana-rpc (= :version))
  (lwt (>= 5.6.0))
  http-mirage-client
  h1
  h2
  fmt
  mimic))

(package
 (name solana)
 (synopsis "Pure OCaml SDK for Solana clients and signers")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.6))
  (solana-types (= :version))
  (solana-crypto (= :version))
  (solana-transaction (= :version))
  (solana-rpc (= :version))
  (alcotest :with-test)
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test)))
