# ocaml-solana

Pure OCaml Solana transaction construction, Ed25519 signing and typed JSON-RPC
for signer-oriented clients. The first alpha supports legacy and v0 messages,
System Program SOL transfers, checked SPL Token transfers, associated token
accounts, compute-budget instructions and a Unix HTTP client. The offline
packages are suitable for MirageOS consumers; HTTP/TLS also has a native
MirageOS adapter.

> **Security:** this code is new, unaudited alpha software. Do not use it to
> control assets of value.

## Install

```sh
opam repository add reuna https://github.com/reuna-labs/opam-repository.git
opam update
opam install solana-rpc-unix.0.1.0~alpha1
```

This installs the hosted client and pure transaction packages from the public
`v0.1.0-alpha1` release. No vendored-codec or development pin is required for
an installed consumer.

## Compatibility target

The checked-in wire fixtures are pinned to Agave `v4.2.1`, including
`solana-message` `4.2.3`, `solana-transaction` `4.1.4`,
`solana-short-vec` `3.2.2`, `solana-system-interface` `3.2.0`, and
`@solana/kit` `v8.0.0`. Transaction v1 remains pending activation and is
rejected as unsupported.

## Build from a checkout

Pin the credential-free vendored codec snapshot, install dependencies, and run:

```sh
opam pin add -yn web3-codec-basen ./vendor/web3-codec
opam pin add -yn web3-codec-base58 ./vendor/web3-codec
opam pin add -yn web3-codec-borsh ./vendor/web3-codec
opam install -y --deps-only --with-test .
opam exec -- dune build @all @runtest
```

The snapshot pins codec commit `13be69c3071a1e66996cc7c64e52366b9bf4ce0e`.

The normal tests do not access a network. Set `SOLANA_ENABLE_NETWORK_TESTS=1`
only when deliberately running the Devnet smoke executable.

## First vertical slice

The implemented launch slice is intentionally small and signer-oriented:

- canonical legacy and version-0 messages, including caller-resolved address
  lookup tables;
- strict shortvec decoding and explicit rejection of unknown versions;
- System Program SOL transfer and Compute Budget limit/price instructions;
- canonical PDA/ATA derivation, idempotent ATA creation, and checked transfers
  for the classic Token and Token-2022 program IDs;
- 32-byte-seed Ed25519 signing, plus verified external-signature attachment;
- reviewable intents derived from compiled message bytes;
- typed JSON-RPC methods, bounded streaming HTTP responses, Unix Cohttp/Lwt,
  and MirageOS ALPN HTTP/TLS adapters;
- deterministic simulation/submission/confirmation state with stale-blockhash
  refresh, re-signing, polling deadlines, and block-height expiry;
- signature and account WebSocket subscriptions on Unix, with automatic
  reconnect and fresh server-side subscription identifiers.

Token-2022 extension-aware account decoding, durable nonces, WebSocket support
inside MirageOS, and automatic lookup-table discovery are deliberately deferred.
The safe token-transfer policy therefore rejects Token-2022 by default; opt in
only after authenticating the mint account and reviewing its extensions.

### External signer flow

Construct a `Transaction.t`, obtain `Transaction.signing_bytes`, and give those
exact bytes to the external signer. Attach the returned signature with
`Transaction.add_signature`; it verifies Ed25519 against the required signer
before accepting it. `Transaction.finalize` refuses missing signatures and
enforces Solana's 1,232-byte legacy/v0 wire limit.

### Devnet smoke

The installed `solana-devnet-transfer` executable is inert unless explicitly
enabled. It verifies the RPC genesis hash, derives and checks the intent, signs
locally, simulates, sends, and waits for confirmed commitment. If simulation,
submission, or confirmation proves the blockhash stale, it fetches a fresh
blockhash and rebuilds, reviews, and re-signs the transaction; attempts and
confirmation polls are bounded.

```sh
SOLANA_ENABLE_NETWORK_TESTS=1 \
SOLANA_SEED_HEX=<64-hex-character-devnet-seed> \
SOLANA_DESTINATION=<base58-address> \
dune exec solana-devnet-transfer
```

To exercise the token path, `SOLANA_DESTINATION` is the recipient owner rather
than a token account. Set `SOLANA_TOKEN_MINT`, `SOLANA_TOKEN_AMOUNT` in base
units, and `SOLANA_TOKEN_DECIMALS`; the smoke derives both ATAs, creates the
recipient ATA idempotently, uses `TransferChecked`, reviews the compiled intent,
then simulates before submission. `SOLANA_TOKEN_PROGRAM=token-2022` additionally
requires `SOLANA_ALLOW_TOKEN_2022=1` after mint-extension review.

Devnet can be reset. Override `SOLANA_EXPECTED_GENESIS_HASH` when a deliberate
reset has been independently verified. Never use a production seed here.

## Package boundaries

`solana-types`, `solana-crypto`, and `solana-transaction` contain no Unix,
clock, environment, RNG, HTTP, or Lwt dependency. `solana-rpc` is
transport-independent and owns the deterministic submission and subscription
state. Unix dependencies exist only in `solana-rpc-unix`; the reusable Cohttp
streaming adapter is in `solana-rpc-cohttp`, and `solana-rpc-mirage` targets the
MirageOS ALPN client. The `mirage-smoke` executables link the same submission
workflow and the Mirage adapter without the Unix RPC package.

The three lean codec packages are vendored at their pinned commit so clean CI
does not need cross-repository credentials. Their canonical development source
remains the private `reuna-labs/ocaml-web3-codec` repository.
