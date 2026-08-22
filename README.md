# ocaml-solana

Pure OCaml Solana transaction construction, Ed25519 signing and typed JSON-RPC
for signer-oriented clients. The first alpha supports legacy and v0 messages,
System Program SOL transfers, compute-budget instructions and a Unix HTTP
client. The offline packages are suitable for MirageOS consumers; a Mirage
network adapter is a later milestone.

> **Security:** this code is new, unaudited alpha software. Do not use it to
> control assets of value.

## Compatibility target

The checked-in wire fixtures are pinned to Agave `v4.2.1`, including
`solana-message` `4.2.3`, `solana-transaction` `4.1.4`,
`solana-short-vec` `3.2.2`, `solana-system-interface` `3.2.0`, and
`@solana/kit` `v8.0.0`. Transaction v1 remains pending activation and is
rejected as unsupported.

## Build

Pin the lean codec packages from `../ocaml-web3-codec`, then install the local
packages and run:

```sh
dune runtest
```

CI pins codec commit `13be69c3071a1e66996cc7c64e52366b9bf4ce0e`.

The normal tests do not access a network. Set `SOLANA_ENABLE_NETWORK_TESTS=1`
only when deliberately running the Devnet smoke executable.

## First vertical slice

The implemented launch slice is intentionally small and signer-oriented:

- canonical legacy and version-0 messages, including caller-resolved address
  lookup tables;
- strict shortvec decoding and explicit rejection of unknown versions;
- System Program SOL transfer and Compute Budget limit/price instructions;
- 32-byte-seed Ed25519 signing, plus verified external-signature attachment;
- reviewable intents derived from compiled message bytes;
- typed HTTP JSON-RPC methods and a Unix Cohttp/Lwt adapter;
- simulation, submission, commitment polling, and block-height expiry.

SPL Token, associated token accounts, durable nonces, WebSocket subscriptions,
and automatic lookup-table discovery are deliberately deferred.

### External signer flow

Construct a `Transaction.t`, obtain `Transaction.signing_bytes`, and give those
exact bytes to the external signer. Attach the returned signature with
`Transaction.add_signature`; it verifies Ed25519 against the required signer
before accepting it. `Transaction.finalize` refuses missing signatures and
enforces Solana's 1,232-byte legacy/v0 wire limit.

### Devnet smoke

The installed `solana-devnet-transfer` executable is inert unless explicitly
enabled. It verifies the RPC genesis hash before requesting a blockhash, derives
and checks the intent, signs locally, simulates, sends, and waits for confirmed
commitment or expiry.

```sh
SOLANA_ENABLE_NETWORK_TESTS=1 \
SOLANA_SEED_HEX=<64-hex-character-devnet-seed> \
SOLANA_DESTINATION=<base58-address> \
dune exec solana-devnet-transfer
```

Devnet can be reset. Override `SOLANA_EXPECTED_GENESIS_HASH` when a deliberate
reset has been independently verified. Never use a production seed here.

## Package boundaries

`solana-types`, `solana-crypto`, and `solana-transaction` contain no Unix,
clock, environment, RNG, HTTP, or Lwt dependency. `solana-rpc` is
transport-independent. Unix dependencies exist only in `solana-rpc-cohttp` and
`solana-rpc-unix`. The `mirage-smoke` link check guards that boundary.

CI needs a repository read token named `REUNA_REPO_READ_TOKEN` because the
three lean codec packages currently live in the private
`reuna-labs/ocaml-web3-codec` repository.
