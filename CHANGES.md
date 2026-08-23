# Changes

## 0.1.0~alpha1

- Add validated addresses, hashes, signatures, unsigned 64-bit values, and
  current network identities.
- Add Ed25519 seed keypairs, external signature verification, legacy and v0
  message compilation, address lookup tables, and strict shortvec codecs.
- Add System transfer and Compute Budget instructions with compiled-byte intent
  review.
- Add Solana PDA and associated-token-account derivation, classic Token and
  Token-2022 `TransferChecked`, strict token intent policy, and independently
  regenerable Kit 8.0.0 / Agave 4.2.1 conformance fixtures.
- Add typed transport-independent JSON-RPC, Unix Cohttp, confirmation expiry,
  an opt-in Devnet transfer smoke, golden fixtures, and Mirage-safe link checks.
- Add a deterministic stale-blockhash refresh/re-sign and confirmation-deadline
  state machine, and use it in the Devnet submission flow.
- Add signature/account WebSocket subscriptions with Unix reconnect, bounded
  streaming Cohttp responses, and a native MirageOS ALPN HTTP/TLS adapter.
