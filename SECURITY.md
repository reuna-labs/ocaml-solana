# Security

This repository is unaudited alpha software. Do not use it to control assets of
value. Report vulnerabilities privately to security@reuna.io rather than
opening a public issue.

The seed helper accepts the raw 32-byte RFC 8032 seed. OCaml strings and heap
objects are not reliably zeroized, so long-lived or high-value keys should use
an external signer through `Transaction.signing_bytes` and
`Transaction.add_signature`.

Treat RPC responses and address-table contents as untrusted. Pin the expected
genesis hash, resolve version-0 tables from an authenticated source, inspect the
derived intent, simulate, and enforce an application-specific policy before
signing. The built-in safe-transfer policy accepts one SOL transfer plus only
recognized Compute Budget instructions.
