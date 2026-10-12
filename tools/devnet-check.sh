#!/usr/bin/env bash
# Public-testnet evidence for the published signing path: Solana devnet nodes
# judge bytes the released packages produced.
#
#   tools/devnet-check.sh
#
# Installs solana-transaction from the public Reuna opam overlay in a pinned
# container (a consumer's view: nothing from this checkout but
# tools/devnet_sign.ml), signs a 5000-lamport transfer from a fresh key against
# a blockhash fetched moments before, and asks each node in SOLANA_RPCS to
# simulateTransaction it with sigVerify, as signed and with one byte flipped in
# the signature and in the message.
#
# What a pass shows, per node:
#   - the transaction as signed passes signature verification and the
#     blockhash check and stops at the payer lookup: AccountNotFound, because
#     the key is new. So the node parsed the legacy message, took its first
#     account as the fee payer, and the Ed25519 signature over the message
#     bytes verified against that key;
#   - either tampered copy is refused as SignatureFailure, so the check above
#     was a real one and not skipped.
# What it does not show: execution. Moving lamports needs a funded payer; with
# faucet SOL, SOLANA_ENABLE_NETWORK_TESTS=1 dune exec solana-devnet-transfer
# (README, "Devnet smoke") runs the whole submission.
set -euo pipefail
here=$(cd "$(dirname "$0")/.." && pwd)
IMAGE=docker.io/ocaml/opam:debian-12-ocaml-5.5@sha256:36803e379c31545ef00dfb64f8bdf3226b5e8c7b76746b636caedda811c625f3
OVERLAY=${SOLANA_OVERLAY:-https://github.com/reuna-labs/opam-repository.git}
SOLANA_RPCS=${SOLANA_RPCS:-"https://api.devnet.solana.com"}
first=${SOLANA_RPCS%% *}

echo "== the published solana-transaction, signing (overlay $OVERLAY)"
# Build first, fetch the blockhash last: a blockhash lives about a minute.
out=$(docker run --rm -e OVERLAY="$OVERLAY" -e RPC="$first" -v "$here/tools/devnet_sign.ml:/src/devnet_sign.ml:ro" "$IMAGE" sh -ec '
	opam repository add reuna "$OVERLAY" >/dev/null 2>&1
	opam install -y --confirm-level=unsafe-yes solana-transaction >/tmp/install.log 2>&1 || { tail -20 /tmp/install.log >&2; exit 1; }
	opam list --installed --columns=name,version solana-types solana-crypto solana-transaction 2>/dev/null | grep -v "^#" >&2
	mkdir -p /tmp/w && cd /tmp/w && cp /src/devnet_sign.ml .
	printf "(lang dune 3.0)\n" > dune-project
	printf "(executable (name devnet_sign) (libraries solana-types solana-crypto solana-transaction))\n" > dune
	opam exec -- dune build ./devnet_sign.exe >&2
	SOLANA_BLOCKHASH=$(curl -fsS -m 30 -H "Content-Type: application/json" \
		-d "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"getLatestBlockhash\",\"params\":[{\"commitment\":\"confirmed\"}]}" "$RPC" \
		| sed -n "s/.*\"blockhash\":\"\([1-9A-HJ-NP-Za-km-z]*\)\".*/\1/p") ./_build/default/devnet_sign.exe')
printf '%s\n' "$out" | sed 's/^/  /'
wire=$(printf '%s\n' "$out" | awk '$1=="wire" {print $2}')
[ -n "$wire" ] || { echo "FAIL: the signer printed no transaction"; exit 1; }

judge() { # judge <rpc> <hex> <offset to flip or -1>: the simulation's err
	python3 - "$@" <<'PY'
import base64, json, sys, urllib.request
rpc, wire, flip = sys.argv[1], bytearray.fromhex(sys.argv[2]), int(sys.argv[3])
if flip >= 0:
    wire[flip if flip < len(wire) else len(wire) - 1] ^= 1
def call(method, params):
    body = json.dumps({"jsonrpc": "2.0", "id": 1, "method": method, "params": params}).encode()
    req = urllib.request.Request(rpc, body, {"Content-Type": "application/json"})
    reply = json.load(urllib.request.urlopen(req, timeout=30))
    if "error" in reply:
        return "rpc error: " + reply["error"]["message"]
    return reply["result"]
r = call("simulateTransaction", [base64.b64encode(bytes(wire)).decode(),
         {"sigVerify": True, "encoding": "base64", "commitment": "confirmed"}])
print(r if isinstance(r, str) else r["value"]["err"])
PY
}
version() {
	curl -fsS -m 30 -H 'Content-Type: application/json' -d '{"jsonrpc":"2.0","id":1,"method":"getVersion"}' "$1" \
		| python3 -c 'import json,sys; print(json.load(sys.stdin)["result"]["solana-core"])'
}

# Wire layout: compact-u16 signature count (one byte, 0x01), the 64-byte
# signature, then the message.
fail=0
for rpc in $SOLANA_RPCS; do
	echo "== $rpc (solana-core $(version "$rpc"))"
	got=$(judge "$rpc" "$wire" -1)
	case "$got" in
	AccountNotFound) echo "  ok: as signed, the signature verified and the node looked up the payer: $got" ;;
	*) echo "  FAIL: as signed: $got"; fail=1 ;;
	esac
	for at in 10 999999; do
		what=signature; [ "$at" = 10 ] || what=message
		got=$(judge "$rpc" "$wire" "$at")
		case "$got" in
		SignatureFailure) echo "  ok: one bit flipped in the $what is refused: $got" ;;
		*) echo "  FAIL: flipped $what: $got"; fail=1 ;;
		esac
	done
done

[ "$fail" = 0 ] && echo PASS || { echo FAIL; exit 1; }
