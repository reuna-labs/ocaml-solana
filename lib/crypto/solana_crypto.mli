(** Ed25519 signing used by Solana transactions. *)

type keypair

val keypair_of_seed : string -> (keypair, string) result
(** Accepts exactly the 32-byte RFC 8032 seed. *)

val seed : keypair -> string
val address : keypair -> Solana_types.Address.t
val sign : keypair -> string -> Solana_types.Signature.t

val verify :
  address:Solana_types.Address.t ->
  signature:Solana_types.Signature.t ->
  string ->
  bool
