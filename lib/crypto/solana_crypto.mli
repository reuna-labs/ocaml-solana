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

(** Derives a Solana program address. Each seed is at most 32 bytes and there
    may be at most 16. An on-curve digest is rejected. *)
val create_program_address :
  seeds:string list ->
  program:Solana_types.Address.t ->
  (Solana_types.Address.t, string) result

(** Searches bump seeds from 255 down to 0. Callers may provide at most 15
    seeds because the bump occupies the final seed slot. *)
val find_program_address :
  seeds:string list ->
  program:Solana_types.Address.t ->
  ((Solana_types.Address.t * int), string) result
