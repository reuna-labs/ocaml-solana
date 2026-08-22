(** Validated Solana wire primitives. Binary values use raw OCaml strings. *)

module U64 : sig
  type t

  val zero : t
  val of_int : int -> (t, string) result
  val of_z : Z.t -> (t, string) result
  val of_string : string -> (t, string) result
  val to_z : t -> Z.t
  val to_string : t -> string
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

module Address : sig
  type t

  val of_bytes : string -> (t, string) result
  val to_bytes : t -> string
  val of_base58 : string -> (t, string) result
  val to_base58 : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Hash : sig
  type t

  val of_bytes : string -> (t, string) result
  val to_bytes : t -> string
  val of_base58 : string -> (t, string) result
  val to_base58 : t -> string
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Signature : sig
  type t

  val of_bytes : string -> (t, string) result
  val to_bytes : t -> string
  val of_base58 : string -> (t, string) result
  val to_base58 : t -> string
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

type commitment = Processed | Confirmed | Finalized

val commitment_to_string : commitment -> string
val commitment_of_string : string -> (commitment, string) result

module Network : sig
  type t = { name : string; genesis_hash : Hash.t }

  val make : name:string -> genesis_hash:Hash.t -> t
  val devnet : t
  val testnet : t
  val mainnet_beta : t
end

type account_meta = { address : Address.t; signer : bool; writable : bool }

val account_meta : ?signer:bool -> ?writable:bool -> Address.t -> account_meta

type instruction = {
  program : Address.t;
  accounts : account_meta list;
  data : string;
}

val instruction : program:Address.t -> accounts:account_meta list -> data:string -> instruction
