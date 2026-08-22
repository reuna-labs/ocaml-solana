(** Partially signed and fully signed Solana transactions. *)

type t
type signed

val max_wire_size : int
val create : Message.t -> t
val message : t -> Message.t
val signing_bytes : t -> (string, string) result
val required_signers : t -> Solana_types.Address.t list
val missing_signers : t -> Solana_types.Address.t list

val add_signature :
  signer:Solana_types.Address.t ->
  Solana_types.Signature.t ->
  t ->
  (t, string) result

val sign_with : Solana_crypto.keypair -> t -> (t, string) result
val finalize : t -> (signed, string) result
val encode : signed -> string
val decode : string -> (signed, string) result
val of_signed : signed -> t
val signed_message : signed -> Message.t
val signatures : signed -> Solana_types.Signature.t list
val id : signed -> Solana_types.Signature.t
