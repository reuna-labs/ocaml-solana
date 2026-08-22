(** Canonical Solana legacy and version-0 messages. *)

type header = {
  num_required_signatures : int;
  num_readonly_signed_accounts : int;
  num_readonly_unsigned_accounts : int;
}

type compiled_instruction = {
  program_id_index : int;
  account_indexes : int list;
  data : string;
}

type address_table_lookup = {
  table : Solana_types.Address.t;
  writable_indexes : int list;
  readonly_indexes : int list;
}

type address_table = {
  key : Solana_types.Address.t;
  addresses : Solana_types.Address.t array;
}

type legacy = {
  header : header;
  static_accounts : Solana_types.Address.t list;
  recent_blockhash : Solana_types.Hash.t;
  instructions : compiled_instruction list;
}

type v0 = {
  header : header;
  static_accounts : Solana_types.Address.t list;
  recent_blockhash : Solana_types.Hash.t;
  instructions : compiled_instruction list;
  address_table_lookups : address_table_lookup list;
}

type t = Legacy of legacy | V0 of v0

val compile_legacy :
  payer:Solana_types.Address.t ->
  recent_blockhash:Solana_types.Hash.t ->
  Solana_types.instruction list ->
  (t, string) result

val compile_v0 :
  payer:Solana_types.Address.t ->
  recent_blockhash:Solana_types.Hash.t ->
  address_tables:address_table list ->
  Solana_types.instruction list ->
  (t, string) result

val encode : t -> (string, string) result
val decode : string -> (t, string) result
val required_signers : t -> Solana_types.Address.t list

(** Returns the full account-key space in instruction-index order. Version-0
    messages require the caller to provide the referenced address tables. *)
val resolve_accounts :
  address_tables:address_table list -> t -> (Solana_types.Address.t list, string) result

val header : t -> header
val static_accounts : t -> Solana_types.Address.t list
val recent_blockhash : t -> Solana_types.Hash.t
val instructions : t -> compiled_instruction list
