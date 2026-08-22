type context = { slot : Solana_types.U64.t; api_version : string option }
type 'a contextual = { context : context; value : 'a }
type version = { solana_core : string; feature_set : Solana_types.U64.t option }
type latest_blockhash = {
  blockhash : Solana_types.Hash.t;
  last_valid_block_height : Solana_types.U64.t;
}

type confirmation_status = Processed | Confirmed | Finalized
type signature_status = {
  slot : Solana_types.U64.t;
  confirmations : int option;
  err : Yojson.Safe.t option;
  confirmation_status : confirmation_status option;
}

type simulation = {
  err : Yojson.Safe.t option;
  logs : string list option;
  units_consumed : Solana_types.U64.t option;
}

val get_genesis_hash : unit -> Solana_types.Hash.t Method.t
val get_version : unit -> version Method.t
val get_latest_blockhash : ?commitment:Solana_types.commitment -> unit -> latest_blockhash contextual Method.t
val get_block_height : ?commitment:Solana_types.commitment -> unit -> Solana_types.U64.t Method.t
val get_balance : ?commitment:Solana_types.commitment -> Solana_types.Address.t -> Solana_types.U64.t contextual Method.t
val get_fee_for_message : ?commitment:Solana_types.commitment -> Solana_transaction.Message.t -> Solana_types.U64.t option contextual Method.t
val simulate_transaction : ?commitment:Solana_types.commitment -> ?sig_verify:bool -> string -> simulation contextual Method.t
val send_transaction : ?preflight_commitment:Solana_types.commitment -> ?skip_preflight:bool -> Solana_transaction.Transaction.signed -> Solana_types.Signature.t Method.t
val get_signature_statuses : ?search_transaction_history:bool -> Solana_types.Signature.t list -> signature_status option list contextual Method.t
val get_transaction : ?commitment:Solana_types.commitment -> Solana_types.Signature.t -> Yojson.Safe.t option Method.t
