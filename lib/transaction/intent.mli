(** Human-reviewable meaning derived from canonical compiled bytes. *)

type token_transfer = {
  token_program : Programs.token_program;
  source : Solana_types.Address.t;
  mint : Solana_types.Address.t;
  destination : Solana_types.Address.t;
  authority : Solana_types.Address.t;
  amount : Solana_types.U64.t;
  decimals : int;
}

type associated_token_account_creation = {
  token_program : Programs.token_program;
  payer : Solana_types.Address.t;
  associated : Solana_types.Address.t;
  owner : Solana_types.Address.t;
  mint : Solana_types.Address.t;
  idempotent : bool;
}

type instruction =
  | Sol_transfer of {
      source : Solana_types.Address.t;
      destination : Solana_types.Address.t;
      lamports : Solana_types.U64.t;
    }
  | Set_compute_unit_limit of int
  | Set_compute_unit_price of Solana_types.U64.t
  | Token_transfer_checked of token_transfer
  | Create_associated_token_account of associated_token_account_creation
  | Opaque of {
      program : Solana_types.Address.t;
      accounts : Solana_types.Address.t list;
      data_sha256 : string;
    }

type t = {
  payer : Solana_types.Address.t;
  recent_blockhash : Solana_types.Hash.t;
  required_signers : Solana_types.Address.t list;
  instructions : instruction list;
}

val derive :
  ?address_tables:Message.address_table list -> Message.t -> (t, string) result

(** Requires exactly one System Program SOL transfer. The only other accepted
    instructions are Compute Budget limit/price setters. *)
val validate_safe_sol_transfer : t -> (unit, string) result

(** Requires exactly one checked SPL Token transfer. Other accepted
    instructions are Compute Budget setters and at most one idempotent ATA
    creation for that transfer's destination. Token-2022 is rejected by
    default because mint extensions can change transfer semantics; enable it
    only after separately authenticating and reviewing the mint state. *)
val validate_safe_token_transfer :
  ?allow_token_2022:bool -> t -> (unit, string) result
