(** Human-reviewable meaning derived from canonical compiled bytes. *)

type instruction =
  | Sol_transfer of {
      source : Solana_types.Address.t;
      destination : Solana_types.Address.t;
      lamports : Solana_types.U64.t;
    }
  | Set_compute_unit_limit of int
  | Set_compute_unit_price of Solana_types.U64.t
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
