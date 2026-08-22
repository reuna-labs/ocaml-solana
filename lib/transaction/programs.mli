val system_program : Solana_types.Address.t
val compute_budget_program : Solana_types.Address.t

val transfer :
  from:Solana_types.Address.t ->
  to_:Solana_types.Address.t ->
  lamports:Solana_types.U64.t ->
  Solana_types.instruction

val set_compute_unit_limit : int -> (Solana_types.instruction, string) result
val set_compute_unit_price : Solana_types.U64.t -> Solana_types.instruction

val decode_transfer : string -> Solana_types.U64.t option
val decode_compute_unit_limit : string -> int option
val decode_compute_unit_price : string -> Solana_types.U64.t option
