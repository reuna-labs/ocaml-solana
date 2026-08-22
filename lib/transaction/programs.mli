val system_program : Solana_types.Address.t
val compute_budget_program : Solana_types.Address.t
val associated_token_program : Solana_types.Address.t

type token_program = Token | Token_2022

val token_program_address : token_program -> Solana_types.Address.t

val transfer :
  from:Solana_types.Address.t ->
  to_:Solana_types.Address.t ->
  lamports:Solana_types.U64.t ->
  Solana_types.instruction

val set_compute_unit_limit : int -> (Solana_types.instruction, string) result
val set_compute_unit_price : Solana_types.U64.t -> Solana_types.instruction

val associated_token_address :
  owner:Solana_types.Address.t ->
  mint:Solana_types.Address.t ->
  token_program:token_program ->
  ((Solana_types.Address.t * int), string) result

val create_associated_token_account_idempotent :
  payer:Solana_types.Address.t ->
  owner:Solana_types.Address.t ->
  mint:Solana_types.Address.t ->
  token_program:token_program ->
  (Solana_types.instruction, string) result

val transfer_checked :
  source:Solana_types.Address.t ->
  mint:Solana_types.Address.t ->
  destination:Solana_types.Address.t ->
  authority:Solana_types.Address.t ->
  amount:Solana_types.U64.t ->
  decimals:int ->
  token_program:token_program ->
  (Solana_types.instruction, string) result

val decode_transfer : string -> Solana_types.U64.t option
val decode_compute_unit_limit : string -> int option
val decode_compute_unit_price : string -> Solana_types.U64.t option
val decode_transfer_checked : string -> (Solana_types.U64.t * int) option
val is_create_associated_token_account_idempotent : string -> bool
