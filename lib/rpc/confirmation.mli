type decision = Pending | Succeeded | Failed of Yojson.Safe.t | Expired
val rank : Methods.confirmation_status -> int
val required_rank : Solana_types.commitment -> int
val decide : commitment:Solana_types.commitment -> current_block_height:Solana_types.U64.t ->
  last_valid_block_height:Solana_types.U64.t -> Methods.signature_status option -> decision
