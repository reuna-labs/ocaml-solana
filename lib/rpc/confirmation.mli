type decision = Pending | Succeeded | Failed of Yojson.Safe.t | Expired
val decide : commitment:Solana_types.commitment -> current_block_height:Solana_types.U64.t ->
  last_valid_block_height:Solana_types.U64.t -> Methods.signature_status option -> decision
