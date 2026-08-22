type decision = Pending | Succeeded | Failed of Yojson.Safe.t | Expired
let rank = function Methods.Processed -> 0 | Methods.Confirmed -> 1 | Methods.Finalized -> 2
let required_rank = function Solana_types.Processed -> 0 | Solana_types.Confirmed -> 1 | Solana_types.Finalized -> 2
let decide ~commitment ~current_block_height ~last_valid_block_height
    (status : Methods.signature_status option) =
  match status with
  | Some { Methods.err = Some error; _ } -> Failed error
  | Some { confirmation_status = Some status; _ } when rank status >= required_rank commitment -> Succeeded
  | _ when Solana_types.U64.compare current_block_height last_valid_block_height > 0 -> Expired
  | _ -> Pending
