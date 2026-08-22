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

let nth accounts index =
  match List.nth_opt accounts index with
  | Some account -> Ok account
  | None -> Error "intent: compiled account index is out of range"

let rec resolve_instruction accounts instruction =
  match nth accounts instruction.Message.program_id_index with
  | Error _ as error -> error
  | Ok program ->
    let rec resolve acc = function
      | [] -> Ok (List.rev acc)
      | index :: rest ->
        (match nth accounts index with
        | Error _ as error -> error
        | Ok account -> resolve (account :: acc) rest)
    in
    (match resolve [] instruction.account_indexes with
    | Error _ as error -> error
    | Ok resolved_accounts ->
      if Solana_types.Address.equal program Programs.system_program then
        (match Programs.decode_transfer instruction.data, resolved_accounts with
        | Some lamports, [ source; destination ] ->
          Ok (Sol_transfer { source; destination; lamports })
        | _ -> opaque program resolved_accounts instruction.data)
      else if Solana_types.Address.equal program Programs.compute_budget_program then
        (match Programs.decode_compute_unit_limit instruction.data with
        | Some units -> Ok (Set_compute_unit_limit units)
        | None ->
          (match Programs.decode_compute_unit_price instruction.data with
          | Some price -> Ok (Set_compute_unit_price price)
          | None -> opaque program resolved_accounts instruction.data))
      else opaque program resolved_accounts instruction.data)

and opaque program accounts data =
  let digest = Digestif.SHA256.digest_string data |> Digestif.SHA256.to_hex in
  Ok (Opaque { program; accounts; data_sha256 = digest })

let derive ?(address_tables = []) message =
  match Message.resolve_accounts ~address_tables message with
  | Error _ as error -> error
  | Ok [] -> Error "intent: message has no payer"
  | Ok (payer :: _ as accounts) ->
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | instruction :: rest ->
        (match resolve_instruction accounts instruction with
        | Error _ as error -> error
        | Ok instruction -> loop (instruction :: acc) rest)
    in
    (match loop [] (Message.instructions message) with
    | Error _ as error -> error
    | Ok instructions ->
      Ok
        { payer;
          recent_blockhash = Message.recent_blockhash message;
          required_signers = Message.required_signers message;
          instructions })

let validate_safe_sol_transfer intent =
  let transfers =
    List.fold_left
      (fun count -> function Sol_transfer _ -> count + 1 | _ -> count)
      0 intent.instructions
  in
  if transfers <> 1 then Error "intent policy: expected exactly one SOL transfer"
  else if
    List.exists (function Opaque _ -> true | _ -> false) intent.instructions
  then Error "intent policy: opaque instruction rejected"
  else Ok ()
