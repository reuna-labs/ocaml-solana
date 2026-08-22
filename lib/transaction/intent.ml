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

let nth accounts index =
  match List.nth_opt accounts index with
  | Some account -> Ok account
  | None -> Error "intent: compiled account index is out of range"

let token_program_of_address address =
  if Solana_types.Address.equal address (Programs.token_program_address Programs.Token) then
    Some Programs.Token
  else if
    Solana_types.Address.equal address (Programs.token_program_address Programs.Token_2022)
  then Some Programs.Token_2022
  else None

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
      else
        (match token_program_of_address program with
        | Some token_program ->
          (match Programs.decode_transfer_checked instruction.data, resolved_accounts with
          | Some (amount, decimals), [ source; mint; destination; authority ] ->
            Ok
              (Token_transfer_checked
                 { token_program; source; mint; destination; authority; amount; decimals })
          | _ -> opaque program resolved_accounts instruction.data)
        | None when Solana_types.Address.equal program Programs.associated_token_program ->
          (match resolved_accounts with
          | [ payer; associated; owner; mint; system_program; token_program_address ]
            when Programs.is_create_associated_token_account_idempotent instruction.data
                 && Solana_types.Address.equal system_program Programs.system_program ->
            (match token_program_of_address token_program_address with
            | None -> opaque program resolved_accounts instruction.data
            | Some token_program ->
              (match Programs.associated_token_address ~owner ~mint ~token_program with
              | Ok (expected, _) when Solana_types.Address.equal expected associated ->
                Ok
                  (Create_associated_token_account
                     { token_program;
                       payer;
                       associated;
                       owner;
                       mint;
                       idempotent = true })
              | _ -> opaque program resolved_accounts instruction.data))
          | _ -> opaque program resolved_accounts instruction.data)
        | None -> opaque program resolved_accounts instruction.data))

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
  else
    let allowed = function
      | Sol_transfer _ | Set_compute_unit_limit _ | Set_compute_unit_price _ -> true
      | Token_transfer_checked _ | Create_associated_token_account _ | Opaque _ -> false
    in
    if List.for_all allowed intent.instructions then Ok ()
    else Error "intent policy: non-SOL-transfer instruction rejected"

let validate_safe_token_transfer ?(allow_token_2022 = false) intent =
  let transfers =
    List.filter_map
      (function Token_transfer_checked transfer -> Some transfer | _ -> None)
      intent.instructions
  in
  match transfers with
  | [] -> Error "intent policy: expected one checked token transfer"
  | _ :: _ :: _ -> Error "intent policy: multiple token transfers rejected"
  | [ transfer ] ->
    if transfer.token_program = Programs.Token_2022 && not allow_token_2022 then
      Error "intent policy: Token-2022 requires explicit opt-in after mint-extension review"
    else if
      not
        (List.exists
           (Solana_types.Address.equal transfer.authority)
           intent.required_signers)
    then Error "intent policy: token authority is not a required signer"
    else
      let associated_creations =
        List.filter_map
          (function Create_associated_token_account creation -> Some creation | _ -> None)
          intent.instructions
      in
      if List.length associated_creations > 1 then
        Error "intent policy: multiple associated-token-account creations rejected"
      else
        let creation_matches creation =
          creation.idempotent
          && creation.token_program = transfer.token_program
          && Solana_types.Address.equal creation.mint transfer.mint
          && Solana_types.Address.equal creation.associated transfer.destination
          && List.exists
               (Solana_types.Address.equal creation.payer)
               intent.required_signers
        in
        if not (List.for_all creation_matches associated_creations) then
          Error "intent policy: associated-token-account creation does not match transfer"
        else
          let allowed = function
            | Token_transfer_checked _ | Create_associated_token_account _
            | Set_compute_unit_limit _ | Set_compute_unit_price _ -> true
            | Sol_transfer _ | Opaque _ -> false
          in
          if List.for_all allowed intent.instructions then Ok ()
          else Error "intent policy: unrelated instruction rejected"
