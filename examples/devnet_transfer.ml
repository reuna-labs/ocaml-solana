let ( let* ) = Lwt.bind

let fail message = Lwt.fail_with message
let rpc_error error = Format.asprintf "%a" Solana_rpc.Error.pp error

let call transport method_ =
  let* result = Solana_rpc_unix.Client.call transport method_ in
  match result with Ok value -> Lwt.return value | Error error -> fail (rpc_error error)

let decode_hex value =
  let digit = function
    | '0' .. '9' as char -> Char.code char - Char.code '0'
    | 'a' .. 'f' as char -> Char.code char - Char.code 'a' + 10
    | 'A' .. 'F' as char -> Char.code char - Char.code 'A' + 10
    | _ -> invalid_arg "seed must contain hexadecimal digits"
  in
  if String.length value mod 2 <> 0 then invalid_arg "seed hex must have even length";
  String.init (String.length value / 2) (fun index ->
      Char.chr ((digit value.[index * 2] lsl 4) lor digit value.[(index * 2) + 1]))

let required_env name =
  match Sys.getenv_opt name with
  | Some value when value <> "" -> value
  | _ -> invalid_arg (name ^ " must be set")

let get = function Ok value -> value | Error message -> invalid_arg message

let wait_for_confirmation transport ~signature ~last_valid_block_height =
  let rec loop () =
    let* statuses =
      call transport (Solana_rpc.Methods.get_signature_statuses [ signature ])
    in
    let* height = call transport (Solana_rpc.Methods.get_block_height ()) in
    let status = match statuses.value with [ status ] -> status | _ -> None in
    match
      Solana_rpc.Confirmation.decide ~commitment:Solana_types.Confirmed
        ~current_block_height:height ~last_valid_block_height status
    with
    | Solana_rpc.Confirmation.Succeeded -> Lwt.return_unit
    | Failed error -> fail ("transaction failed: " ^ Yojson.Safe.to_string error)
    | Expired -> fail "transaction expired before reaching confirmed commitment"
    | Pending -> let* () = Lwt_unix.sleep 0.5 in loop ()
  in
  loop ()

let run () =
  if Sys.getenv_opt "SOLANA_ENABLE_NETWORK_TESTS" <> Some "1" then (
    print_endline "Devnet smoke skipped; set SOLANA_ENABLE_NETWORK_TESTS=1 to opt in.";
    Lwt.return_unit)
  else
    let keypair =
      required_env "SOLANA_SEED_HEX" |> decode_hex |> Solana_crypto.keypair_of_seed |> get
    in
    let destination =
      required_env "SOLANA_DESTINATION" |> Solana_types.Address.of_base58 |> get
    in
    let uri =
      Sys.getenv_opt "SOLANA_RPC_URL"
      |> Option.value ~default:"https://api.devnet.solana.com" |> Uri.of_string
    in
    let transport = Solana_rpc_unix.create uri in
    let* actual_genesis = call transport (Solana_rpc.Methods.get_genesis_hash ()) in
    let expected_genesis =
      match Sys.getenv_opt "SOLANA_EXPECTED_GENESIS_HASH" with
      | None -> Solana_types.Network.devnet.genesis_hash
      | Some value -> Solana_types.Hash.of_base58 value |> get
    in
    if not (Solana_types.Hash.equal actual_genesis expected_genesis) then
      fail
        (Printf.sprintf "genesis mismatch: expected %s, RPC returned %s"
           (Solana_types.Hash.to_base58 expected_genesis)
           (Solana_types.Hash.to_base58 actual_genesis))
    else
      let* latest = call transport (Solana_rpc.Methods.get_latest_blockhash ()) in
      let payer = Solana_crypto.address keypair in
      let instructions, validate =
        match Sys.getenv_opt "SOLANA_TOKEN_MINT" with
        | None | Some "" ->
          let lamports =
            Sys.getenv_opt "SOLANA_LAMPORTS" |> Option.value ~default:"5000"
            |> Solana_types.U64.of_string |> get
          in
          ( [ Solana_transaction.Programs.set_compute_unit_limit 20_000 |> get;
              Solana_transaction.Programs.transfer ~from:payer ~to_:destination
                ~lamports ],
            Solana_transaction.Intent.validate_safe_sol_transfer )
        | Some encoded_mint ->
          let mint = Solana_types.Address.of_base58 encoded_mint |> get in
          let amount = required_env "SOLANA_TOKEN_AMOUNT" |> Solana_types.U64.of_string |> get in
          let decimals = required_env "SOLANA_TOKEN_DECIMALS" |> int_of_string in
          let token_program, allow_token_2022 =
            match Sys.getenv_opt "SOLANA_TOKEN_PROGRAM" with
            | None | Some "" | Some "token" -> Solana_transaction.Programs.Token, false
            | Some "token-2022" ->
              if Sys.getenv_opt "SOLANA_ALLOW_TOKEN_2022" <> Some "1" then
                invalid_arg
                  "Token-2022 requires SOLANA_ALLOW_TOKEN_2022=1 after mint-extension review";
              Solana_transaction.Programs.Token_2022, true
            | Some _ -> invalid_arg "SOLANA_TOKEN_PROGRAM must be token or token-2022"
          in
          let source, _ =
            Solana_transaction.Programs.associated_token_address ~owner:payer ~mint
              ~token_program
            |> get
          in
          let target, _ =
            Solana_transaction.Programs.associated_token_address ~owner:destination ~mint
              ~token_program
            |> get
          in
          let create =
            Solana_transaction.Programs.create_associated_token_account_idempotent
              ~payer ~owner:destination ~mint ~token_program
            |> get
          in
          let transfer =
            Solana_transaction.Programs.transfer_checked ~source ~mint
              ~destination:target ~authority:payer ~amount ~decimals ~token_program
            |> get
          in
          ( [ Solana_transaction.Programs.set_compute_unit_limit 50_000 |> get;
              create;
              transfer ],
            Solana_transaction.Intent.validate_safe_token_transfer
              ~allow_token_2022 )
      in
      let message =
        Solana_transaction.Message.compile_legacy ~payer
          ~recent_blockhash:latest.value.blockhash instructions
        |> get
      in
      let intent = Solana_transaction.Intent.derive message |> get in
      validate intent |> get;
      let signed =
        Solana_transaction.Transaction.create message
        |> Solana_transaction.Transaction.sign_with keypair |> get
        |> Solana_transaction.Transaction.finalize |> get
      in
      let wire = Solana_transaction.Transaction.encode signed in
      let* simulation =
        call transport (Solana_rpc.Methods.simulate_transaction ~sig_verify:true wire)
      in
      (match simulation.value.err with
      | Some error -> fail ("simulation failed: " ^ Yojson.Safe.to_string error)
      | None ->
        let* submitted = call transport (Solana_rpc.Methods.send_transaction signed) in
        let local_id = Solana_transaction.Transaction.id signed in
        if not (Solana_types.Signature.equal submitted local_id) then
          fail "RPC returned a transaction signature different from the locally signed ID"
        else
          let* () =
            wait_for_confirmation transport ~signature:submitted
              ~last_valid_block_height:latest.value.last_valid_block_height
          in
          Printf.printf "confirmed %s\n%!" (Solana_types.Signature.to_base58 submitted);
          Lwt.return_unit)

let () = Lwt_main.run (run ())
