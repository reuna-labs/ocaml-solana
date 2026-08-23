let get = function Ok value -> value | Error message -> Alcotest.fail message
let bytes byte length = String.make length (Char.chr byte)
let address byte = get (Solana_types.Address.of_bytes (bytes byte 32))
let hash byte = get (Solana_types.Hash.of_bytes (bytes byte 32))
let address_of_base58 value = get (Solana_types.Address.of_base58 value)

let hex value =
  let out = Bytes.create (String.length value * 2) in
  String.iteri
    (fun index char ->
      let value = Char.code char in
      Bytes.set out (index * 2) "0123456789abcdef".[value lsr 4];
      Bytes.set out ((index * 2) + 1) "0123456789abcdef".[value land 0xf])
    value;
  Bytes.unsafe_to_string out

let unhex value =
  let nibble = function
    | '0' .. '9' as char -> Char.code char - Char.code '0'
    | 'a' .. 'f' as char -> Char.code char - Char.code 'a' + 10
    | 'A' .. 'F' as char -> Char.code char - Char.code 'A' + 10
    | _ -> Alcotest.fail "invalid fixture hex"
  in
  if String.length value mod 2 <> 0 then Alcotest.fail "odd fixture hex length";
  String.init (String.length value / 2) (fun index ->
      Char.chr ((nibble value.[index * 2] lsl 4) lor nibble value.[(index * 2) + 1]))

let transfer ~payer ~destination =
  Solana_transaction.Programs.transfer ~from:payer ~to_:destination
    ~lamports:(get (Solana_types.U64.of_int 5000))

let legacy_fixture () =
  let payer = address 1 in
  let destination = address 3 in
  let message =
    get
      (Solana_transaction.Message.compile_legacy ~payer ~recent_blockhash:(hash 2)
         [ transfer ~payer ~destination ])
  in
  let actual = get (Solana_transaction.Message.encode message) |> hex in
  let expected =
    "01000103"
    ^ String.concat "" (List.init 32 (fun _ -> "01"))
    ^ String.concat "" (List.init 32 (fun _ -> "03"))
    ^ String.concat "" (List.init 32 (fun _ -> "00"))
    ^ String.concat "" (List.init 32 (fun _ -> "02"))
    ^ "01020200010c020000008813000000000000"
  in
  Alcotest.(check string) "Agave-compatible legacy wire" expected actual;
  let decoded = get (Solana_transaction.Message.decode (get (Solana_transaction.Message.encode message))) in
  Alcotest.(check string) "round trip" actual (get (Solana_transaction.Message.encode decoded) |> hex)

let v0_fixture () =
  let payer = address 1 in
  let destination = address 3 in
  let table : Solana_transaction.Message.address_table =
    { key = address 4; addresses = [| address 9; destination |] }
  in
  let message =
    get
      (Solana_transaction.Message.compile_v0 ~payer ~recent_blockhash:(hash 2)
         ~address_tables:[ table ] [ transfer ~payer ~destination ])
  in
  let actual = get (Solana_transaction.Message.encode message) |> hex in
  let expected =
    "8001000102"
    ^ String.concat "" (List.init 32 (fun _ -> "01"))
    ^ String.concat "" (List.init 32 (fun _ -> "00"))
    ^ String.concat "" (List.init 32 (fun _ -> "02"))
    ^ "01010200020c02000000881300000000000001"
    ^ String.concat "" (List.init 32 (fun _ -> "04"))
    ^ "010100"
  in
  Alcotest.(check string) "Agave-compatible v0 wire" expected actual;
  let intent = get (Solana_transaction.Intent.derive ~address_tables:[ table ] message) in
  get (Solana_transaction.Intent.validate_safe_sol_transfer intent)

let kit_token_fixture () =
  let open Yojson.Safe.Util in
  let fixture = Yojson.Safe.from_file "../conformance/fixtures/kit-8.0.0-token.json" in
  let addresses = fixture |> member "addresses" in
  let address_field name = addresses |> member name |> to_string |> address_of_base58 in
  let owner = address_field "owner" in
  let recipient = address_field "recipient" in
  let mint = address_field "mint" in
  let expected_source = address_field "sourceAta" in
  let expected_destination = address_field "destinationAta" in
  let lookup_table = address_field "lookupTable" in
  let source, source_bump =
    get
      (Solana_transaction.Programs.associated_token_address ~owner ~mint
         ~token_program:Solana_transaction.Programs.Token)
  in
  let destination, destination_bump =
    get
      (Solana_transaction.Programs.associated_token_address ~owner:recipient ~mint
         ~token_program:Solana_transaction.Programs.Token)
  in
  Alcotest.(check string) "source ATA"
    (Solana_types.Address.to_base58 expected_source)
    (Solana_types.Address.to_base58 source);
  Alcotest.(check int) "source bump" (addresses |> member "sourceBump" |> to_int) source_bump;
  Alcotest.(check string) "destination ATA"
    (Solana_types.Address.to_base58 expected_destination)
    (Solana_types.Address.to_base58 destination);
  Alcotest.(check int) "destination bump"
    (addresses |> member "destinationBump" |> to_int)
    destination_bump;
  let create_destination =
    get
      (Solana_transaction.Programs.create_associated_token_account_idempotent
         ~payer:owner ~owner:recipient ~mint
         ~token_program:Solana_transaction.Programs.Token)
  in
  let transfer_checked =
    get
      (Solana_transaction.Programs.transfer_checked ~source ~mint ~destination
         ~authority:owner ~amount:(get (Solana_types.U64.of_int 1_234_567))
         ~decimals:6 ~token_program:Solana_transaction.Programs.Token)
  in
  let blockhash = get (Solana_types.Hash.of_base58 (Solana_types.Address.to_base58 lookup_table)) in
  let legacy =
    get
      (Solana_transaction.Message.compile_legacy ~payer:owner
         ~recent_blockhash:blockhash [ create_destination; transfer_checked ])
  in
  let agave = Yojson.Safe.from_file "../conformance/fixtures/agave-4.2.1-token.json" in
  Alcotest.(check string) "Agave legacy bytes"
    (agave |> member "legacyAtaAndTransferCheckedHex" |> to_string)
    (get (Solana_transaction.Message.encode legacy) |> hex);
  let legacy_intent = get (Solana_transaction.Intent.derive legacy) in
  get (Solana_transaction.Intent.validate_safe_token_transfer legacy_intent);
  let kit_legacy =
    fixture |> member "legacyAtaAndTransferChecked" |> member "hex" |> to_string
    |> unhex |> Solana_transaction.Message.decode |> get
  in
  let kit_legacy_intent = get (Solana_transaction.Intent.derive kit_legacy) in
  get (Solana_transaction.Intent.validate_safe_token_transfer kit_legacy_intent);
  (match Solana_transaction.Intent.validate_safe_sol_transfer legacy_intent with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "SOL policy accepted a token transfer");
  let table : Solana_transaction.Message.address_table =
    { key = lookup_table; addresses = [| mint; destination |] }
  in
  let v0 =
    get
      (Solana_transaction.Message.compile_v0 ~payer:owner ~recent_blockhash:blockhash
         ~address_tables:[ table ] [ transfer_checked ])
  in
  Alcotest.(check string) "Agave v0 bytes"
    (agave |> member "v0LookupTransferCheckedHex" |> to_string)
    (get (Solana_transaction.Message.encode v0) |> hex);
  let v0_intent =
    get (Solana_transaction.Intent.derive ~address_tables:[ table ] v0)
  in
  get (Solana_transaction.Intent.validate_safe_token_transfer v0_intent);
  let kit_v0 =
    fixture |> member "v0LookupTransferChecked" |> member "hex" |> to_string
    |> unhex |> Solana_transaction.Message.decode |> get
  in
  let kit_v0_intent =
    get (Solana_transaction.Intent.derive ~address_tables:[ table ] kit_v0)
  in
  get (Solana_transaction.Intent.validate_safe_token_transfer kit_v0_intent)

let token_policy_edges () =
  let owner = address 1 in
  let mint = address 2 in
  let destination = address 3 in
  let source = address 4 in
  let token_2022 =
    get
      (Solana_transaction.Programs.transfer_checked ~source ~mint ~destination
         ~authority:owner ~amount:(get (Solana_types.U64.of_int 1)) ~decimals:6
         ~token_program:Solana_transaction.Programs.Token_2022)
  in
  let message =
    get
      (Solana_transaction.Message.compile_legacy ~payer:owner ~recent_blockhash:(hash 5)
         [ token_2022 ])
  in
  let intent = get (Solana_transaction.Intent.derive message) in
  (match Solana_transaction.Intent.validate_safe_token_transfer intent with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "Token-2022 policy must require explicit opt-in");
  get
    (Solana_transaction.Intent.validate_safe_token_transfer ~allow_token_2022:true
       intent);
  let bad_ata =
    Solana_types.instruction
      ~program:Solana_transaction.Programs.associated_token_program
      ~accounts:
        [ Solana_types.account_meta ~signer:true ~writable:true owner;
          Solana_types.account_meta ~writable:true destination;
          Solana_types.account_meta owner;
          Solana_types.account_meta mint;
          Solana_types.account_meta Solana_transaction.Programs.system_program;
          Solana_types.account_meta
            (Solana_transaction.Programs.token_program_address
               Solana_transaction.Programs.Token) ]
      ~data:"\001"
  in
  let classic =
    get
      (Solana_transaction.Programs.transfer_checked ~source ~mint ~destination
         ~authority:owner ~amount:(get (Solana_types.U64.of_int 1)) ~decimals:6
         ~token_program:Solana_transaction.Programs.Token)
  in
  let malicious =
    get
      (Solana_transaction.Message.compile_legacy ~payer:owner ~recent_blockhash:(hash 5)
         [ bad_ata; classic ])
    |> Solana_transaction.Intent.derive |> get
  in
  match Solana_transaction.Intent.validate_safe_token_transfer malicious with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "policy accepted an ATA with the wrong derived address"

let pda_limits () =
  match
    Solana_crypto.create_program_address ~seeds:[ String.make 33 'x' ]
      ~program:(address 1)
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "PDA accepted an oversized seed"

let lookup_table_policy () =
  let payer = address 1 in
  let program = address 8 in
  let intended = address 9 in
  let attacker = address 10 in
  let table_key = address 4 in
  let instruction =
    Solana_types.instruction ~program
      ~accounts:[ Solana_types.account_meta ~writable:true intended ]
      ~data:"arbitrary-program-payload"
  in
  let trusted_table : Solana_transaction.Message.address_table =
    { key = table_key; addresses = [| intended |] }
  in
  let message =
    get
      (Solana_transaction.Message.compile_v0 ~payer ~recent_blockhash:(hash 2)
         ~address_tables:[ trusted_table ] [ instruction ])
  in
  let resolved_destination table =
    let intent =
      get (Solana_transaction.Intent.derive ~address_tables:[ table ] message)
    in
    (match intent.instructions with
    | [ Solana_transaction.Intent.Opaque { program = actual_program; accounts = [ account ]; _ } ] ->
      Alcotest.(check string) "arbitrary program"
        (Solana_types.Address.to_base58 program)
        (Solana_types.Address.to_base58 actual_program);
      (match Solana_transaction.Intent.validate_safe_sol_transfer intent with
      | Error _ -> ()
      | Ok () -> Alcotest.fail "SOL policy accepted an arbitrary program");
      account
    | _ -> Alcotest.fail "arbitrary instruction was not represented as opaque")
  in
  let trusted = resolved_destination trusted_table in
  Alcotest.(check string) "trusted lookup value"
    (Solana_types.Address.to_base58 intended)
    (Solana_types.Address.to_base58 trusted);
  let malicious_table : Solana_transaction.Message.address_table =
    { key = table_key; addresses = [| attacker |] }
  in
  let substituted = resolved_destination malicious_table in
  Alcotest.(check string) "malicious lookup substitution is visible in intent"
    (Solana_types.Address.to_base58 attacker)
    (Solana_types.Address.to_base58 substituted)

let version_rejection () =
  match Solana_transaction.Message.decode "\x81" with
  | Error message -> Alcotest.(check bool) "names unsupported version" true (String.length message > 0)
  | Ok _ -> Alcotest.fail "transaction message v1 must be rejected"

let shortvec_vectors () =
  let cases = [ 0, "00"; 127, "7f"; 128, "8001"; 16_383, "ff7f"; 16_384, "808001"; 65_535, "ffff03" ] in
  List.iter
    (fun (value, expected) ->
      let encoded = get (Solana_transaction.Shortvec.encode value) in
      Alcotest.(check string) "encoded bytes" expected (hex encoded);
      let decoded, offset = get (Solana_transaction.Shortvec.decode encoded 0) in
      Alcotest.(check int) "decoded value" value decoded;
      Alcotest.(check int) "consumed" (String.length encoded) offset)
    cases;
  List.iter
    (fun invalid ->
      match Solana_transaction.Shortvec.decode invalid 0 with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "non-canonical shortvec accepted")
    [ "\x80\x00"; "\xff\x00"; "\x80\x80\x04"; "\x80\x80\x80" ]

let shortvec_property =
  QCheck.Test.make ~name:"shortvec round trip" ~count:10_000 QCheck.(0 -- 65_535)
    (fun value ->
      match Solana_transaction.Shortvec.encode value with
      | Error _ -> false
      | Ok encoded -> Solana_transaction.Shortvec.decode encoded 0 = Ok (value, String.length encoded))

let signing () =
  let keypair = get (Solana_crypto.keypair_of_seed (bytes 7 32)) in
  let payer = Solana_crypto.address keypair in
  let message =
    get
      (Solana_transaction.Message.compile_legacy ~payer ~recent_blockhash:(hash 2)
         [ transfer ~payer ~destination:(address 3) ])
  in
  let unsigned = Solana_transaction.Transaction.create message in
  Alcotest.(check int) "one missing signer" 1 (List.length (Solana_transaction.Transaction.missing_signers unsigned));
  let signed = get (Solana_transaction.Transaction.sign_with keypair unsigned) |> Solana_transaction.Transaction.finalize |> get in
  let wire = Solana_transaction.Transaction.encode signed in
  let decoded = get (Solana_transaction.Transaction.decode wire) in
  Alcotest.(check string) "signed round trip" wire (Solana_transaction.Transaction.encode decoded);
  Alcotest.(check string) "ID is first signature"
    (Solana_types.Signature.to_base58 (Solana_transaction.Transaction.id signed))
    (Solana_types.Signature.to_base58 (Solana_transaction.Transaction.id decoded))

module Mock_http = struct
  type t = string
  type 'a io = 'a
  let return value = value
  let bind value function_ = function_ value
  let post response ~body:_ = Ok response
end

module Mock_provider = Solana_rpc.Provider.Of_http (Mock_http)
module Mock_client = Solana_rpc.Provider.Make (Mock_provider)

let rpc_decode () =
  let response =
    {|{"jsonrpc":"2.0","id":0,"result":{"context":{"slot":42,"apiVersion":"4.2.1"},"value":{"blockhash":"11111111111111111111111111111111","lastValidBlockHeight":99}}}|}
  in
  match Mock_client.call response (Solana_rpc.Methods.get_latest_blockhash ()) with
  | Error error -> Alcotest.failf "%a" Solana_rpc.Error.pp error
  | Ok result ->
    Alcotest.(check string) "height" "99" (Solana_types.U64.to_string result.value.last_valid_block_height);
    Alcotest.(check string) "hash" "11111111111111111111111111111111" (Solana_types.Hash.to_base58 result.value.blockhash)

let confirmation () =
  let status : Solana_rpc.Methods.signature_status =
    { slot = get (Solana_types.U64.of_int 10); confirmations = Some 1; err = None;
      confirmation_status = Some Solana_rpc.Methods.Confirmed }
  in
  let decision =
    Solana_rpc.Confirmation.decide ~commitment:Solana_types.Confirmed
      ~current_block_height:(get (Solana_types.U64.of_int 90))
      ~last_valid_block_height:(get (Solana_types.U64.of_int 99)) (Some status)
  in
  match decision with Solana_rpc.Confirmation.Succeeded -> () | _ -> Alcotest.fail "expected confirmation"

let context value : _ Solana_rpc.Methods.contextual =
  { context = { slot = get (Solana_types.U64.of_int 1); api_version = None };
    value }

let signed_transfer keypair recent_blockhash =
  let payer = Solana_crypto.address keypair in
  Solana_transaction.Message.compile_legacy ~payer ~recent_blockhash
    [ transfer ~payer ~destination:(address 3) ]
  |> get |> Solana_transaction.Transaction.create
  |> Solana_transaction.Transaction.sign_with keypair |> get
  |> Solana_transaction.Transaction.finalize |> get

let advance state event = Solana_rpc.Submission.advance state event |> get

let submission_to_confirmation config keypair latest =
  let transaction = signed_transfer keypair latest.Solana_rpc.Methods.blockhash in
  let state = Solana_rpc.Submission.start config in
  let state = advance state (Solana_rpc.Submission.Latest_blockhash (context latest)) in
  let state = advance state (Solana_rpc.Submission.Signed transaction) in
  let simulation : Solana_rpc.Methods.simulation =
    { err = None; logs = None; units_consumed = None }
  in
  let state = advance state (Solana_rpc.Submission.Simulation (context simulation)) in
  let signature = Solana_transaction.Transaction.id transaction in
  let state = advance state (Solana_rpc.Submission.Submitted signature) in
  state, signature

let submission_stale_blockhash_retry () =
  let keypair = get (Solana_crypto.keypair_of_seed (bytes 12 32)) in
  let config =
    Solana_rpc.Submission.config ~max_attempts:2 ~max_confirmation_polls:3
      ~commitment:Solana_types.Confirmed
    |> get
  in
  let latest : Solana_rpc.Methods.latest_blockhash =
    { blockhash = hash 20;
      last_valid_block_height = get (Solana_types.U64.of_int 100) }
  in
  let transaction = signed_transfer keypair latest.blockhash in
  let state = Solana_rpc.Submission.start config in
  let state = advance state (Solana_rpc.Submission.Latest_blockhash (context latest)) in
  let state = advance state (Solana_rpc.Submission.Signed transaction) in
  let simulation : Solana_rpc.Methods.simulation =
    { err = None; logs = None; units_consumed = None }
  in
  let state = advance state (Solana_rpc.Submission.Simulation (context simulation)) in
  let stale =
    Solana_rpc.Error.Rpc
      { code = -32002; message = "Transaction simulation failed";
        data = Some (`Assoc [ "err", `String "BlockhashNotFound" ]) }
  in
  let state = advance state (Solana_rpc.Submission.Rpc_error stale) in
  Alcotest.(check int) "fresh attempt" 2
    (Solana_rpc.Submission.attempts_started state);
  match Solana_rpc.Submission.action state with
  | Fetch_latest_blockhash -> ()
  | _ -> Alcotest.fail "stale blockhash did not request a fresh blockhash"

let submission_expiry_and_timeout () =
  let keypair = get (Solana_crypto.keypair_of_seed (bytes 13 32)) in
  let config =
    Solana_rpc.Submission.config ~max_attempts:2 ~max_confirmation_polls:2
      ~commitment:Solana_types.Confirmed
    |> get
  in
  let latest : Solana_rpc.Methods.latest_blockhash =
    { blockhash = hash 21;
      last_valid_block_height = get (Solana_types.U64.of_int 100) }
  in
  let state, _signature = submission_to_confirmation config keypair latest in
  let state =
    advance state
      (Solana_rpc.Submission.Signature_statuses (context [ None ]))
  in
  let state =
    advance state
      (Solana_rpc.Submission.Block_height (get (Solana_types.U64.of_int 101)))
  in
  Alcotest.(check int) "expired attempt refresh" 2
    (Solana_rpc.Submission.attempts_started state);
  let latest =
    { Solana_rpc.Methods.blockhash = hash 22;
      last_valid_block_height = get (Solana_types.U64.of_int 200) }
  in
  let state, _signature = submission_to_confirmation config keypair latest in
  let pending state height =
    let state =
      advance state
        (Solana_rpc.Submission.Signature_statuses (context [ None ]))
    in
    advance state
      (Solana_rpc.Submission.Block_height (get (Solana_types.U64.of_int height)))
  in
  let state = pending state 150 in
  let state = advance state Solana_rpc.Submission.Waited in
  let state = pending state 151 in
  match Solana_rpc.Submission.action state with
  | Finished (Failed (Confirmation_timeout 2)) -> ()
  | _ -> Alcotest.fail "confirmation polling did not stop at its deterministic timeout"

let websocket_reconnect () =
  let keypair = get (Solana_crypto.keypair_of_seed (bytes 14 32)) in
  let signature =
    signed_transfer keypair (hash 23) |> Solana_transaction.Transaction.id
  in
  let state = Solana_rpc.Subscriptions.create () in
  let state, signature_id, signature_commands =
    Solana_rpc.Subscriptions.add state
      (Signature
         { signature; commitment = Solana_types.Confirmed;
           enable_received_notification = true })
  in
  Alcotest.(check int) "offline signature commands" 0 (List.length signature_commands);
  let state, account_id, _ =
    Solana_rpc.Subscriptions.add state
      (Account
         { address = address 24; commitment = Solana_types.Confirmed;
           encoding = Base64 })
  in
  let state, commands = Solana_rpc.Subscriptions.connected state in
  Alcotest.(check int) "initial subscriptions" 2 (List.length commands);
  let state, _, _ =
    Solana_rpc.Subscriptions.receive state
      {|{"jsonrpc":"2.0","id":0,"result":100}|}
    |> get
  in
  let state, _, _ =
    Solana_rpc.Subscriptions.receive state
      {|{"jsonrpc":"2.0","id":1,"result":200}|}
    |> get
  in
  Alcotest.(check int) "active subscriptions" 2
    (Solana_rpc.Subscriptions.active_count state);
  let state = Solana_rpc.Subscriptions.disconnected state in
  Alcotest.(check int) "server IDs dropped" 0
    (Solana_rpc.Subscriptions.active_count state);
  let state, commands = Solana_rpc.Subscriptions.connected state in
  Alcotest.(check int) "resubscriptions" 2 (List.length commands);
  let state, _, _ =
    Solana_rpc.Subscriptions.receive state
      {|{"jsonrpc":"2.0","id":2,"result":300}|}
    |> get
  in
  let state, _, _ =
    Solana_rpc.Subscriptions.receive state
      {|{"jsonrpc":"2.0","id":3,"result":400}|}
    |> get
  in
  let state, outputs, _ =
    Solana_rpc.Subscriptions.receive state
      {|{"jsonrpc":"2.0","method":"signatureNotification","params":{"subscription":300,"result":"receivedSignature"}}|}
    |> get
  in
  Alcotest.(check int) "received notification" 1 (List.length outputs);
  Alcotest.(check int) "one-shot remains until result" 2
    (Solana_rpc.Subscriptions.desired_count state);
  let state, outputs, _ =
    Solana_rpc.Subscriptions.receive state
      {|{"jsonrpc":"2.0","method":"signatureNotification","params":{"subscription":300,"result":{"context":{"slot":9},"value":{"err":null}}}}|}
    |> get
  in
  (match outputs with
  | [ Notification { local_id; _ } ] ->
    Alcotest.(check int) "signature local ID" signature_id local_id
  | _ -> Alcotest.fail "missing final signature notification");
  Alcotest.(check int) "one-shot removed" 1
    (Solana_rpc.Subscriptions.desired_count state);
  let state, outputs, _ =
    Solana_rpc.Subscriptions.receive state
      {|{"jsonrpc":"2.0","method":"accountNotification","params":{"subscription":400,"result":{"context":{"slot":10},"value":{"lamports":1}}}}|}
    |> get
  in
  (match outputs with
  | [ Notification { local_id; _ } ] ->
    Alcotest.(check int) "account local ID" account_id local_id
  | _ -> Alcotest.fail "missing account notification");
  Alcotest.(check int) "account remains desired" 1
    (Solana_rpc.Subscriptions.desired_count state)

let () =
  Alcotest.run "ocaml-solana"
    [ "wire", [ Alcotest.test_case "legacy golden" `Quick legacy_fixture; Alcotest.test_case "v0 golden" `Quick v0_fixture; Alcotest.test_case "Kit token vectors" `Quick kit_token_fixture; Alcotest.test_case "reject v1" `Quick version_rejection ];
      "token", [ Alcotest.test_case "policy edges" `Quick token_policy_edges; Alcotest.test_case "PDA limits" `Quick pda_limits ];
      "policy", [ Alcotest.test_case "lookup-table substitution" `Quick lookup_table_policy ];
      "shortvec", [ Alcotest.test_case "boundary vectors" `Quick shortvec_vectors; QCheck_alcotest.to_alcotest shortvec_property ];
      "signing", [ Alcotest.test_case "Ed25519 transaction" `Quick signing ];
      "rpc",
      [ Alcotest.test_case "decode latest blockhash" `Quick rpc_decode;
        Alcotest.test_case "confirmation state" `Quick confirmation;
        Alcotest.test_case "stale blockhash retry" `Quick submission_stale_blockhash_retry;
        Alcotest.test_case "expiry and timeout" `Quick submission_expiry_and_timeout;
        Alcotest.test_case "WebSocket reconnect" `Quick websocket_reconnect ] ]
