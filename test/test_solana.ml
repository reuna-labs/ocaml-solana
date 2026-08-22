let get = function Ok value -> value | Error message -> Alcotest.fail message
let bytes byte length = String.make length (Char.chr byte)
let address byte = get (Solana_types.Address.of_bytes (bytes byte 32))
let hash byte = get (Solana_types.Hash.of_bytes (bytes byte 32))

let hex value =
  let out = Bytes.create (String.length value * 2) in
  String.iteri
    (fun index char ->
      let value = Char.code char in
      Bytes.set out (index * 2) "0123456789abcdef".[value lsr 4];
      Bytes.set out ((index * 2) + 1) "0123456789abcdef".[value land 0xf])
    value;
  Bytes.unsafe_to_string out

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

let () =
  Alcotest.run "ocaml-solana"
    [ "wire", [ Alcotest.test_case "legacy golden" `Quick legacy_fixture; Alcotest.test_case "v0 golden" `Quick v0_fixture; Alcotest.test_case "reject v1" `Quick version_rejection ];
      "shortvec", [ Alcotest.test_case "boundary vectors" `Quick shortvec_vectors; QCheck_alcotest.to_alcotest shortvec_property ];
      "signing", [ Alcotest.test_case "Ed25519 transaction" `Quick signing ];
      "rpc", [ Alcotest.test_case "decode latest blockhash" `Quick rpc_decode; Alcotest.test_case "confirmation state" `Quick confirmation ] ]
