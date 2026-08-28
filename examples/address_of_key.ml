let fail message =
  prerr_endline ("solana-address-of-key: " ^ message);
  exit 2

let hex_value = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
  | _ -> fail "stdin must contain a hexadecimal key"

let decode_hex value =
  let length = String.length value in
  if length <> 64 then fail "stdin must contain exactly 32 key bytes";
  String.init (length / 2) (fun index ->
      let offset = index * 2 in
      Char.chr
        ((hex_value value.[offset] lsl 4) lor hex_value value.[offset + 1]))

let read_stdin () =
  let input = Buffer.create 64 in
  (try
     while true do
       Buffer.add_string input (input_line stdin)
     done
   with End_of_file -> ());
  String.trim (Buffer.contents input)

let () =
  let seed = read_stdin () |> decode_hex in
  match Solana_crypto.keypair_of_seed seed with
  | Error _ -> fail "invalid Ed25519 seed"
  | Ok keypair ->
      keypair |> Solana_crypto.address |> Solana_types.Address.to_base58
      |> print_endline
