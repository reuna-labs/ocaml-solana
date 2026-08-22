let address encoded =
  match Solana_types.Address.of_base58 encoded with
  | Ok address -> address
  | Error message -> invalid_arg message

let system_program = address "11111111111111111111111111111111"
let compute_budget_program = address "ComputeBudget111111111111111111111111111111"

let transfer ~from ~to_ ~lamports =
  let data =
    Web3_codec_borsh.encode_u32 2
    ^ Web3_codec_borsh.encode_u64 (Solana_types.U64.to_z lamports)
  in
  Solana_types.instruction ~program:system_program
    ~accounts:
      [ Solana_types.account_meta ~signer:true ~writable:true from;
        Solana_types.account_meta ~writable:true to_ ]
    ~data

let set_compute_unit_limit units =
  if units < 0 then Error "compute unit limit: negative value"
  else
    match Web3_codec_borsh.encode_u32 units with
    | data ->
      Ok
        (Solana_types.instruction ~program:compute_budget_program ~accounts:[]
           ~data:("\002" ^ data))
    | exception Invalid_argument message -> Error message

let set_compute_unit_price price =
  let data = "\003" ^ Web3_codec_borsh.encode_u64 (Solana_types.U64.to_z price) in
  Solana_types.instruction ~program:compute_budget_program ~accounts:[] ~data

let read_u32 source offset =
  if offset + 4 > String.length source then None
  else
    Some
      (Char.code source.[offset]
      lor (Char.code source.[offset + 1] lsl 8)
      lor (Char.code source.[offset + 2] lsl 16)
      lor (Char.code source.[offset + 3] lsl 24))

let read_u64 source offset =
  if offset + 8 > String.length source then None
  else
    let value = ref Z.zero in
    for index = 7 downto 0 do
      value := Z.logor (Z.shift_left !value 8) (Z.of_int (Char.code source.[offset + index]))
    done;
    match Solana_types.U64.of_z !value with Ok value -> Some value | Error _ -> None

let decode_transfer data =
  if String.length data <> 12 then None
  else match read_u32 data 0 with Some 2 -> read_u64 data 4 | _ -> None

let decode_compute_unit_limit data =
  if String.length data <> 5 || data.[0] <> '\002' then None else read_u32 data 1

let decode_compute_unit_price data =
  if String.length data <> 9 || data.[0] <> '\003' then None else read_u64 data 1
