module String_map = Map.Make (String)

type header = {
  num_required_signatures : int;
  num_readonly_signed_accounts : int;
  num_readonly_unsigned_accounts : int;
}

type compiled_instruction = {
  program_id_index : int;
  account_indexes : int list;
  data : string;
}

type address_table_lookup = {
  table : Solana_types.Address.t;
  writable_indexes : int list;
  readonly_indexes : int list;
}

type address_table = {
  key : Solana_types.Address.t;
  addresses : Solana_types.Address.t array;
}

type legacy = {
  header : header;
  static_accounts : Solana_types.Address.t list;
  recent_blockhash : Solana_types.Hash.t;
  instructions : compiled_instruction list;
}

type v0 = {
  header : header;
  static_accounts : Solana_types.Address.t list;
  recent_blockhash : Solana_types.Hash.t;
  instructions : compiled_instruction list;
  address_table_lookups : address_table_lookup list;
}

type t = Legacy of legacy | V0 of v0

type flags = { address : Solana_types.Address.t; signer : bool; writable : bool; invoked : bool }

let key address = Solana_types.Address.to_bytes address

let merge_flags map address ~signer ~writable ~invoked =
  let raw = key address in
  let next =
    match String_map.find_opt raw map with
    | None -> { address; signer; writable; invoked }
    | Some previous ->
      { address;
        signer = previous.signer || signer;
        writable = previous.writable || writable;
        invoked = previous.invoked || invoked }
  in
  String_map.add raw next map

let collect payer instructions =
  let initial = merge_flags String_map.empty payer ~signer:true ~writable:true ~invoked:false in
  List.fold_left
    (fun map instruction ->
      let map =
        merge_flags map instruction.Solana_types.program ~signer:false ~writable:false
          ~invoked:true
      in
      List.fold_left
        (fun map meta ->
          merge_flags map meta.Solana_types.address ~signer:meta.signer
            ~writable:meta.writable ~invoked:false)
        map instruction.accounts)
    initial instructions

let partition_static payer map =
  let payer_key = key payer in
  let groups = Array.make 4 [] in
  String_map.iter
    (fun raw item ->
      if raw <> payer_key then
        let index =
          match item.signer, item.writable with
          | true, true -> 0
          | true, false -> 1
          | false, true -> 2
          | false, false -> 3
        in
        groups.(index) <- item.address :: groups.(index))
    map;
  Array.iteri (fun index values -> groups.(index) <- List.rev values) groups;
  payer :: List.concat (Array.to_list groups)

let header_of_static map accounts =
  let flags address = String_map.find (key address) map in
  let required = List.fold_left (fun n address -> if (flags address).signer then n + 1 else n) 0 accounts in
  let readonly_signed =
    List.fold_left
      (fun n address ->
        let item = flags address in
        if item.signer && not item.writable then n + 1 else n)
      0 accounts
  in
  let readonly_unsigned =
    List.fold_left
      (fun n address ->
        let item = flags address in
        if not item.signer && not item.writable then n + 1 else n)
      0 accounts
  in
  { num_required_signatures = required;
    num_readonly_signed_accounts = readonly_signed;
    num_readonly_unsigned_accounts = readonly_unsigned }

let index_map accounts =
  List.mapi (fun index address -> key address, index) accounts
  |> List.fold_left (fun map (raw, index) -> String_map.add raw index map) String_map.empty

let compile_instructions accounts instructions =
  if List.length accounts > 256 then Error "message: more than 256 account keys"
  else
    let indexes = index_map accounts in
    let lookup address =
      match String_map.find_opt (key address) indexes with
      | Some index -> Ok index
      | None -> Error "message: instruction account missing from key space"
    in
    let rec account_indexes acc = function
      | [] -> Ok (List.rev acc)
      | meta :: rest ->
        (match lookup meta.Solana_types.address with
        | Error _ as error -> error
        | Ok index -> account_indexes (index :: acc) rest)
    in
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | instruction :: rest ->
        (match lookup instruction.Solana_types.program with
        | Error _ as error -> error
        | Ok 0 -> Error "message: payer cannot be invoked as a program"
        | Ok program_id_index ->
          (match account_indexes [] instruction.accounts with
          | Error _ as error -> error
          | Ok account_indexes ->
            loop ({ program_id_index; account_indexes; data = instruction.data } :: acc) rest))
    in
    loop [] instructions

let compile_legacy ~payer ~recent_blockhash instructions =
  let flags = collect payer instructions in
  let static_accounts = partition_static payer flags in
  match compile_instructions static_accounts instructions with
  | Error _ as error -> error
  | Ok instructions ->
    Ok
      (Legacy
         { header = header_of_static flags static_accounts;
           static_accounts;
           recent_blockhash;
           instructions })

let find_table_index address table =
  let rec loop index =
    if index = Array.length table.addresses then None
    else if Solana_types.Address.equal address table.addresses.(index) then Some index
    else loop (index + 1)
  in
  loop 0

let compile_v0 ~payer ~recent_blockhash ~address_tables instructions =
  let all_flags = collect payer instructions in
  let remaining = ref all_flags in
  let loaded_writable = ref [] in
  let loaded_readonly = ref [] in
  let lookups = ref [] in
  List.iter
    (fun table ->
      let writable = ref [] in
      let readonly = ref [] in
      String_map.iter
        (fun raw item ->
          if (not item.signer) && not item.invoked then
            match find_table_index item.address table with
            | None -> ()
            | Some index when index > 255 -> ()
            | Some index ->
              remaining := String_map.remove raw !remaining;
              if item.writable then (
                writable := index :: !writable;
                loaded_writable := item.address :: !loaded_writable)
              else (
                readonly := index :: !readonly;
                loaded_readonly := item.address :: !loaded_readonly))
        !remaining;
      if !writable <> [] || !readonly <> [] then
        lookups :=
          { table = table.key;
            writable_indexes = List.rev !writable;
            readonly_indexes = List.rev !readonly }
          :: !lookups)
    address_tables;
  let static_accounts = partition_static payer !remaining in
  let loaded_writable = List.rev !loaded_writable in
  let loaded_readonly = List.rev !loaded_readonly in
  let all_accounts = static_accounts @ loaded_writable @ loaded_readonly in
  match compile_instructions all_accounts instructions with
  | Error _ as error -> error
  | Ok instructions ->
    Ok
      (V0
         { header = header_of_static !remaining static_accounts;
           static_accounts;
           recent_blockhash;
           instructions;
           address_table_lookups = List.rev !lookups })

let add_byte buffer value =
  if value < 0 || value > 255 then Error "message: byte value outside [0, 255]"
  else (Buffer.add_char buffer (Char.chr value); Ok ())

let add_shortvec buffer value =
  match Shortvec.encode value with
  | Error _ as error -> error
  | Ok encoded -> Buffer.add_string buffer encoded; Ok ()

let add_addresses buffer addresses =
  match add_shortvec buffer (List.length addresses) with
  | Error _ as error -> error
  | Ok () ->
    List.iter (fun address -> Buffer.add_string buffer (Solana_types.Address.to_bytes address)) addresses;
    Ok ()

let add_bytes buffer bytes =
  match add_shortvec buffer (String.length bytes) with
  | Error _ as error -> error
  | Ok () -> Buffer.add_string buffer bytes; Ok ()

let add_indexes buffer indexes =
  match add_shortvec buffer (List.length indexes) with
  | Error _ as error -> error
  | Ok () ->
    let rec loop = function
      | [] -> Ok ()
      | index :: rest ->
        (match add_byte buffer index with Error _ as error -> error | Ok () -> loop rest)
    in
    loop indexes

let add_instruction buffer instruction =
  match add_byte buffer instruction.program_id_index with
  | Error _ as error -> error
  | Ok () ->
    (match add_indexes buffer instruction.account_indexes with
    | Error _ as error -> error
    | Ok () -> add_bytes buffer instruction.data)

let add_list add buffer values =
  match add_shortvec buffer (List.length values) with
  | Error _ as error -> error
  | Ok () ->
    let rec loop = function
      | [] -> Ok ()
      | item :: rest ->
        (match add buffer item with Error _ as error -> error | Ok () -> loop rest)
    in
    loop values

let add_header buffer header =
  match add_byte buffer header.num_required_signatures with
  | Error _ as error -> error
  | Ok () ->
    (match add_byte buffer header.num_readonly_signed_accounts with
    | Error _ as error -> error
    | Ok () -> add_byte buffer header.num_readonly_unsigned_accounts)

let add_lookup buffer lookup =
  Buffer.add_string buffer (Solana_types.Address.to_bytes lookup.table);
  match add_indexes buffer lookup.writable_indexes with
  | Error _ as error -> error
  | Ok () -> add_indexes buffer lookup.readonly_indexes

let encode_common buffer header static_accounts recent_blockhash instructions =
  match add_header buffer header with
  | Error _ as error -> error
  | Ok () ->
    (match add_addresses buffer static_accounts with
    | Error _ as error -> error
    | Ok () ->
      Buffer.add_string buffer (Solana_types.Hash.to_bytes recent_blockhash);
      add_list add_instruction buffer instructions)

let validate header static_accounts instructions loaded_count =
  let static_count = List.length static_accounts in
  let total = static_count + loaded_count in
  let duplicate =
    let seen = Hashtbl.create static_count in
    List.exists
      (fun address ->
        let raw = key address in
        if Hashtbl.mem seen raw then true else (Hashtbl.add seen raw (); false))
      static_accounts
  in
  if static_count = 0 then Error "message: empty static account list"
  else if total > 256 then Error "message: more than 256 account keys"
  else if duplicate then Error "message: duplicate static account key"
  else if header.num_required_signatures = 0 || header.num_required_signatures > static_count then
    Error "message: invalid required signature count"
  else if header.num_readonly_signed_accounts >= header.num_required_signatures then
    Error "message: no writable signed payer"
  else if
    header.num_readonly_unsigned_accounts > static_count - header.num_required_signatures
  then Error "message: invalid readonly unsigned count"
  else
    let valid instruction =
      instruction.program_id_index > 0
      && instruction.program_id_index < static_count
      && List.for_all (fun index -> index >= 0 && index < total) instruction.account_indexes
    in
    if List.for_all valid instructions then Ok ()
    else Error "message: instruction contains an invalid account index"

let loaded_count lookups =
  List.fold_left
    (fun count lookup ->
      count + List.length lookup.writable_indexes + List.length lookup.readonly_indexes)
    0 lookups

let validate_for_encode = function
  | Legacy message ->
    if message.header.num_required_signatures >= 128 then
      Error "message: legacy signature count sets the version prefix bit"
    else validate message.header message.static_accounts message.instructions 0
  | V0 message ->
    if
      List.exists
        (fun lookup -> lookup.writable_indexes = [] && lookup.readonly_indexes = [])
        message.address_table_lookups
    then Error "message: empty address table lookup"
    else
      validate message.header message.static_accounts message.instructions
        (loaded_count message.address_table_lookups)

let encode message =
  let buffer = Buffer.create 256 in
  match validate_for_encode message with
  | Error _ as error -> error
  | Ok () ->
    let result =
      match message with
      | Legacy message ->
        encode_common buffer message.header message.static_accounts message.recent_blockhash
          message.instructions
      | V0 message ->
        Buffer.add_char buffer '\x80';
        (match
           encode_common buffer message.header message.static_accounts message.recent_blockhash
             message.instructions
         with
        | Error _ as error -> error
        | Ok () -> add_list add_lookup buffer message.address_table_lookups)
    in
    match result with Error _ as error -> error | Ok () -> Ok (Buffer.contents buffer)

type cursor = { source : string; mutable offset : int }

let remaining cursor = String.length cursor.source - cursor.offset

let read_byte cursor =
  if remaining cursor < 1 then Error "message: truncated byte"
  else let value = Char.code cursor.source.[cursor.offset] in cursor.offset <- cursor.offset + 1; Ok value

let read_exact cursor length =
  if length < 0 || remaining cursor < length then Error "message: truncated bytes"
  else
    let bytes = String.sub cursor.source cursor.offset length in
    cursor.offset <- cursor.offset + length;
    Ok bytes

let read_shortvec cursor =
  match Shortvec.decode cursor.source cursor.offset with
  | Error _ as error -> error
  | Ok (value, offset) -> cursor.offset <- offset; Ok value

let read_list read cursor =
  match read_shortvec cursor with
  | Error _ as error -> error
  | Ok length ->
    let rec loop count acc =
      if count = 0 then Ok (List.rev acc)
      else match read cursor with Error _ as error -> error | Ok item -> loop (count - 1) (item :: acc)
    in
    loop length []

let read_address cursor =
  match read_exact cursor 32 with
  | Error _ as error -> error
  | Ok bytes -> Solana_types.Address.of_bytes bytes

let read_indexes cursor = read_list read_byte cursor

let read_instruction cursor =
  match read_byte cursor with
  | Error _ as error -> error
  | Ok program_id_index ->
    (match read_indexes cursor with
    | Error _ as error -> error
    | Ok account_indexes ->
      (match read_shortvec cursor with
      | Error _ as error -> error
      | Ok length ->
        (match read_exact cursor length with
        | Error _ as error -> error
        | Ok data -> Ok { program_id_index; account_indexes; data })))

let read_lookup cursor =
  match read_address cursor with
  | Error _ as error -> error
  | Ok table ->
    (match read_indexes cursor with
    | Error _ as error -> error
    | Ok writable_indexes ->
      (match read_indexes cursor with
      | Error _ as error -> error
      | Ok readonly_indexes -> Ok { table; writable_indexes; readonly_indexes }))

let read_header cursor =
  match read_byte cursor with
  | Error _ as error -> error
  | Ok num_required_signatures ->
    (match read_byte cursor with
    | Error _ as error -> error
    | Ok num_readonly_signed_accounts ->
      (match read_byte cursor with
      | Error _ as error -> error
      | Ok num_readonly_unsigned_accounts ->
        Ok
          { num_required_signatures;
            num_readonly_signed_accounts;
            num_readonly_unsigned_accounts }))

let decode_common cursor =
  match read_header cursor with
  | Error _ as error -> error
  | Ok header ->
    (match read_list read_address cursor with
    | Error _ as error -> error
    | Ok static_accounts ->
      (match read_exact cursor 32 with
      | Error _ as error -> error
      | Ok hash_bytes ->
        (match Solana_types.Hash.of_bytes hash_bytes with
        | Error _ as error -> error
        | Ok recent_blockhash ->
          (match read_list read_instruction cursor with
          | Error _ as error -> error
          | Ok instructions -> Ok (header, static_accounts, recent_blockhash, instructions)))))

let decode source =
  if String.length source = 0 then Error "message: empty input"
  else
    let first = Char.code source.[0] in
    let versioned = first land 0x80 <> 0 in
    let version = first land 0x7f in
    if versioned && version <> 0 then Error (Printf.sprintf "message: unsupported version %d" version)
    else
      let cursor = { source; offset = if versioned then 1 else 0 } in
      match decode_common cursor with
      | Error _ as error -> error
      | Ok (header, static_accounts, recent_blockhash, instructions) ->
        if not versioned then
          (match validate header static_accounts instructions 0 with
          | Error _ as error -> error
          | Ok () ->
            if remaining cursor <> 0 then Error "message: trailing bytes"
            else Ok (Legacy { header; static_accounts; recent_blockhash; instructions }))
        else
          (match read_list read_lookup cursor with
          | Error _ as error -> error
          | Ok address_table_lookups ->
            let loaded_count = loaded_count address_table_lookups in
            let empty_lookup =
              List.exists
                (fun lookup -> lookup.writable_indexes = [] && lookup.readonly_indexes = [])
                address_table_lookups
            in
            if empty_lookup then Error "message: empty address table lookup"
            else
              (match validate header static_accounts instructions loaded_count with
              | Error _ as error -> error
              | Ok () ->
                if remaining cursor <> 0 then Error "message: trailing bytes"
                else
                  Ok
                    (V0
                       { header;
                         static_accounts;
                         recent_blockhash;
                         instructions;
                         address_table_lookups })))

let header = function Legacy message -> message.header | V0 message -> message.header
let static_accounts = function Legacy message -> message.static_accounts | V0 message -> message.static_accounts
let recent_blockhash = function Legacy message -> message.recent_blockhash | V0 message -> message.recent_blockhash
let with_recent_blockhash recent_blockhash = function
  | Legacy message -> Legacy { message with recent_blockhash }
  | V0 message -> V0 { message with recent_blockhash }
let instructions = function Legacy message -> message.instructions | V0 message -> message.instructions

let required_signers message =
  let count = (header message).num_required_signatures in
  let rec take count = function
    | _ when count = 0 -> []
    | [] -> []
    | item :: rest -> item :: take (count - 1) rest
  in
  take count (static_accounts message)

let resolve_lookup tables lookup =
  match List.find_opt (fun table -> Solana_types.Address.equal table.key lookup.table) tables with
  | None -> Error ("message: missing address table " ^ Solana_types.Address.to_base58 lookup.table)
  | Some table ->
    let pick indexes =
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | index :: rest ->
          if index >= Array.length table.addresses then Error "message: address table index out of range"
          else loop (table.addresses.(index) :: acc) rest
      in
      loop [] indexes
    in
    (match pick lookup.writable_indexes with
    | Error _ as error -> error
    | Ok writable ->
      match pick lookup.readonly_indexes with
      | Error _ as error -> error
      | Ok readonly -> Ok (writable, readonly))

let resolve_accounts ~address_tables = function
  | Legacy message -> Ok message.static_accounts
  | V0 message ->
    let rec loop writable readonly = function
      | [] -> Ok (message.static_accounts @ List.concat (List.rev writable) @ List.concat (List.rev readonly))
      | lookup :: rest ->
        (match resolve_lookup address_tables lookup with
        | Error _ as error -> error
        | Ok (loaded_writable, loaded_readonly) ->
          loop (loaded_writable :: writable) (loaded_readonly :: readonly) rest)
    in
    loop [] [] message.address_table_lookups
