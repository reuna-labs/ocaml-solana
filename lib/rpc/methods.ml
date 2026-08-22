type context = { slot : Solana_types.U64.t; api_version : string option }
type 'a contextual = { context : context; value : 'a }
type version = { solana_core : string; feature_set : Solana_types.U64.t option }
type latest_blockhash = { blockhash : Solana_types.Hash.t; last_valid_block_height : Solana_types.U64.t }
type confirmation_status = Processed | Confirmed | Finalized
type signature_status = { slot : Solana_types.U64.t; confirmations : int option; err : Yojson.Safe.t option; confirmation_status : confirmation_status option }
type simulation = { err : Yojson.Safe.t option; logs : string list option; units_consumed : Solana_types.U64.t option }

let fail expected value = Error (expected ^ ": " ^ Yojson.Safe.to_string value)
let u64 = function
  | `Int value -> Solana_types.U64.of_int value
  | `Intlit value | `String value -> Solana_types.U64.of_string value
  | value -> fail "expected unsigned integer" value
let hash = function `String value -> Solana_types.Hash.of_base58 value | value -> fail "expected base58 hash" value
let signature = function `String value -> Solana_types.Signature.of_base58 value | value -> fail "expected base58 signature" value
let config commitment fields = `Assoc (("commitment", `String (Solana_types.commitment_to_string commitment)) :: fields)

let context = function
  | `Assoc fields ->
    (match List.assoc_opt "slot" fields with
    | None -> Error "context.slot is missing"
    | Some slot ->
      (match u64 slot with
      | Error _ as error -> error
      | Ok slot ->
        let api_version = match List.assoc_opt "apiVersion" fields with Some (`String value) -> Some value | _ -> None in
        Ok { slot; api_version }))
  | value -> fail "expected context object" value

let contextual decode = function
  | `Assoc fields ->
    (match List.assoc_opt "context" fields, List.assoc_opt "value" fields with
    | Some context_json, Some value_json ->
      (match context context_json with
      | Error _ as error -> error
      | Ok context -> Result.map (fun value -> { context; value }) (decode value_json))
    | _ -> Error "contextual result requires context and value")
  | value -> fail "expected contextual result object" value

let get_genesis_hash () = Method.make ~name:"getGenesisHash" hash

let get_version () =
  let decode = function
    | `Assoc fields ->
      (match List.assoc_opt "solana-core" fields with
      | Some (`String solana_core) ->
        let feature_set = match List.assoc_opt "feature-set" fields with Some value -> Result.to_option (u64 value) | None -> None in
        Ok { solana_core; feature_set }
      | _ -> Error "getVersion result lacks solana-core")
    | value -> fail "expected version object" value
  in
  Method.make ~name:"getVersion" decode

let get_latest_blockhash ?(commitment = Solana_types.Confirmed) () =
  let value = function
    | `Assoc fields ->
      (match List.assoc_opt "blockhash" fields, List.assoc_opt "lastValidBlockHeight" fields with
      | Some blockhash, Some height ->
        (match hash blockhash, u64 height with
        | Ok blockhash, Ok last_valid_block_height -> Ok { blockhash; last_valid_block_height }
        | Error message, _ | _, Error message -> Error message)
      | _ -> Error "latest blockhash result lacks required fields")
    | json -> fail "expected latest blockhash object" json
  in
  Method.make ~name:"getLatestBlockhash" ~params:[ config commitment [] ] (contextual value)

let get_block_height ?(commitment = Solana_types.Confirmed) () =
  Method.make ~name:"getBlockHeight" ~params:[ config commitment [] ] u64

let get_balance ?(commitment = Solana_types.Confirmed) address =
  Method.make ~name:"getBalance"
    ~params:[ `String (Solana_types.Address.to_base58 address); config commitment [] ]
    (contextual u64)

let get_fee_for_message ?(commitment = Solana_types.Confirmed) message =
  let decode = function `Null -> Ok None | value -> Result.map Option.some (u64 value) in
  let encoded =
    match Solana_transaction.Message.encode message with
    | Ok bytes -> Base64.encode_exn bytes
    | Error message -> invalid_arg message
  in
  Method.make ~name:"getFeeForMessage" ~params:[ `String encoded; config commitment [] ] (contextual decode)

let simulation = function
  | `Assoc fields ->
    let err = match List.assoc_opt "err" fields with Some `Null | None -> None | Some value -> Some value in
    let logs =
      match List.assoc_opt "logs" fields with
      | Some (`List values) ->
        let rec collect acc = function [] -> Some (List.rev acc) | `String value :: rest -> collect (value :: acc) rest | _ -> None in
        collect [] values
      | _ -> None
    in
    let units_consumed = match List.assoc_opt "unitsConsumed" fields with Some value -> Result.to_option (u64 value) | None -> None in
    Ok { err; logs; units_consumed }
  | value -> fail "expected simulation object" value

let simulate_transaction ?(commitment = Solana_types.Confirmed) ?(sig_verify = false) wire =
  Method.make ~name:"simulateTransaction"
    ~params:[ `String (Base64.encode_exn wire); config commitment [ "encoding", `String "base64"; "sigVerify", `Bool sig_verify ] ]
    (contextual simulation)

let send_transaction ?(preflight_commitment = Solana_types.Confirmed) ?(skip_preflight = false) transaction =
  Method.make ~name:"sendTransaction"
    ~params:
      [ `String (Base64.encode_exn (Solana_transaction.Transaction.encode transaction));
        `Assoc [ "encoding", `String "base64"; "preflightCommitment", `String (Solana_types.commitment_to_string preflight_commitment); "skipPreflight", `Bool skip_preflight; "maxRetries", `Int 3 ] ]
    signature

let confirmation_status = function
  | "processed" -> Ok Processed | "confirmed" -> Ok Confirmed | "finalized" -> Ok Finalized
  | value -> Error ("unknown confirmation status: " ^ value)

let signature_status = function
  | `Null -> Ok None
  | `Assoc fields ->
    (match List.assoc_opt "slot" fields with
    | None -> Error "signature status lacks slot"
    | Some slot_json ->
      (match u64 slot_json with
      | Error _ as error -> error
      | Ok slot ->
        let confirmations = match List.assoc_opt "confirmations" fields with Some (`Int value) -> Some value | _ -> None in
        let err = match List.assoc_opt "err" fields with Some `Null | None -> None | Some value -> Some value in
        let confirmation_status = match List.assoc_opt "confirmationStatus" fields with Some (`String value) -> Result.to_option (confirmation_status value) | _ -> None in
        Ok (Some { slot; confirmations; err; confirmation_status })))
  | value -> fail "expected signature status or null" value

let get_signature_statuses ?(search_transaction_history = false) signatures =
  let rec values acc = function
    | [] -> Ok (List.rev acc)
    | value :: rest ->
      (match signature_status value with Error _ as error -> error | Ok status -> values (status :: acc) rest)
  in
  let decode = function `List items -> values [] items | value -> fail "expected status list" value in
  Method.make ~name:"getSignatureStatuses"
    ~params:[ `List (List.map (fun value -> `String (Solana_types.Signature.to_base58 value)) signatures); `Assoc [ "searchTransactionHistory", `Bool search_transaction_history ] ]
    (contextual decode)

let get_transaction ?(commitment = Solana_types.Confirmed) signature =
  let decode = function `Null -> Ok None | value -> Ok (Some value) in
  Method.make ~name:"getTransaction"
    ~params:[ `String (Solana_types.Signature.to_base58 signature); config commitment [ "encoding", `String "json"; "maxSupportedTransactionVersion", `Int 0 ] ]
    decode
