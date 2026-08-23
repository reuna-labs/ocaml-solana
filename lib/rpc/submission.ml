type config = {
  max_attempts : int;
  max_confirmation_polls : int;
  commitment : Solana_types.commitment;
}

let config ~max_attempts ~max_confirmation_polls ~commitment =
  if max_attempts <= 0 then Error "max_attempts must be positive"
  else if max_confirmation_polls <= 0 then
    Error "max_confirmation_polls must be positive"
  else Ok { max_attempts; max_confirmation_polls; commitment }

type failure =
  | Rpc of Error.t
  | Simulation_failed of Yojson.Safe.t
  | Transaction_failed of Yojson.Safe.t
  | Signature_mismatch of {
      expected : Solana_types.Signature.t;
      actual : Solana_types.Signature.t;
    }
  | Signed_with_wrong_blockhash of {
      expected : Solana_types.Hash.t;
      actual : Solana_types.Hash.t;
    }
  | Attempts_exhausted of int
  | Confirmation_timeout of int

type outcome = Confirmed of Solana_types.Signature.t | Failed of failure

let pp_failure formatter = function
  | Rpc error -> Error.pp formatter error
  | Simulation_failed error ->
    Format.fprintf formatter "simulation failed: %s" (Yojson.Safe.to_string error)
  | Transaction_failed error ->
    Format.fprintf formatter "transaction failed: %s" (Yojson.Safe.to_string error)
  | Signature_mismatch { expected; actual } ->
    Format.fprintf formatter "RPC returned signature %s, expected local ID %s"
      (Solana_types.Signature.to_base58 actual)
      (Solana_types.Signature.to_base58 expected)
  | Signed_with_wrong_blockhash { expected; actual } ->
    Format.fprintf formatter "signed blockhash %s, expected %s"
      (Solana_types.Hash.to_base58 actual) (Solana_types.Hash.to_base58 expected)
  | Attempts_exhausted attempts ->
    Format.fprintf formatter "stale blockhash after %d attempts" attempts
  | Confirmation_timeout polls ->
    Format.fprintf formatter "confirmation timed out after %d polls" polls

type action =
  | Fetch_latest_blockhash
  | Sign of Methods.latest_blockhash
  | Simulate of Solana_transaction.Transaction.signed
  | Submit of Solana_transaction.Transaction.signed
  | Check_signature of Solana_types.Signature.t
  | Check_block_height
  | Wait
  | Finished of outcome

type event =
  | Latest_blockhash of Methods.latest_blockhash Methods.contextual
  | Signed of Solana_transaction.Transaction.signed
  | Simulation of Methods.simulation Methods.contextual
  | Submitted of Solana_types.Signature.t
  | Signature_statuses of Methods.signature_status option list Methods.contextual
  | Block_height of Solana_types.U64.t
  | Waited
  | Rpc_error of Error.t
  | Deadline_reached

type phase =
  | Need_blockhash
  | Need_signature of Methods.latest_blockhash
  | Need_simulation of Methods.latest_blockhash * Solana_transaction.Transaction.signed
  | Need_submission of Methods.latest_blockhash * Solana_transaction.Transaction.signed
  | Need_status of Methods.latest_blockhash * Solana_types.Signature.t
  | Need_height of
      Methods.latest_blockhash * Solana_types.Signature.t * Methods.signature_status option
  | Need_wait of Methods.latest_blockhash * Solana_types.Signature.t
  | Done of outcome

type t = {
  config : config;
  attempts_started : int;
  confirmation_polls : int;
  phase : phase;
}

let start config =
  { config; attempts_started = 1; confirmation_polls = 0; phase = Need_blockhash }

let attempts_started state = state.attempts_started
let confirmation_polls state = state.confirmation_polls

let action state =
  match state.phase with
  | Need_blockhash -> Fetch_latest_blockhash
  | Need_signature latest -> Sign latest
  | Need_simulation (_, transaction) -> Simulate transaction
  | Need_submission (_, transaction) -> Submit transaction
  | Need_status (_, signature) -> Check_signature signature
  | Need_height _ -> Check_block_height
  | Need_wait _ -> Wait
  | Done outcome -> Finished outcome

let contains ~needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec loop index =
    if index + needle_length > haystack_length then false
    else if String.sub haystack index needle_length = needle then true
    else loop (index + 1)
  in
  needle_length = 0 || loop 0

let stale_string value =
  let value = String.lowercase_ascii value in
  List.exists
    (fun needle -> contains ~needle value)
    [ "blockhash not found"; "blockhashnotfound"; "block height exceeded";
      "transaction expired"; "blockhash expired" ]

let rec stale_json = function
  | `String value -> stale_string value
  | `Assoc fields ->
    List.exists (fun (key, value) -> stale_string key || stale_json value) fields
  | `List values | `Tuple values -> List.exists stale_json values
  | `Variant (name, value) ->
    stale_string name || Option.fold ~none:false ~some:stale_json value
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ -> false

let is_stale_blockhash_error = function
  | Error.Rpc { message; data; _ } ->
    stale_string message || Option.fold ~none:false ~some:stale_json data
  | Error.Transport message | Error.Malformed_json message
  | Error.Invalid_response message -> stale_string message
  | Error.Decode { message; value; _ } -> stale_string message || stale_json value

let finish state failure = { state with phase = Done (Failed failure) }

let retry state =
  if state.attempts_started >= state.config.max_attempts then
    finish state (Attempts_exhausted state.config.max_attempts)
  else
    { state with
      attempts_started = state.attempts_started + 1;
      confirmation_polls = 0;
      phase = Need_blockhash }

let invalid state event =
  let action_name =
    match action state with
    | Fetch_latest_blockhash -> "Fetch_latest_blockhash"
    | Sign _ -> "Sign"
    | Simulate _ -> "Simulate"
    | Submit _ -> "Submit"
    | Check_signature _ -> "Check_signature"
    | Check_block_height -> "Check_block_height"
    | Wait -> "Wait"
    | Finished _ -> "Finished"
  in
  let event_name =
    match event with
    | Latest_blockhash _ -> "Latest_blockhash"
    | Signed _ -> "Signed"
    | Simulation _ -> "Simulation"
    | Submitted _ -> "Submitted"
    | Signature_statuses _ -> "Signature_statuses"
    | Block_height _ -> "Block_height"
    | Waited -> "Waited"
    | Rpc_error _ -> "Rpc_error"
    | Deadline_reached -> "Deadline_reached"
  in
  Error
    (Printf.sprintf "submission: event %s is invalid while awaiting %s"
       event_name action_name)

let advance state event =
  match state.phase, event with
  | Done _, _ -> invalid state event
  | _, Deadline_reached ->
    Ok (finish state (Confirmation_timeout state.confirmation_polls))
  | (Need_simulation _ | Need_submission _), Rpc_error error
    when is_stale_blockhash_error error ->
    Ok (retry state)
  | _, Rpc_error error -> Ok (finish state (Rpc error))
  | Need_blockhash, Latest_blockhash latest ->
    Ok { state with phase = Need_signature latest.value }
  | Need_signature latest, Signed transaction ->
    let actual =
      transaction |> Solana_transaction.Transaction.signed_message
      |> Solana_transaction.Message.recent_blockhash
    in
    if not (Solana_types.Hash.equal latest.blockhash actual) then
      Ok
        (finish state
           (Signed_with_wrong_blockhash { expected = latest.blockhash; actual }))
    else Ok { state with phase = Need_simulation (latest, transaction) }
  | Need_simulation (latest, transaction), Simulation simulation ->
    (match simulation.value.err with
    | None -> Ok { state with phase = Need_submission (latest, transaction) }
    | Some error when stale_json error -> Ok (retry state)
    | Some error -> Ok (finish state (Simulation_failed error)))
  | Need_submission (latest, transaction), Submitted actual ->
    let expected = Solana_transaction.Transaction.id transaction in
    if not (Solana_types.Signature.equal expected actual) then
      Ok (finish state (Signature_mismatch { expected; actual }))
    else Ok { state with phase = Need_status (latest, actual) }
  | Need_status (latest, signature), Signature_statuses statuses ->
    (match statuses.value with
    | [ Some { Methods.err = Some error; _ } ] ->
      Ok (finish state (Transaction_failed error))
    | [ Some { confirmation_status = Some status; _ } ]
      when Confirmation.rank status
           >= Confirmation.required_rank state.config.commitment ->
      Ok { state with phase = Done (Confirmed signature) }
    | [ status ] ->
      Ok { state with phase = Need_height (latest, signature, status) }
    | _ ->
      Error "submission: getSignatureStatuses returned a list of unexpected length")
  | Need_height (latest, signature, status), Block_height current_block_height ->
    let polls = state.confirmation_polls + 1 in
    (match
       Confirmation.decide ~commitment:state.config.commitment
         ~current_block_height
         ~last_valid_block_height:latest.last_valid_block_height status
     with
    | Confirmation.Succeeded ->
      Ok { state with phase = Done (Confirmed signature) }
    | Failed error -> Ok (finish state (Transaction_failed error))
    | Expired -> Ok (retry { state with confirmation_polls = polls })
    | Pending when polls >= state.config.max_confirmation_polls ->
      Ok
        (finish { state with confirmation_polls = polls }
           (Confirmation_timeout polls))
    | Pending ->
      Ok
        { state with
          confirmation_polls = polls;
          phase = Need_wait (latest, signature) })
  | Need_wait (latest, signature), Waited ->
    Ok { state with phase = Need_status (latest, signature) }
  | _ -> invalid state event
