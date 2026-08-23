type local_id = int
type account_encoding = Base64 | Json_parsed

type subscription =
  | Signature of {
      signature : Solana_types.Signature.t;
      commitment : Solana_types.commitment;
      enable_received_notification : bool;
    }
  | Account of {
      address : Solana_types.Address.t;
      commitment : Solana_types.commitment;
      encoding : account_encoding;
    }

type command = { text : string }

type output =
  | Subscribed of { local_id : local_id; server_id : int }
  | Unsubscribed of local_id
  | Notification of { local_id : local_id; value : Yojson.Safe.t }
  | Server_error of { request_id : int; error : Yojson.Safe.t }

type pending_kind = Subscribe of local_id * subscription | Unsubscribe of local_id

type t = {
  connected : bool;
  next_local_id : int;
  next_request_id : int;
  desired : (local_id * subscription) list;
  pending : (int * pending_kind) list;
  active : (int * local_id * subscription) list;
}

let create () =
  { connected = false; next_local_id = 0; next_request_id = 0;
    desired = []; pending = []; active = [] }

let is_connected state = state.connected
let desired_count state = List.length state.desired
let active_count state = List.length state.active
let commitment value = `String (Solana_types.commitment_to_string value)
let account_encoding = function Base64 -> "base64" | Json_parsed -> "jsonParsed"

let request_json request_id method_ params =
  `Assoc
    [ "jsonrpc", `String "2.0"; "id", `Int request_id;
      "method", `String method_; "params", `List params ]
  |> Yojson.Safe.to_string

let subscribe_request request_id = function
  | Signature { signature; commitment = requested_commitment;
                enable_received_notification } ->
    request_json request_id "signatureSubscribe"
      [ `String (Solana_types.Signature.to_base58 signature);
        `Assoc
          [ "commitment", commitment requested_commitment;
            "enableReceivedNotification", `Bool enable_received_notification ] ]
  | Account { address; commitment = requested_commitment; encoding } ->
    request_json request_id "accountSubscribe"
      [ `String (Solana_types.Address.to_base58 address);
        `Assoc
          [ "commitment", commitment requested_commitment;
            "encoding", `String (account_encoding encoding) ] ]

let unsubscribe_method = function
  | Signature _ -> "signatureUnsubscribe"
  | Account _ -> "accountUnsubscribe"

let issue state pending_kind text =
  let request_id = state.next_request_id in
  let command = { text = text request_id } in
  ({ state with next_request_id = request_id + 1;
                pending = (request_id, pending_kind) :: state.pending },
   command)

let issue_subscribe state local_id subscription =
  issue state (Subscribe (local_id, subscription)) (fun request_id ->
      subscribe_request request_id subscription)

let issue_unsubscribe state local_id subscription server_id =
  issue state (Unsubscribe local_id) (fun request_id ->
      request_json request_id (unsubscribe_method subscription) [ `Int server_id ])

let add state subscription =
  let local_id = state.next_local_id in
  let state =
    { state with next_local_id = local_id + 1;
                 desired = state.desired @ [ local_id, subscription ] }
  in
  if state.connected then
    let state, command = issue_subscribe state local_id subscription in
    state, local_id, [ command ]
  else state, local_id, []

let remove_assoc key values =
  List.filter (fun (candidate, _) -> candidate <> key) values

let remove state local_id =
  let desired = remove_assoc local_id state.desired in
  let active, removed =
    List.fold_right
      (fun ((server_id, candidate, subscription) as item) (kept, removed) ->
        if candidate = local_id then kept, (server_id, subscription) :: removed
        else item :: kept, removed)
      state.active ([], [])
  in
  let state = { state with desired; active } in
  if not state.connected then state, []
  else
    List.fold_left
      (fun (state, commands) (server_id, subscription) ->
        let state, command =
          issue_unsubscribe state local_id subscription server_id
        in
        state, commands @ [ command ])
      (state, []) removed

let connected state =
  let state = { state with connected = true; pending = []; active = [] } in
  List.fold_left
    (fun (state, commands) (local_id, subscription) ->
      let state, command = issue_subscribe state local_id subscription in
      state, commands @ [ command ])
    (state, []) state.desired

let disconnected state =
  { state with connected = false; pending = []; active = [] }

let int = function
  | `Int value -> Ok value
  | `Intlit value ->
    (try Ok (int_of_string value) with Failure _ -> Error "integer is out of range")
  | _ -> Error "expected integer"

let assoc name fields =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error (name ^ " is missing")

let find_pending request_id pending = List.assoc_opt request_id pending
let remove_pending request_id pending =
  List.filter (fun (candidate, _) -> candidate <> request_id) pending

let desired local_id state = List.assoc_opt local_id state.desired

let handle_response state fields =
  match assoc "id" fields with
  | Error _ as error -> error
  | Ok id_json ->
    (match int id_json with
    | Error _ as error -> error
    | Ok request_id ->
      match find_pending request_id state.pending with
      | None -> Ok (state, [], [])
      | Some pending_kind ->
        let state =
          { state with pending = remove_pending request_id state.pending }
        in
        (match List.assoc_opt "error" fields with
        | Some error -> Ok (state, [ Server_error { request_id; error } ], [])
        | None ->
          match assoc "result" fields, pending_kind with
          | Error _ as error, _ -> error
          | Ok result, Subscribe (local_id, subscription) ->
            (match int result with
            | Error _ as error -> error
            | Ok server_id ->
              (match desired local_id state with
              | Some _ ->
                let state =
                  { state with
                    active = (server_id, local_id, subscription) :: state.active }
                in
                Ok (state, [ Subscribed { local_id; server_id } ], [])
              | None ->
                let state, command =
                  issue_unsubscribe state local_id subscription server_id
                in
                Ok (state, [], [ command ])))
          | Ok (`Bool true), Unsubscribe local_id ->
            Ok (state, [ Unsubscribed local_id ], [])
          | Ok _, Unsubscribe _ -> Error "unsubscribe result was not true"))

let find_active server_id active =
  List.find_opt (fun (candidate, _, _) -> candidate = server_id) active

let expected_method = function
  | Signature _ -> "signatureNotification"
  | Account _ -> "accountNotification"

let final_signature_notification = function
  | `String "receivedSignature" -> false
  | _ -> true

let handle_notification state method_ fields =
  match assoc "params" fields with
  | Error _ as error -> error
  | Ok (`Assoc params) ->
    (match assoc "subscription" params, assoc "result" params with
    | Ok server_json, Ok value ->
      (match int server_json with
      | Error _ as error -> error
      | Ok server_id ->
        (match find_active server_id state.active with
        | None -> Ok (state, [], [])
        | Some (_, local_id, subscription) ->
          if method_ <> expected_method subscription then
            Error "notification method does not match subscription kind"
          else
            let state =
              match subscription with
              | Signature _ when final_signature_notification value ->
                { state with
                  desired = remove_assoc local_id state.desired;
                  active =
                    List.filter
                      (fun (candidate, _, _) -> candidate <> server_id)
                      state.active }
              | Signature _ | Account _ -> state
            in
            Ok (state, [ Notification { local_id; value } ], [])))
    | Error message, _ | _, Error message -> Error message)
  | Ok _ -> Error "notification params must be an object"

let receive state text =
  try
    match Yojson.Safe.from_string text with
    | `Assoc fields ->
      (match List.assoc_opt "jsonrpc" fields with
      | Some (`String "2.0") ->
        (match List.assoc_opt "method" fields with
        | Some (`String method_) -> handle_notification state method_ fields
        | Some _ -> Error "notification method must be a string"
        | None -> handle_response state fields)
      | _ -> Error "expected jsonrpc=2.0")
    | _ -> Error "WebSocket message must be a JSON object"
  with Yojson.Json_error message ->
    Error ("malformed WebSocket JSON: " ^ message)
