module type S = sig
  type t
  type 'a io
  val return : 'a -> 'a io
  val bind : 'a io -> ('a -> 'b io) -> 'b io
  val request : t -> method_:string -> params:Yojson.Safe.t list ->
    (Yojson.Safe.t, Error.t) result io
end

module Make (P : S) = struct
  let call provider method_ =
    P.bind
      (P.request provider ~method_:(Method.name method_) ~params:(Method.params method_))
      (function
        | Error _ as error -> P.return error
        | Ok value ->
          (match Method.decode method_ value with
          | Ok decoded -> P.return (Ok decoded)
          | Error message ->
            P.return (Error (Error.Decode { method_ = Method.name method_; message; value }))))
end

module type HTTP = sig
  type t
  type 'a io
  val return : 'a -> 'a io
  val bind : 'a io -> ('a -> 'b io) -> 'b io
  val post : t -> body:string -> (string, string) result io
end

module Of_http (H : HTTP) = struct
  type t = H.t
  type 'a io = 'a H.io
  let return = H.return
  let bind = H.bind

  let decode_rpc_error = function
    | `Assoc fields ->
      (match List.assoc_opt "code" fields, List.assoc_opt "message" fields with
      | Some (`Int code), Some (`String message) ->
        Ok { Error.code; message; data = List.assoc_opt "data" fields }
      | _ -> Error "JSON-RPC error requires integer code and string message")
    | _ -> Error "JSON-RPC error must be an object"

  let decode body =
    try
      match Yojson.Safe.from_string body with
      | `Assoc fields ->
        (match List.assoc_opt "jsonrpc" fields, List.assoc_opt "id" fields with
        | Some (`String "2.0"), Some (`Int 0) ->
          (match List.assoc_opt "result" fields, List.assoc_opt "error" fields with
          | Some result, None -> Ok result
          | None, Some value ->
            (match decode_rpc_error value with
            | Ok error -> Error (Error.Rpc error)
            | Error message -> Error (Error.Invalid_response message))
          | Some _, Some _ -> Error (Error.Invalid_response "response has result and error")
          | None, None -> Error (Error.Invalid_response "response has neither result nor error"))
        | _ -> Error (Error.Invalid_response "expected jsonrpc=2.0 and id=0"))
      | _ -> Error (Error.Invalid_response "response must be a JSON object")
    with Yojson.Json_error message -> Error (Error.Malformed_json message)

  let request transport ~method_ ~params =
    let request =
      `Assoc
        [ "jsonrpc", `String "2.0"; "id", `Int 0; "method", `String method_;
          "params", `List params ]
      |> Yojson.Safe.to_string
    in
    H.bind (H.post transport ~body:request) (function
      | Error message -> H.return (Error (Error.Transport message))
      | Ok body -> H.return (decode body))
end
