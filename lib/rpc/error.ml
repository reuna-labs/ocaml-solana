type rpc = { code : int; message : string; data : Yojson.Safe.t option }

type t =
  | Transport of string
  | Malformed_json of string
  | Invalid_response of string
  | Rpc of rpc
  | Decode of { method_ : string; message : string; value : Yojson.Safe.t }

let pp formatter = function
  | Transport message -> Format.fprintf formatter "transport error: %s" message
  | Malformed_json message -> Format.fprintf formatter "malformed JSON: %s" message
  | Invalid_response message -> Format.fprintf formatter "invalid JSON-RPC response: %s" message
  | Rpc error -> Format.fprintf formatter "JSON-RPC error %d: %s" error.code error.message
  | Decode { method_; message; _ } ->
    Format.fprintf formatter "%s result decode error: %s" method_ message
