type rpc = { code : int; message : string; data : Yojson.Safe.t option }

type t =
  | Transport of string
  | Malformed_json of string
  | Invalid_response of string
  | Rpc of rpc
  | Decode of { method_ : string; message : string; value : Yojson.Safe.t }

val pp : Format.formatter -> t -> unit
